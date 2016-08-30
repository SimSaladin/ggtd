------------------------------------------------------------------------------
-- |
-- Module         : GGTD.TUI
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.TUI where

import           Control.Monad
import           Control.Lens hiding ((&), Context, Context')
import           Control.Monad.IO.Class (liftIO)

import           Data.Default
import           Data.Graph.Inductive.Graph
import           Data.Tree as Tree

import           GGTD.Base
import           GGTD.Relation
import           GGTD.Filter
import           GGTD.DB (runHandler)
import qualified GGTD.DB.Query as Q

import           GGTD.TUI.TaskList (TaskList)
import qualified GGTD.TUI.TaskList as TaskList

import           Brick
import           Graphics.Vty

-- XXX: Move
type TreeNav = Q.TaggedView (Bool, Bool) -- ^ (expanded, focused)?

data Model = Model
    { _curTaskList   :: TaskList
    , _urgentMatter  :: Maybe [String] -- notifications
    , _treeNav       :: TreeNav 
    , _curFocus      :: AppFocus
    }

data AppFocus = FocusTreeNav
              | FocusTaskList

-- makeLenses
makeLenses ''Model

run :: Handler a -> EventM a
run = liftIO . runHandler

-- | brick/vty-based UI for ggtd.
terminalUI :: IO ()
terminalUI = void $ defaultMain app =<< runHandler initialModel

app :: App Model Event
app = App
    { appDraw         = draw
    , appChooseCursor = \_ xs -> case xs of
                                    (x:_) -> Just x
                                    _ -> Nothing
    , appHandleEvent  = updateModel
    , appStartEvent   = return
    , appAttrMap      = def
    , appLiftVtyEvent = id
    }

-- MODEL

initialModel :: Handler Model
initialModel = Model
    <$> TaskList.new 0
    <*> pure Nothing
    <*> getTreeNav 0
    <*> pure FocusTreeNav

-- VIEW

draw :: Model -> [Widget]
draw Model{..} =
    [ raw (pad 0 0 1 0 ( viewTreeNav _treeNav <-> viewUrgentMatter _urgentMatter )) <+> TaskList.view _curTaskList ]

viewTreeNav :: TreeNav -> Image
viewTreeNav (Node ((rel, ctx), (expanded, focused)) xs)
    = thisBalloon <|> thisItem
        <-> (if expanded then childItems else mempty)
  where
    thisBalloon
        | rel == relLink = string defAttr "L "
        | expanded       = string defAttr "- "
        | otherwise      = string defAttr "+ "
    thisItem
        | focused   = TaskList.viewThingyFocused (lab' ctx)
        | otherwise = TaskList.viewThingyUnfocused (lab' ctx)

    childItems
        = pad 1 0 0 0 $ vertCat $ map viewTreeNav xs

viewUrgentMatter :: Maybe [String] -> Image
viewUrgentMatter Nothing = mempty
viewUrgentMatter (Just xs) = vertCat [ string (defAttr `withForeColor` red) x | x <- xs ]

-- UPDATE

type Action = Model -> EventM (Next Model)

updateModel :: Model -> Event -> EventM (Next Model)
updateModel model EvMouse{}        = continue model
updateModel model EvResize{}       = continue model
updateModel model (EvKey key mods) = case model^.curFocus of
    FocusTaskList -> handleKeyTaskList (model^.curTaskList) key mods model
    FocusTreeNav  -> handleKeyTreeNav  key mods model

handleKeyTaskList :: TaskList -> Key -> [Modifier] -> Action
handleKeyTaskList taskList key mods
    | Just _ <- TaskList.editor taskList = handleTaskListEditorKey key mods
    | otherwise = handleKeyTaskListNormal key mods
    where

    handleKeyTaskListNormal :: Key -> [Modifier] -> Action
    handleKeyTaskListNormal (KChar 'd') [MCtrl] = withTaskList TaskList.deleteFocused
    handleKeyTaskListNormal (KChar 'k') [] = withTaskList $ return . TaskList.up
    handleKeyTaskListNormal (KChar 'j') [] = withTaskList $ return . TaskList.down
    handleKeyTaskListNormal (KChar 'n') [] = withTaskList $ TaskList.openNewNodeEditor
    handleKeyTaskListNormal (KChar 'e') [] = withTaskList $ TaskList.editFocused
    handleKeyTaskListNormal (KChar 'h') [] = continue . (curFocus .~ FocusTreeNav)
    handleKeyTaskListNormal _ _ = handleKeyGeneric key mods

    handleTaskListEditorKey :: Key -> [Modifier] -> Action
    handleTaskListEditorKey KEsc   [] = withTaskList TaskList.exitEditor
    handleTaskListEditorKey KEnter [] = \model -> do
        let parent = findFocusedNode (_treeNav model)
        withTaskList (TaskList.submitEditor parent) model

    handleTaskListEditorKey _ _ = \model -> do
        tlist <- TaskList.handleBasicEditorKey key mods (model ^. curTaskList)
        continue $ curTaskList .~ tlist $ model

-- | Core keys, available almost everywhere except an edit mode.
handleKeyGeneric :: Key -> [Modifier] -> Action
handleKeyGeneric (KChar 'c') [MCtrl] = halt
handleKeyGeneric (KChar 'q') []      = halt
handleKeyGeneric _           _       = continue

withTaskList :: (TaskList -> Handler TaskList) -> Action
withTaskList go model = do
    tlist <- run $ go (model ^. curTaskList)
    continue $ curTaskList .~ tlist $ model

handleKeyTreeNav :: Key -> [Modifier] -> Action
handleKeyTreeNav (KChar 'k') [] = focusUpTreeNav
handleKeyTreeNav (KChar 'j') [] = focusDownTreeNav
handleKeyTreeNav (KChar 'l') [] = continue . (curFocus .~ FocusTaskList)
handleKeyTreeNav KEnter      [] = continue . (treeNav %~ toggleExpand)
handleKeyTreeNav key       mods = handleKeyGeneric key mods

focusUpTreeNav, focusDownTreeNav :: Action
focusUpTreeNav m = do
    let (n, tnav) = treeNavUp (_treeNav m)
    tlist <- run $ TaskList.new n
    continue $ (curTaskList .~ tlist) $ (treeNav .~ tnav) m
focusDownTreeNav m = do
    let (n, tnav) = treeNavDown (_treeNav m)
    tlist <- run $ TaskList.new n
    continue $ (curTaskList .~ tlist) $ (treeNav .~ tnav) m

-- TreeNav

-- | * ignores child nodes
getTreeNav :: Node -> Handler TreeNav
getTreeNav node =
        -- TODO: partial: head
    Q.tagging (True, True) tag . head . fst <$> Q.getViewAtGr fltr [] node
  where
  tag  = const (False, False)
  fltr = [ FRel relGroup ]

treeNavUp, treeNavDown :: TreeNav -> (Node, TreeNav)

treeNavUp root = (findFocusedNode root', root')
  where
    (_, root') = go root

    go :: TreeNav -> (Bool, TreeNav) -- (overflow to parent?, new tree)
    go this@(Node (x, (expanded, focus)) xs)
        | focus            = (True, Node (x, (expanded, focus)) xs) -- root is focus
        | not expanded     = (False, this)                          -- no focus, no childs expanded; leaf no-op
        | otherwise                                                 -- child maybe focused
            = let (overflows, xs') = unzip $ map go xs
                  in case findIndexOf traversed id overflows :: Maybe Int of
                      Nothing -> (False, Node (x, (expanded, focus)) xs')  -- no overflow
                      Just 0  -> (False, Node (x, (expanded, True)) $ ix 0 %~ defocus $ xs') -- focus moves to parent (this)
                      Just i  -> (False, Node (x, (expanded, focus)) $ ix (i-1) %~ setfocuslast $ ix i %~ defocus $ xs') -- focus moves up by one

treeNavDown root = if newfocus < 0 then (findFocusedNode root, root)
                                   else (newfocus, root')
  where
    (_, root') = go root
    newfocus   = findFocusedNode root'

    go :: TreeNav -> (Bool, TreeNav) -- (overflow to parent?, new tree)
    go this@(Node (x, (expanded, focus)) xs)
        | focus, expanded, x' : xs' <- xs = (False, Node (x, (expanded, False)) (setfocus x' : xs')) -- root is fcous and can move to child
        | focus = (True, defocus this) -- root is focus but can't move to child
        | otherwise -- child maybe focused
            = let (overflows, xs') = unzip $ map go xs
                  in case findIndexOf traversed id overflows :: Maybe Int of
                      Nothing -> (False, Node (x, (expanded, focus)) xs')  -- no overflow
                      Just i
                        | i+1 < length xs' -> (False, Node (x, (expanded, focus)) $ ix (i+1) %~ setfocus $ ix i %~ defocus $ xs') -- focus moves up by one
                        | otherwise -> (True, Node (x, (expanded, focus)) xs') -- focus moves down to next group

findFocusedNode :: TreeNav -> Node
findFocusedNode = foldl (\n ((_,ctx), (_, focus)) -> if focus then node' ctx else n) (-1)

isFocused :: TreeNav -> Bool
isFocused (Node (_, (_,f)) _) = f

defocus :: TreeNav -> TreeNav
defocus (Node (x, (e,_)) xs) = Node (x, (e, False)) xs

setfocus :: TreeNav -> TreeNav
setfocus (Node (x, (e,_)) xs) = Node (x, (e, True)) xs

setfocuslast :: TreeNav -> TreeNav
setfocuslast (Node (x, (e,_)) []) = Node (x, (e, True)) []
setfocuslast (Node (x, (e,_)) ys)
    | e = Node (x, (e, False)) (_last %~ setfocuslast $ ys)
    | otherwise = Node (x, (e, True)) ys

toggleExpand :: TreeNav -> TreeNav
toggleExpand = fmap go where
    go r@(x, (e, f)) | f         = (x, (not e, f))
                     | otherwise = r
