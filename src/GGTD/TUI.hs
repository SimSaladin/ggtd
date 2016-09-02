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

import           GGTD.Base
import           GGTD.Relation
import           GGTD.DB (runHandler)
import           GGTD.TUI.TaskList     (TaskList)
import qualified GGTD.TUI.TaskList  as TaskList
import           GGTD.TUI.TreeNav      (TreeNav)
import qualified GGTD.TUI.TreeNav   as TreeNav
import           GGTD.TUI.LineInput    (LineInput)
import qualified GGTD.TUI.LineInput as LineInput

import           Brick
import           Graphics.Vty

data Model = Model
    { _curTaskList   :: TaskList
    , _urgentMatter  :: Maybe [String] -- notifications
    , _treeNav       :: TreeNav 
    , _curFocus      :: AppFocus
    , _lineInput     :: LineInput Model
    }

data AppFocus = FocusTreeNav
              | FocusTaskList
              | FocusLineInput

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
    <*> TreeNav.new 0
    <*> pure FocusTreeNav
    <*> pure LineInput.inactive

-- VIEW

draw :: Model -> [Widget]
draw Model{..} =
    [ raw (pad 0 0 1 0 (
        TreeNav.toImage _treeNav <-> viewUrgentMatter _urgentMatter ))
      <+> TaskList.view _curTaskList <=> LineInput.renderLineInput _lineInput ]

viewUrgentMatter :: Maybe [String] -> Image
viewUrgentMatter Nothing = mempty
viewUrgentMatter (Just xs) = vertCat [ string (defAttr `withForeColor` red) x | x <- xs ]

-- UPDATE

type Action = Model -> EventM (Next Model)

updateModel :: Model -> Event -> EventM (Next Model)
updateModel model EvMouse{}           = continue model
updateModel model EvResize{}          = continue model
updateModel model ev@(EvKey key mods) = case model^.curFocus of
    FocusTaskList  -> handleKeyTaskList (model^.curTaskList) key mods model
    FocusTreeNav   -> handleKeyTreeNav  key mods model
    FocusLineInput -> handleKeyLineInput ev model

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
        let parent = TreeNav.findFocusedNode (_treeNav model)
        withTaskList (TaskList.submitEditor parent) model

    handleTaskListEditorKey _ _ = \model -> do
        tlist <- TaskList.handleBasicEditorKey key mods (model ^. curTaskList)
        continue $ curTaskList .~ tlist $ model

handleKeyTreeNav :: Key -> [Modifier] -> Action
handleKeyTreeNav (KChar 'k') [] = focusUpTreeNav
handleKeyTreeNav (KChar 'j') [] = focusDownTreeNav
handleKeyTreeNav (KChar 'l') [] = continue . (curFocus .~ FocusTaskList)
handleKeyTreeNav KEnter      [] = continue . (treeNav %~ TreeNav.toggleExpand)
handleKeyTreeNav (KChar 'n') [] = groupAddTreeNav
handleKeyTreeNav key       mods = handleKeyGeneric key mods

handleKeyLineInput :: Event -> Action
handleKeyLineInput ev model' = do
    model <- handleEventLensed model' lineInput ev
    if model ^. lineInput . LineInput.submitted
        then refreshTreeNav $ curFocus .~ FocusTreeNav $ model
        else continue model

-- | Core keys, available almost everywhere except an edit mode.
handleKeyGeneric :: Key -> [Modifier] -> Action
handleKeyGeneric (KChar 'c') [MCtrl] = halt
handleKeyGeneric (KChar 'q') []      = halt
handleKeyGeneric _           _       = continue

-- * Generic

withTaskList :: (TaskList -> Handler TaskList) -> Action
withTaskList go model = do
    tlist <- run $ go (model ^. curTaskList)
    continue $ curTaskList .~ tlist $ model

withTreeNav :: (TreeNav -> Handler TreeNav) -> Action
withTreeNav go model = do
    nav <- run $ go (model ^. treeNav)
    continue $ treeNav .~ nav $ model

refreshTreeNav :: Action
refreshTreeNav model = do
    nav <- run $ TreeNav.new 0
    continue $ treeNav .~ nav $ model

groupAddTreeNav :: Action
groupAddTreeNav model = do
    let parent = model^.treeNav.to TreeNav.findFocusedNode
    continue $ (curFocus .~ FocusLineInput) . (lineInput .~ LineInput.nodeAdd relGroup parent) $ model

-- * TaskList

focusUpTreeNav, focusDownTreeNav :: Action
focusUpTreeNav m = do
    let (n, tnav) = TreeNav.up (_treeNav m)
    tlist <- run $ TaskList.new n
    continue $ (curTaskList .~ tlist) $ (treeNav .~ tnav) m
focusDownTreeNav m = do
    let (n, tnav) = TreeNav.down (_treeNav m)
    tlist <- run $ TaskList.new n
    continue $ (curTaskList .~ tlist) $ (treeNav .~ tnav) m
