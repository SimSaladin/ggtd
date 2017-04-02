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

-- import qualified GGTD.DB.Query as Q
import qualified GGTD.DB.Update as U

import           GGTD.TUI.Base
import           GGTD.TUI.TaskList     (TaskList)
import qualified GGTD.TUI.TaskList  as TaskList
import           GGTD.TUI.TreeNav      (TreeNav)
import qualified GGTD.TUI.TreeNav   as TreeNav
import           GGTD.TUI.LineInput    (LineInput)
import qualified GGTD.TUI.LineInput as LineInput

data Model = Model
    { _curTaskList   :: TaskList
    , _urgentMatter  :: Maybe [String] -- notifications
    , _treeNav       :: TreeNav 
    , _curFocus      :: AppFocus
    , _lineInput     :: LineInput Model
    , _statusLine    :: Text
    }

-- makeLenses
makeLenses ''Model

run :: Handler a -> EventM Text a
run = liftIO . runHandler

-- | brick/vty-based UI for ggtd.
terminalUI :: IO ()
terminalUI = do
    model <- runHandler initialModel
    _ <- customMain (mkVty defaultConfig) (Just evchan) app model
    return ()

app :: App Model MyEvent Text
app = App
    { appDraw         = draw
    , appChooseCursor = \_ xs -> case xs of
                                    (x:_) -> Just x
                                    _ -> Nothing
    , appHandleEvent  = flip updateModel
    , appStartEvent   = return
    , appAttrMap      = const $ attrMap defAttr []
    }

-- KEYS

myEventMap :: AppFocus -> BrickEvent n MyEvent -> Action
myEventMap FocusTaskList  = \ev model -> handleKeyTaskList (model^.curTaskList) ev model
myEventMap FocusTreeNav   = handleKeyTreeNav
myEventMap FocusLineInput = handleKeyLineInput

handleKeyTaskList :: TaskList -> BrickEvent n MyEvent -> Action
handleKeyTaskList taskList (VtyEvent (EvKey key mods))
    | Just _ <- TaskList.editor taskList = handleEditor
    | otherwise                          = handleNormal
    where
    handleNormal :: Action
    handleNormal = case (key, mods) of
        (KChar 'd', []     ) -> withTaskList $ TaskList.withFocused (\n -> U.alterFlagGr n Done (Just "done"))
        (KChar 'd', [MCtrl]) -> withTaskList TaskList.deleteFocused
        (KChar 'e', []     ) -> withTaskList $ TaskList.editFocused
        (KChar 'h', []     ) -> continue . (curFocus .~ FocusTreeNav)
        (KChar 'j', []     ) -> withTaskList $ return . TaskList.down
        (KChar 'k', []     ) -> withTaskList $ return . TaskList.up
        (KChar 'n', []     ) -> withTaskList $ TaskList.openNewNodeEditor
        _                    -> handleKeyGeneric key mods

    handleEditor :: Action
    handleEditor = case (key, mods) of
        (KEsc  , []) -> withTaskList TaskList.exitEditor
        (KEnter, []) -> withTreeNavFocusedNode $ \parent ->
                withTaskList (TaskList.submitEditor parent)
        _ -> \model -> do
            tlist <- TaskList.handleBasicEditorKey key mods (model ^. curTaskList)
            continue $ curTaskList .~ tlist $ model
handleKeyTaskList _taskList _ = continue

handleKeyTreeNav :: BrickEvent n MyEvent -> Action
handleKeyTreeNav ev = case ev of
    VtyEvent (EvKey (KChar 'k') []     ) -> focusUpTreeNav
    VtyEvent (EvKey (KChar 'j') []     ) -> focusDownTreeNav
    VtyEvent (EvKey (KChar 'l') []     ) -> continue . (curFocus .~ FocusTaskList)
    VtyEvent (EvKey KEnter      []     ) -> continue . (treeNav %~ TreeNav.toggleExpand)
    VtyEvent (EvKey (KChar 'n') []     ) -> groupAddTreeNav
    VtyEvent (EvKey (KChar 'd') [MCtrl]) -> withTreeNav TreeNav.deleteFocused
    VtyEvent (EvKey key       mods     ) -> handleKeyGeneric key mods
    _ -> continue

handleKeyLineInput :: BrickEvent n MyEvent -> Action
handleKeyLineInput ev model' = handleEventLensed model' lineInput LineInput.handleLineInputEvent ev >>= continue

-- | Core keys, available almost everywhere except an edit mode.
handleKeyGeneric :: Key -> [Modifier] -> Action
handleKeyGeneric key mods = case (key, mods) of
    (KChar 'c', [MCtrl]) -> halt
    (KChar 'q', [])      -> halt
    (KChar 'c', [])      -> copyFocusedNode
    (KChar 'm', [])      -> moveFocusedNode
    _                    -> continue

-- MODEL

initialModel :: Handler Model
initialModel = Model
    <$> TaskList.new 0
    <*> pure Nothing
    <*> TreeNav.new 0
    <*> pure FocusTreeNav
    <*> pure LineInput.inactive
    <*> pure "ggtd"

-- VIEW

draw :: Model -> [Widget Text]
draw Model{..} = return $
    (tnav <+> tlist)
    <=>
    sline
  where
    tnav  = raw (pad 0 0 1 0 ( TreeNav.toImage _treeNav <-> viewUrgentMatter _urgentMatter ))
    tlist = TaskList.view _curTaskList <=> LineInput.renderLineInput _lineInput
    sline = padTop Max $ txt _statusLine

viewUrgentMatter :: Maybe [String] -> Image
viewUrgentMatter Nothing = mempty
viewUrgentMatter (Just xs) = vertCat [ string (defAttr `withForeColor` red) x | x <- xs ]

-- UPDATE

type Action = Model -> EventM Text (Next Model)

updateModel :: BrickEvent Text MyEvent -> Action
updateModel ev@(VtyEvent{}) = \model -> myEventMap (model^.curFocus) ev model

updateModel (AppEvent (SetStatus newstatus)) = continue . (statusLine .~ newstatus)
updateModel (AppEvent (UIError err))         = continue . (statusLine .~ err)
updateModel (AppEvent (SetFocus foc))        = continue . (curFocus .~ foc)
updateModel (AppEvent RefreshTaskList)       = refreshTaskList
updateModel (AppEvent RefreshTreeNav)        = \model -> do
    tnav <- run $ TreeNav.update 0 (model^.treeNav)
    run $ emit RefreshTaskList
    continue $ treeNav.~tnav $ model
updateModel MouseUp{} = continue . id
updateModel MouseDown{} = continue . id

-- * ACTION

-- | Copy node to under asked parent node
copyFocusedNode :: Action
copyFocusedNode =
    withFocused $ \(rel, focusedNode, _) ->
    withChooseNode $ \parent -> U.addRelGr (parent,focusedNode,rel)
                             >> emit RefreshTreeNav

-- | Copy node to under asked parent node, remove from current focused
-- parent.
moveFocusedNode :: Action
moveFocusedNode =
    withFocused $ \(rel, focusedNode, _) ->
    withFocusedParentNode $ \focusedParent ->
    withChooseNode $ \parent -> do
        U.addRelGr (parent, focusedNode, rel)
        U.delRelGr (focusedParent, focusedNode)
        emit $ SetStatus $ pack $ show (focusedParent, focusedNode)
        emit RefreshTreeNav

refreshTaskList :: Action
refreshTaskList = do
    mparent <- view $ treeNav.to TreeNav.findFocusedNode
    mfocus <- view $ curTaskList.to TaskList.askFocused
    (\model -> do
        tlist' <- run $ TaskList.new $ maybe 0 (^._2) mparent
        let tlist = case mfocus of
                        Just focus -> TaskList.focusNode (focus^._2) tlist'
                        Nothing    -> tlist'
        continue $ (curTaskList.~tlist) model
     )

-- * Generic

-- | Ask for a node with an editor and perform an action with the node.
withChooseNode :: (Node -> Handler ()) -> Action
withChooseNode f = focusNewEditor $ LineInput.nodeChooser f

withTaskList :: (TaskList -> Handler TaskList) -> Action
withTaskList go model = do
    tlist <- run $ go (model ^. curTaskList)

    run $ case TaskList.askFocused tlist of
        Nothing -> return ()
        Just x  -> emit $ SetStatus $ pack $ show x

    continue $ curTaskList .~ tlist $ model

withTreeNav :: (TreeNav -> Handler TreeNav) -> Action
withTreeNav go model = do
    nav <- run $ go (model ^. treeNav)
    continue $ treeNav .~ nav $ model

groupAddTreeNav :: Action
groupAddTreeNav = withTreeNavFocusedNode $ \parent ->
    focusNewEditor (LineInput.nodeAdd relGroup parent)

focusNewEditor :: LineInput Model -> Action
focusNewEditor li' = do
    foc <- view curFocus
    let li = LineInput.addDestroyHandler li' (emit $ SetFocus foc)
    continue . (curFocus .~ FocusLineInput) . (lineInput .~ li)

-- * The focused node

-- | The focused node is taken from the context we are focused at:
-- * TreeNav focused node
-- * TaskList focused node
-- If no focused node can be found, an UIError is emitted
withFocused :: ((Relation, Node, Thingy) -> Action) -> Action
withFocused f = view curFocus >>= \case
    FocusTaskList -> withTaskListFocused f
    FocusTreeNav -> withTreeNavFocused f
    _ -> do _ <- const $ run.emit $ UIError "Current focus doesn't specify a node"
            continue

-- ** Focused parent

-- | Resolve the focused parent node for currently focused node.
withFocusedParentNode :: (Node -> Action) -> Action
withFocusedParentNode f = view curFocus >>= \case
    FocusTaskList -> withTreeNavFocusedNode f
    FocusTreeNav  -> view (treeNav . to TreeNav.findFocusedParentNode) >>= f
    _ -> do _ <- const $ run.emit $ UIError "Current focus doesn't specify focus's parent"
            continue

-- ** per-context focus

withTreeNavFocused :: ((Relation, Node, Thingy) -> Action) -> Action
withTreeNavFocused f = do
    mthis <- view $ treeNav . to TreeNav.findFocusedNode
    case mthis of
        Just this -> f this
        Nothing   -> \m -> run (emit $ UIError "No treenav node focused") >> continue m

withTaskListFocused :: ((Relation, Node, Thingy) -> Action) -> Action
withTaskListFocused f = do
    mthis <- view $ curTaskList.to (TaskList.askFocused)
    case mthis of
        Just this -> f this
        Nothing   -> \m -> run (emit $ UIError "No tasklist node focused") >> continue m

-- ** Node only

withFocusedNode :: (Node -> Action) -> Action
withFocusedNode f = withFocused $ \(_,n,_) -> f n
withTreeNavFocusedNode :: (Node -> Action) -> Action
withTreeNavFocusedNode f = withTreeNavFocused $ \(_,n,_) -> f n
withTaskListFocusedNode :: (Node -> Action) -> Action
withTaskListFocusedNode f = withTaskListFocused $ \(_,n,_) -> f n

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
