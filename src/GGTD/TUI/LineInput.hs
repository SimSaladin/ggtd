------------------------------------------------------------------------------
-- |
-- Module         : GGTD.TUI.LineInput
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.TUI.LineInput where

import           GGTD.TUI.Base
import qualified GGTD.DB.Update as U
import qualified GGTD.DB.Query as Q

import           Brick.Widgets.Edit (Editor)
import qualified Brick.Widgets.Edit as Edit

data LineInput model = LineInput
                     { _editor :: Editor String Text
                     , _submit :: [String] -> Handler ()
                     , _submitted :: Bool
                     , _destroyHandler :: Handler ()
                     }
makeLenses ''LineInput

handleLineInputEvent :: BrickEvent n e -> LineInput a -> EventM Text (LineInput a)
handleLineInputEvent brickEv li = case brickEv of
  VtyEvent (EvKey KEnter []) -> submitInput li
  VtyEvent (EvKey KEsc   []) -> liftIO (runHandler (_destroyHandler li)) >> return (submitted .~ True $ li)
  VtyEvent ev                -> handleEventLensed li editor Edit.handleEditorEvent ev
  _ -> return li

-- * Submit and render

-- | Render the editor widget
renderLineInput :: LineInput model -> Widget Text
renderLineInput li = if li^.submitted
                       then emptyWidget
                       else Edit.renderEditor True (li^.editor)

-- | Submit the editor using associated submit action. Sets submitted to
-- True and disables the submit action, so it cannot be used again.
submitInput :: LineInput model -> EventM n (LineInput model)
submitInput li = do
    liftIO . runHandler $ li ^. submit $ Edit.getEditContents $ li ^. editor
    liftIO . runHandler $ _destroyHandler li
    return $ submit .~ const (return ())
           $ submitted .~ True $ li

-- | Add an action to execute when the input is deactivated
addDestroyHandler :: LineInput model -> Handler () -> LineInput model
addDestroyHandler li h = destroyHandler .~ (_destroyHandler li >> h) $ li

-- * LineInputs

-- Null editor, used as the default base
singleLine :: ([String] -> Handler ()) -> LineInput model
singleLine doSubmit = LineInput
    (Edit.editor "singleline" (str . unlines) (Just 1) "")
    doSubmit
    False
    (return ())

-- | Ask a new node under given parent.
nodeAdd
    :: Relation
    -> Node -- ^ Parent node
    -> LineInput model
nodeAdd rel parent = singleLine $ \contents' -> do
    let contents = head contents'
    thingy <- U.createThingy contents
    node   <- U.addThingyGr thingy
    U.addRelGr (parent, node, rel)
    emit RefreshTreeNav

nodeChooser
    :: (Node -> Handler ())
    -> LineInput model
nodeChooser f = singleLine $ \[line] -> do
    res <- Q.findNodesHeuristic line
    case res of
        [(node,_)] -> f node
        []         -> emit $ UIError "no nodes found with given query"
        xs         -> emit $ UIError $ "Multiple nodes found: " <> pack (show xs)

-- | place holder
inactive :: LineInput model
inactive = (singleLine (\_ -> return ())) { _submitted = True }
