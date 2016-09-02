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

import           Brick.Widgets.Edit (Editor)
import qualified Brick.Widgets.Edit as Edit

data LineInput model = LineInput
                     { _editor :: Editor
                     , _submit :: [String] -> Handler ()
                     , _submitted :: Bool
                     }

makeLenses ''LineInput

instance HandleEvent (LineInput a) where
    handleEvent (EvKey KEnter []) li = submitInput li
    handleEvent (EvKey KEsc   []) li = return $ submitted .~ True $ li
    handleEvent ev                li = handleEventLensed li editor ev

renderLineInput :: LineInput model -> Widget
renderLineInput li = if li^.submitted then emptyWidget else Edit.renderEditor (li^.editor)

submitInput :: LineInput model -> EventM (LineInput model)
submitInput li = do
    liftIO . runHandler $ li ^. submit $ Edit.getEditContents $ li ^. editor
    return $ submit .~ const (return ())
           $ submitted .~ True $ li

nodeAdd
    :: Relation
    -> Node
    -> LineInput model
nodeAdd rel parent = LineInput
    { _editor = Edit.editor "node-editor" (str . unlines) (Just 1) ""
    , _submit = \contents' -> do
        let contents = head contents'
        thingy <- U.createThingy contents
        node   <- U.addThingyGr thingy
        U.addRelGr (parent, node, rel)
    , _submitted = False
    }

inactive :: LineInput model
inactive= LineInput (error "Not used") (error "Not used") True
