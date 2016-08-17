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

import           GGTD.Base
import           GGTD.DB (runHandler)
import qualified GGTD.DB.Query as Q

import           Control.Lens hiding ((&), Context, Context')
import           Control.Monad.IO.Class (liftIO)
import           Control.Exception (finally)
import           Data.Graph.Inductive.Graph
import           Graphics.Vty
import           Data.Tree as Tree
import           System.Exit (exitSuccess)

type TaskList = ([Thingy], [Thingy]) -- ^ (before, at : after) focus

-- XXX: Move
type TreeNav = Q.TaggedView (Bool, Bool) -- ^ (expanded, focused)?

data Model = Model
    { _curTaskList  :: TaskList
    , _urgentMatter :: Maybe [String] -- notifications
    , _treeNav      :: TreeNav 
    }
-- makeLenses
makeLenses ''Model

-- | vty-based UI for ggtd.
terminalUI :: IO ()
terminalUI = do
    cfg <- standardIOConfig
    vty <- mkVty cfg

    let go model = do update vty (viewModel model)
                      e <- nextEvent vty
                      go =<< runHandler (updateModel e model)

    model_init <- runHandler initialModel

    go model_init `finally` shutdown vty

-- MODEL

initialModel :: Handler Model
initialModel = Model
    <$> getTaskList 0
    <*> pure Nothing
    <*> getTreeNav 0

-- VIEW

viewModel :: Model -> Picture
viewModel Model{..} = picForImage $
    viewTreeNav _treeNav
    <-> viewUrgentMatter _urgentMatter
    <-> viewTaskList _curTaskList

viewTreeNav :: TreeNav -> Image
viewTreeNav [theRoot] =

viewUrgentMatter :: Maybe [String] -> Image
viewUrgentMatter Nothing = mempty
viewUrgentMatter (Just xs) = vertCat [ string (defAttrs `withForeColor` red) x | x <- xs ]

viewTaskList :: TaskList -> Image
viewTaskList (before, []) = vertCat (map viewThingyUnfocused before)
viewTaskList (before, atFocus : after)
    = vertCat $ map viewThingyUnfocused before
             ++ [ viewThingyFocused atFocus ]
             ++ map viewThingyUnfocused after

viewThingyUnfocused :: Thingy -> Image
viewThingyUnfocused thingy = string defAttrs (thingy^.content)

viewThingyFocused :: Thingy -> Image
viewThingyFocused thingy = string (defAttr `withForeColor` green) (thingy^.content)

-- UPDATE

updateModel :: Event -> Model -> Handler Model
updateModel EvMouse{} model = return model
updateModel EvResize{} model = return model
updateModel (EvKey key mods) model = handleKey key mods model

handleKey :: Key -> [Modifier] -> Model -> Handler Model
handleKey (KChar 'c') [MCtrl] = \_ -> liftIO exitSuccess
handleKey (KChar 'q') [] = \_ -> liftIO exitSuccess

handleKey KUp   [] = return . (curTaskList %~ taskListUp)
handleKey KDown [] = return . (curTaskList %~ taskListDown)
handleKey (KChar 'k') [] = return . (curTaskList %~ taskListUp)
handleKey (KChar 'j') [] = return . (curTaskList %~ taskListDown)

handleKey _ _ = return

------------------- GENERIC ------------------------------

-- TaskList

type IsTask = Relation -> Bool

-- | Move focus by one.
taskListUp, taskListDown :: TaskList -> TaskList
taskListUp   ([], after) = ([], after)
taskListUp   (before, after) = (init before, last before : after)
taskListDown (before, []) = (before, [])
taskListDown (before, after) = (before ++ [head after], tail after)

-- | Build task list recursively from given node.
getTaskList :: Node -> IsTask -> Handler TaskList
getTaskList node isTask = do
    ( [ Node _ xs ], _) <- Q.getViewAtGr [] [] node
    return ([], [ thingy | (Node (_rel, (_,_,thingy,_)) _) <- xs ])

-- TreeNav

type IsGroup = Relation -> Bool

getTreeNav :: Node -> IsGroup -> TreeNav
getTreeNav node isGroup = fst <$> Q.getViewAtGr _ _ node
