{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
------------------------------------------------------------------------------
-- |
-- Module         : GGTD.TUI.TaskList
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- TaskList: a horizontal flat list of thingies.
module GGTD.TUI.TaskList where

import           GGTD.TUI.Base

import qualified GGTD.DB.Query as Q
import qualified GGTD.DB.Update as U

import           Brick.Widgets.Edit (Editor)
import qualified Brick.Widgets.Edit as Edit

import qualified Data.Tree as Tree

data TaskList = TaskList
              { root   :: Node
              , tl     :: ([(Relation, Node, Thingy)], [(Relation, Node, Thingy)]) -- ^ (before, at : after) focus
              , editor :: Maybe (Editor String Text, Maybe Node) -- ^ Maybe the node to be edited
              }

view :: TaskList -> Widget Text
view TaskList{..} = raw (viewtl tl) <=> maybe emptyWidget (Edit.renderEditor True . fst) editor
        where
    viewtl (before, []) = vertCat (map (viewThingyTask False) before)
    viewtl (before, atFocus : after)
        = vertCat $ map (viewThingyTask False) before
                 ++ [ viewThingyTask True atFocus ]
                 ++ map (viewThingyTask False) after

-- * Actions

-- | Move focus by one.
up, down :: TaskList -> TaskList
(up, down) = (helper up', helper down')
    where
        helper f TaskList{..} = TaskList { root = root, tl = f tl, editor = editor }

        up'   ([], after) = ([], after)
        up'   (before, after) = (init before, last before : after)
        down' (before, []) = (before, [])
        down' (before, after) = (before ++ [head after], tail after)

focusNode :: Node -> TaskList -> TaskList
focusNode n taskList = taskList { tl = scroll [] (before ++ after) }
    where (before, after) = tl taskList
          scroll xs [] = (xs, [])
          scroll xs (x : []) = (xs, x : [])
          scroll xs (x : ys)
            | x^._2 == n = (xs, x :ys)
            | otherwise  = scroll (xs ++ [x]) ys

-- | Build task list recursively from given node.
new :: Node -> Handler TaskList
new root = do
    ( [ Tree.Node _ xs ], _) <- Q.getViewAtGr [FNotRel relGroup] [SDesc (SFlag Done), SRelation linkLast] root
    let tl     = ([], [ (rel, n, thingy) | (Tree.Node (rel, (_,n,thingy,_)) _) <- xs ])
        editor = Nothing
    return TaskList{..}
    where
        linkLast a b | a == relLink, b == relLink = EQ
                     | a == relLink = GT
                     | b == relLink = LT
                     | otherwise    = EQ

deleteFocused :: TaskList -> Handler TaskList
deleteFocused taskList@TaskList{..} = case tl of
    (xs, (_, n, _) : ys) -> do
        U.deleteNodeGr  n
        return taskList { tl = (xs, ys) }
    _ -> return taskList

-- * content editing

editFocused :: TaskList -> Handler TaskList
editFocused taskList@TaskList{..} = case tl of
    (_, (_, n, _) : _) -> do
        Just (_,_,th,_) <- Q.getContext n
        return taskList { editor = Just (contentsEditor $ th^.content, Just n) }
    _ -> return taskList

openNewNodeEditor :: TaskList -> Handler TaskList
openNewNodeEditor taskList = return taskList { editor = Just (newNodeEditor, Nothing) }

exitEditor :: TaskList -> Handler TaskList
exitEditor taskList = return taskList { editor = Nothing }

submitEditor
    :: Node -- ^ Parent node
    -> TaskList -> Handler TaskList
submitEditor parent taskList = do
    let Just (edit,mtarget) = editor taskList
        (_, (_,curn,_) : _) = tl taskList
        contents            = head $ Edit.getEditContents edit
    case mtarget of
        Nothing -> do
            thingy <- U.createThingy contents
            node <- U.addThingyGr thingy
            U.addRelGr (parent, node, relChild)
        Just target ->
            U.updateContentGr target contents
    focusNode curn <$> new parent

handleBasicEditorKey :: Key -> [Modifier] -> TaskList -> EventM Text TaskList
handleBasicEditorKey key mods taskList

    | Just (edit, mt) <- editor taskList = do
        edit' <- handleEventLensed edit id Edit.handleEditorEvent (EvKey key mods)
        return taskList { editor = Just (edit', mt) }

    | otherwise = return taskList

-- * focus

-- | Modify the focused node. Recalculate list afterwards.
withFocused :: (Node -> Handler ()) -> TaskList -> Handler TaskList
withFocused f taskList@TaskList{..} = case tl of
    (_, (_, node, _) : _) -> f node >> fmap (focusNode node) (new root)
    _ -> return taskList

-- | Currently focused node (if any).
askFocused :: TaskList -> Maybe (Relation, Node, Thingy)
askFocused TaskList{tl} | (_, x : _) <- tl = Just x
                        | otherwise        = Nothing

-- * Utility

-- | 
-- [TASK] some item
viewThingyTask :: Bool -> (Relation, Node, Thingy) -> Image
viewThingyTask focused (rel, _, thingy)
    = string defAttr "[" <|> string defAttr relTypeString <|> string defAttr "] "
      <|> string focusAttr (thingy^.content)
    where
        focusAttr | focused = defAttr `withForeColor` green
                  | Just _ <- thingy^.flags.at Done = defAttr `withForeColor` brightGreen
                  | Just _ <- thingy^.flags.at Priority = defAttr `withForeColor` yellow
                  | otherwise = relTypeAttr

        relTypeAttr
            | rel == relChild = defAttr `withForeColor` brightBlue
            | rel == relLink  = defAttr `withForeColor` yellow
            | otherwise       = defAttr `withForeColor` red

        relTypeString
            | rel == relChild = "TASK"
            | rel == relLink  = "LINK"
            | otherwise       = rel

viewThingyUnfocused :: Thingy -> Image
viewThingyUnfocused thingy = string defAttr (thingy^.content)

viewThingyFocused :: Thingy -> Image
viewThingyFocused thingy = string (defAttr `withForeColor` blue) (thingy^.content)

-- * Editors

newNodeEditor :: Editor String Text
newNodeEditor = contentsEditor ""

contentsEditor :: String -> Editor String Text
contentsEditor = Edit.editor "contents-editor" render (Just 1)
    where render = str . unlines
