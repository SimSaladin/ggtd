------------------------------------------------------------------------------
-- |
-- Module         : GGTD.TUI.TreeNav
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.TUI.TreeNav where

import           GGTD.TUI.Base
import qualified GGTD.TUI.TaskList as TaskList -- for thingy view
import qualified GGTD.DB.Query as Q

import           Data.Tree as Tree

type TreeNav = Q.TaggedView (Bool, Bool) -- ^ (expanded, focused)?

-- XXX: ignores child nodes
new :: Node -> Handler TreeNav
new node = -- TODO: partial: head
    Q.tagging (True, True) tag . head . fst <$> Q.getViewAtGr fltr [] node
  where
  tag  = const (False, False)
  fltr = [ FRel relGroup ]

up, down :: TreeNav -> (Node, TreeNav)
up root = (findFocusedNode root', root')
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
down root = if newfocus < 0 then (findFocusedNode root, root) else (newfocus, root')
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

-- * rendering

toImage :: TreeNav -> Image
toImage (Node ((rel, ctx), (expanded, focused)) xs)
    = thisBalloon <|> thisItem
        <-> (if expanded then childItems else mempty)
  where
    thisBalloon
        | rel == relLink = string defAttr "L "
        | expanded       = string defAttr "- "
        | otherwise      = string defAttr "+ "
    thisItem
        | focused     = TaskList.viewThingyFocused (lab' ctx)
        | hasChildren = TaskList.viewThingyUnfocused (lab' ctx)
        | otherwise   = viewThingyNonImportant (lab' ctx)

    hasChildren = length (suc' ctx) > 0

    childItems
        = pad 1 0 0 0 $ vertCat $ map toImage xs

viewThingyNonImportant :: Thingy -> Image
viewThingyNonImportant thingy = string (defAttr `withForeColor` brightGreen) (thingy^.content)
