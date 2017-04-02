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

import           GGTD.TUI.Base hiding (update)
import qualified GGTD.TUI.TaskList as TaskList -- for thingy view
import qualified GGTD.DB.Query as Q
import qualified GGTD.DB.Update as U

import           Data.Tree as Tree

type TreeNav = Q.TaggedView (Bool, Bool) -- ^ (expanded, focused)?

-- XXX: ignores child nodes
new :: Node -> Handler TreeNav
new node = -- TODO: partial: head
    Q.tagging (True, True) tag . head . fst <$> Q.getViewAtGr fltr [] node
  where
  tag  = const (False, False)
  fltr = [ FAny [ FRel relGroup, FRel relLink ] ]

update :: Node -> TreeNav -> Handler TreeNav
update node old = do
    tnav <- combine <$> new node <*> pure old
    -- focus something if nothing is focused
    case findFocusedNode tnav of
        Nothing -> return $ setfocus tnav
        _ -> return tnav

up, down :: TreeNav -> (Node, TreeNav)
up root = case findFocusedNode root' of
              Just focus -> (focus^._2, root')
              Nothing -> (0, root') -- TODO hard-coded default
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

down root = case newfocus of
                Just focus -> (focus^._2, root')
                Nothing    -> case findFocusedNode root of
                                  Just focus -> (focus^._2, root)
                                  Nothing    -> (0, root) -- TODO hard-coded default
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

findFocusedNode :: TreeNav -> Maybe (Relation, Node, Thingy)
findFocusedNode = foldl -- note: doesn't use the tree structure
    (\this ((rel, ctx), (_, focus)) ->
        if focus then Just (rel, node' ctx, lab' ctx)
                 else this
    ) Nothing

findFocusedParentNode :: TreeNav -> Node
findFocusedParentNode (Node this subs) = case go (node' $ snd $ fst this) subs of
                                             Nothing -> (node' $ snd $ fst this)
                                             Just n  -> n
    where
    go    _     [] = Nothing
    go curp (x:xs)
        | Node ((_,ctx), (_, focus)) xs' <- x
        = if focus then Just curp
                   else go curp xs `mplus` go (node' ctx) xs'

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

deleteFocused :: TreeNav -> Handler TreeNav
deleteFocused tn = do
    case findFocusedNode tn of
        Nothing -> return ()
        Just (_,node,_) -> U.deleteNodeGr node
    update 0 tn
    -- TODO hard-coded root

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

-- * other

-- | @combine new old@ preserves structure and content from new, while
-- restoring tags from old.
combine :: TreeNav -> TreeNav -> TreeNav 
combine tnew@(Node (nthis, _tags) nsub)
             (Node (othis, otags) osub)
    | takeNodeTree nthis == takeNodeTree othis = Node (nthis, otags) (combineF nsub osub)
    | otherwise                                = tnew
 where
    takeNodeTree = node' . snd

    combineF []      _     = []
    combineF nforest []    = nforest
    combineF (n:ns) (o:os) = combine n o : combineF ns os
