------------------------------------------------------------------------------
-- |
-- Module         : GGTD.DB.Query
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.DB.Query where

import           GGTD.Base
import           GGTD.Filter
import           GGTD.Sort

import           Control.Lens hiding ((&), Context, Context')
import           Data.Tree
import           Data.Tuple (swap)
import           Data.Graph.Inductive.Graph

type View = Tree (Relation, Context') -- ^ The context, and via which path we reached it

getViewAtGr :: [Filter] -> [Sort] -> Node -> Handler ([View], Gr')
getViewAtGr fltr srt node = do
    g <- use gr
    let recurse :: Context' -> [(Node, Relation)]
        recurse (_,_,_,xs) = map swap . (each._2 %~ node') . applySorts srt . filter (applyFilters fltr) $ (each._2 %~ context g) xs
    xdfWith' recurse (,) [(node, "")] <$> use gr

