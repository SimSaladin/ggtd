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
import           GGTD.Relation

import           Control.Lens hiding ((&), Context, Context')
import qualified Data.List as L
import           Data.Tree
import           Data.Tuple (swap)
import           Data.Graph.Inductive.Graph

type View = Tree (Relation, Context') -- ^ The context, and via which path we reached it

getViewAtGr :: [Filter] -> [Sort] -> Node -> Handler ([View], Gr')
getViewAtGr fltr srt node = do
    g <- use gr
    let recurse :: Relation -> Context' -> [(Node, Relation)]
        recurse rel (_,_,_,xs)
            | rel == relLink = [] -- Don't recurse into link-node's ancestors.
            | otherwise = map swap
            . over (each._2) node'
            . applySorts srt . filter (applyFilters fltr)
            $ over (each._2) (context g) xs
    xdfWith' recurse (,) [(node, "")] <$> use gr

getRootChildByLabel :: String -> Handler (Maybe Node)
getRootChildByLabel str = use gr <&> go
  where
    go :: Gr' -> Maybe Node
    go g = let f n = maybe False ((== str) . _content) $ lab g n
               in fst (match 0 g) >>= L.find f . pre'

-- | Find by exact submatch.
findNodes :: String -> Handler [LNode Thingy]
findNodes str = use gr <&> filter f . labNodes
  where
    f (_, th) = str `L.isInfixOf` _content th
