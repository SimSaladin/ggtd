------------------------------------------------------------------------------
-- |
-- Module         : GGTD.DB.Update
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.DB.Update where

import           GGTD.Base

import           Control.Lens hiding ((&), Context, Context')
import           Control.Monad.IO.Class
import           Data.Graph.Inductive.Graph
import           Data.Time (getCurrentTime)
import qualified Data.Map as Map


-- * Create nodes

createThingy :: MonadIO m => String -> m Thingy
createThingy txt = Thingy <$> liftIO getCurrentTime <*> pure txt <*> pure Map.empty

-- | Creates the new node and adds correct edges.
addThingyGr :: Thingy -> Handler Node
addThingyGr thingy = do
    nxt <- nodeEnum <+= 1
    gr %= insNode (nxt, thingy)
    return nxt

-- * Create relations

-- | @addRelGr (parent, child, rel)@ creates the given relation.
addRelGr :: (Node, Node, Relation) -> Handler ()
addRelGr edge = gr %= insEdge edge

-- | Replaces all parent links with a link from given node.
setParentGr :: Node -> Node -> Handler ()
setParentGr node parent = overNode node (ctxParents.each.adjNode .~ parent)

delRelGr :: (Node, Node) -> Handler ()
delRelGr edge = gr %= delEdge edge

-- * Modify nodes

alterFlagGr :: Node -> Flag -> Maybe String -> Handler ()
alterFlagGr node flag val = overNode node $
    ctxLab.flags %~ Map.alter (const val) flag

updateContentGr :: Node -> String -> Handler ()
updateContentGr node cnt = overNode node (ctxLab.content .~ cnt)

-- | Combinator used by graph modifying code.
overNode :: Node -> (Context' -> Context') -> Handler ()
overNode node f = use gr >>= go
  where
    go g | (Just ctx, g') <- match node g = gr .= (f ctx & g')
         | otherwise                      = nodeNotFound

-- * Delete nodes

deleteNodeGr :: Node -> Handler ()
deleteNodeGr node = gr %= delNode node
