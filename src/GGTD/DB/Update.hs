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


-- * Modify

createThingy :: MonadIO m => String -> m Thingy
createThingy txt = Thingy <$> liftIO getCurrentTime <*> pure txt <*> pure Map.empty

-- | Creates the new node and adds correct edges.
addThingyGr :: Thingy -> Handler Node
addThingyGr thingy = do
    nxt <- nodeEnum <+= 1
    gr %= insNode (nxt, thingy)
    return nxt

addRelGr :: (Node, Node, Relation) -> Handler ()
addRelGr edge = gr %= insEdge edge

alterFlagGr :: Node -> Flag -> Maybe String -> Handler ()
alterFlagGr node flag val = overNode node $
    _3.flags %~ Map.alter (const val) flag

updateContentGr :: Node -> String -> Handler ()
updateContentGr node cnt = overNode node (_3.content .~ cnt)

setParentGr :: Node -> Node -> Handler ()
setParentGr node parent = overNode node (_1.each._2 .~ parent)

-- | Combinator used by graph modifying code.
overNode :: Node -> (Context' -> Context') -> Handler ()
overNode node f = use gr >>= go
  where
    go g | (Just ctx, g') <- match node g = gr .= (f ctx & g')
         | otherwise                      = nodeNotFound

deleteNodeGr :: Node -> Handler ()
deleteNodeGr node = gr %= delNode node
