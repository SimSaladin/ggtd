{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
------------------------------------------------------------------------------
-- |
-- Module         : GGTD.Base
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- This module defines the basic types and some lenses.
------------------------------------------------------------------------------
module GGTD.Base where

import           Control.Lens hiding ((&), Context, Context')
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict
import           Data.Tree
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.Query.DFS (CFun)
import           Data.Graph.Inductive.PatriciaTree
import           Data.Time
import           Data.Map (Map)
import qualified Data.Map as Map
import           GHC.Generics (Generic)
import           Data.Yaml (ToJSON(..), FromJSON(..))

-- | Use Handlers to make modificationss to the DB.
type Handler = StateT DB IO

data DB = DB
        { _gr :: Gr'
        , _nodeEnum    :: Node -- Note: could be excluded and use fgl to find a free node id
        , _rootNode    :: Node -- ^ Parent of everything
        , _viewContext :: Node -- ^ Active context root
        , _ticklerLast :: Day  -- ^ Last tickler run
        } deriving (Show, Read, Generic)

type Gr' = Gr Thingy Relation

type Context' = Context Thingy Relation

-- | Nodes
data Thingy = Thingy
    { _created :: UTCTime
    , _content :: String
    , _flags :: Map Flag String
    } deriving (Show, Read, Eq, Generic)

data Flag = Done
          | Wait
          | Priority
          | Ticklers
          deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

-- | Edges
type Relation = String

-- * Lenses

makeLenses ''Thingy
makeLenses ''DB

-- ** Lenses for graph manipulation

preadj, sucadj :: Lens' (Context a b) (Adj b)
preadj = _1
sucadj = _4

adjnode :: Lens' (b, Node) Node
adjnode = _2
adjlab :: Lens' (b, Node) b
adjlab  = _1

-- * Graph algorithms

-- | like @xdfWith@, but with extra information about the traversed nodes
-- propagated into the tree.
--
-- Used to make the depth recursion (first argument) at the current node
-- depend on the edge we arrived to it. See "GGTD.DB.Query.getViewAtGr".
xdfWith' :: (Graph gr)
    => (e -> CFun a b [(Node, e)])
    -> (e -> CFun a b c)
    -> [(Node, e)]
    -> gr a b
    -> ([Tree c],gr a b)
xdfWith' _ _ []     g             = ([],g)
xdfWith' _ _ _      g | isEmpty g = ([],g)
xdfWith' d f ((v,e):vs) g = case match v g of
                        (Nothing,g1) -> xdfWith' d f vs g1
                        (Just c,g1)  -> (Node (f e c) ts:ts',g3)
                                 where (ts,g2)  = xdfWith' d f (d e c) g1
                                       (ts',g3) = xdfWith' d f vs g2

-- * Util

ifExists :: Node -> Handler () -> Handler () -> Handler ()
ifExists node failure success = do
    exists <- use gr <&> gelem node
    if exists then success else failure

nodeNotFound :: Handler ()
nodeNotFound = liftIO $ putStrLn "Node not found"

------------------------------------
-- Instances

instance ToJSON DB
instance FromJSON DB

instance ToJSON Gr'
instance FromJSON Gr'

instance ToJSON Flag
instance FromJSON Flag 

instance ToJSON Thingy
instance FromJSON Thingy

instance ToJSON (Map Flag String) where
    toJSON = toJSON . Map.mapKeys show
instance FromJSON (Map Flag String) where
    parseJSON = fmap (Map.mapKeys read) . parseJSON
