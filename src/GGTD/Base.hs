------------------------------------------------------------------------------
-- |
-- Module         : GGTD.Base
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
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

type Handler = StateT DB IO

data DB = DB
        { _gr :: Gr'
        , _nodeEnum :: Node
        , _rootNode :: Node -- ^ Parent of everything
        , _viewContext :: Node -- ^ Active context root
        } deriving (Show, Read)

type Gr' = Gr Thingy Relation
type Context' = Context Thingy Relation

-- | Nodes
data Thingy = Thingy
    { _created :: UTCTime
    , _content :: String
    , _flags :: Map Flag String
    } deriving (Show, Read, Eq)

data Flag = Done
          | Wait
          | Priority
          deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Edges
type Relation = String

makeLenses ''Thingy
makeLenses ''DB

-- * Graph algorithms

-- | like @xdfWith@, but with extra information about the traversed nodes
-- propagated into the tree.
xdfWith' :: (Graph gr)
    => CFun a b [(Node, e)]
    -> (e -> CFun a b c)
    -> [(Node, e)]
    -> gr a b
    -> ([Tree c],gr a b)
xdfWith' _ _ []     g             = ([],g)
xdfWith' _ _ _      g | isEmpty g = ([],g)
xdfWith' d f ((v,e):vs) g = case match v g of
                        (Nothing,g1) -> xdfWith' d f vs g1
                        (Just c,g1)  -> (Node (f e c) ts:ts',g3)
                                 where (ts,g2)  = xdfWith' d f (d c) g1
                                       (ts',g3) = xdfWith' d f vs g2

-- * Util

ifExists :: Node -> Handler () -> Handler () -> Handler ()
ifExists node failure success = do
    exists <- use gr <&> gelem node
    if exists then success else failure

nodeNotFound :: Handler ()
nodeNotFound = liftIO $ putStrLn "Node not found"
