------------------------------------------------------------------------------
-- |
-- Module         : GGTD.CLI.Option
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.CLI.Option where

import           GGTD.Base
import           GGTD.DB.Query
import           GGTD.Relation
import           GGTD.Tickler

import           Text.Read (readMaybe)
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Graph.Inductive.Graph
import qualified Data.List as L
import           System.Console.Argument (Option, option, Type(..))
import qualified System.Console.Argument as Arg
import           System.Console.Command

-- * Arguments

-- | A pointer to node. Either the raw number value or some text which
-- identifies a node somewhat uniquely. Use "fromNodeP" to resolve
-- a "NodeP" into a node.
type NodeP = String

nodeOpt :: Option NodeP
nodeOpt = option "n" ["node"] nodeType "-1" "Node"

parentOpt :: Option NodeP
parentOpt = option "p" ["parent"] nodeType "-1" "Parent node"

contentOpt :: Option String
contentOpt = option "c" ["content"] Arg.string
    { parser = \str -> if null str then Left "Cannot be empty" else Right str }
    "" "The content"

relOpt :: Option Relation
relOpt = option "r" ["rel"] relType
    { parser = \str -> if | null str -> Left "Relation cannot be empty"
                          | str `elem` relations -> Right str
                          | otherwise -> Left $ "Relation should be one of " ++ show relations
    } "child"
    ("Set relation from parent " ++ show relations)

deleteOpt :: Option Bool
deleteOpt = option "d" ["delete"] Arg.boolean
    False "Delete instead of adding"

daysOpt :: Option Integer
daysOpt = option "n" ["days"] Arg.integer
    1 "Number of days"

-- ** Types

nodeType :: Arg.Type NodeP
nodeType = Arg.string { Arg.name = "NODE" }

nodeOptType :: Node -> Arg.Type NodeP
nodeOptType node = nodeType { Arg.defaultValue = Just (toNodeP node) }

relType :: Arg.Type Relation
relType = Arg.string { Arg.name = "REL" }

contentType :: Arg.Type String
contentType = Arg.string { Arg.name = "CONTENT" }

--   * every [ma|ti|ke|to|pe|la] <[until DD.MM]>
--   * (others?)
ticklerType :: Arg.Type Tickler
ticklerType = Arg.Type parse "TICKLER" Nothing
  where
    parse str = maybe (Left "Invalid tickler") Right $ L.lookup str days

    days = zip ["ma", "ti", "ke", "to", "pe", "la", "su"] (map TDayOfWeek [1..7])

flagType :: Arg.Type Flag
flagType = Arg.Type parse "FLAG" Nothing
  where
    parse str = maybe (Left "Invalid flag") Right $ L.lookup str opts

    opts = zip ["done", "wait", "priority"] [minBound..maxBound]

-- * Utility

foldingOpts :: MonadIO m => [Option a] -> ([a] -> Action m) -> Action m
foldingOpts (o:os) f = withOption o $ \r -> foldingOpts os (\rs -> f (r:rs))
foldingOpts [] f = f []

fromNodeP :: NodeP -> Handler (Maybe Node)
fromNodeP p = case readMaybe p of
    Just node -> return (Just node)
    Nothing -> findNodes p >>= \case
        [(n,_)] -> return (Just n)
        []      -> liftIO $ putStrLn "No such node" >> return Nothing
        xs      -> reasonablyUnique p xs >>= \case
            Just (n,_) -> return (Just n)
            Nothing    -> do
                liftIO $ putStrLn "Multiple nodes found, please narrow down:"
                liftIO $ mapM_ print xs
                return Nothing

-- | A node of a list of nodes is reasonably unique, if one of the
-- following holds
--  
--  * The argument is the only exact text match
--  * There is only one result as a direct successor of our current context
reasonablyUnique :: NodeP -> [(Node, Thingy)] -> Handler (Maybe (Node, Thingy))
reasonablyUnique p xs = do
    here <- use viewContext
    successors <- use gr <&> flip suc here
    if | [res] <- filter ( (== p) . _content . snd ) xs -> return (Just res)
       | [res] <- filter ( (`elem` successors) . fst ) xs -> return (Just res)
       | otherwise -> return Nothing

toNodeP :: Node -> NodeP
toNodeP = show
