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

import           Control.Monad.IO.Class
import           Data.Graph.Inductive.Graph
import           System.Console.Argument (Option, option, Type(..))
import qualified System.Console.Argument as Arg
import           System.Console.Command

-- * Arguments

nodeOpt :: Option Node
nodeOpt = option "n" ["node"] nodeType (-1) "Node"

parentOpt :: Option Node
parentOpt = option "p" ["parent"] nodeType (-1) "Parent node"

contentOpt :: Option String
contentOpt = option "c" ["content"] Arg.string
    { parser = \str -> if null str then Left "Cannot be empty" else Right str }
    "" "The content"

relOpt :: Option Relation
relOpt = option "r" ["relation"] Arg.string
    { parser = \str -> if null str then Left "Relation cannot be empty" else Right str }
    "child" "Relation to context"

-- ** Types

nodeType :: Arg.Type Node
nodeType = fromIntegral <$> Arg.integer { Arg.name = "NODE" }

relType :: Arg.Type Relation
relType = Arg.string { Arg.name = "RELATION" }

contentType :: Arg.Type String
contentType = Arg.string { Arg.name = "CONTENT" }

-- * Utility

foldingOpts :: MonadIO m => [Option a] -> ([a] -> Action m) -> Action m
foldingOpts (o:os) f = withOption o $ \r -> foldingOpts os (\rs -> f (r:rs))
foldingOpts [] f = f []


