------------------------------------------------------------------------------
-- |
-- Module         : GGTD.CLI
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.CLI where

import           GGTD.Base
import           GGTD.CLI.Ls
import           GGTD.CLI.Todo
import           GGTD.CLI.Set
import           GGTD.CLI.Node
import           GGTD.CLI.Edge

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Tree
import           System.Console.Command
import           System.Console.Program (showUsage)
import           System.Exit (exitSuccess)

-- | Default commands
commands :: Commands Handler
commands = Node
    (command "ggtd" "A graphy getting-things-done application" . io $ lift $ putStrLn "No command given; try \"ggtd help\".")
    [
      Node (command "ls" "List thingies. By default lists all not-done tasks starting at current view context." lsAction) []
    , Node (command "todo" "Print a flat list of items, a TODO list" todoAction) []
    , Node (command "context" "Set the active context to the given node" setContextAction) []
    , Node (command "node" "Add, modify and delete nodes. node \"Some node description\" creates a node with the given description under the current view context." nodeAction)
        [
          Node (command "create" "Create a new node with the specified edge" nodeCreateAction) [] 
        , Node (command "update" "Update the content" nodeUpdateAction) []
        , Node (command "done" "Set the task done" $ nodeFlagAction Done) []
        , Node (command "wait" "Set to waiting state" $ nodeFlagAction Wait) []
        , Node (command "priority" "Set node priority" $ nodePriorityAction) []
        ]
    , Node (command "edge" "Add, modify or delete edges" edgeAction)
        [ 
          Node (command "relation" "Change the relation type" edgeChangeAction) []
        , Node (command "parent" "Change the parent of a node" edgeParentAction) []
        ]
    , Node (command "quit" "Quit the application" $ io $ liftIO exitSuccess) []
    , Node (command "help" "Show help" $ io $ showUsage commands) []
    ]


