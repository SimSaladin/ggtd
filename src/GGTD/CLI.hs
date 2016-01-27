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

import GGTD.Base
import GGTD.CLI.Edge
import GGTD.CLI.Ls
import GGTD.CLI.Node
import GGTD.CLI.Set
import GGTD.CLI.Tickler
import GGTD.CLI.Todo

import Data.Tree
import System.Console.Command
import System.Console.Program (showUsage)
import System.Exit (exitSuccess)

-- | Default commands
commands :: Commands IO
commands = Node
    (command "ggtd" "A graphy getting-things-done application" . io $ putStrLn "No command given; try \"ggtd help\".")
    [
      Node (command "ls" "List thingies. By default lists all not-done tasks starting at current view context." lsAction) []
    , Node (command "in" "Add a new thingy into the \"in\" node" inAction) []
    , Node (command "action" "Print a flat list of actionable items" todoAction) []
    , Node (command "grep" "Grep contents of all items" nodeGrepAction) []
    , Node (command "done" "Set the task done" $ nodeFlagAction Done) []
    , Node (command "wait" "Set to waiting state" $ nodeFlagAction Wait) []
    , Node (command "context" "Set the active context to the given node" setContextAction) []
    , Node (command "node"
           "Add, modify and delete nodes.\n\
           \node \"Some node description\" creates a node\n\
           \with the given description under the current view context."
           nodeCreateAction)
        [
          Node (command "show" "Show node context" nodeShowAction) [] 
        , Node (command "update" "Update the content" nodeUpdateAction) []
        , Node (command "priority" "Set node priority" $ nodePriorityAction) []
        ]
    , Node (command "rel" "Add, modify or delete edges" edgeCreateAction)
        [ 
          Node (command "update" "Change the relation type" edgeChangeAction) []
        , Node (command "parent" "Change the parent of a node" edgeParentAction) []
        , Node (command "delete" "Remove a relation" edgeDeleteAction) []
        ]
    , Node (command "tickler" "Manage ticklers" ticklerAction)
        [
          Node (command "add" "Attach a tickler to a node" ticklerAddAction) []
        , Node (command "rm" "Remove tickler(s) from a node" ticklerRmAction) []
        -- , Node (command "ls" "List all active ticklers") []
        ]
    , Node (command "quit" "Quit the application" $ io $ exitSuccess) []
    , Node (command "help" "Show help" $ io $ showUsage commands) []
    ]


