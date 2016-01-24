------------------------------------------------------------------------------
-- |
-- Module         : GGTD.CLI.Node
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.CLI.Node where

import           GGTD.Base
import           GGTD.CLI.Option
import           GGTD.CLI.Render
import           GGTD.DB (saveDB)
import           GGTD.DB.Update

import           Control.Lens hiding ((&), Context, Context')
import           System.Console.Command
import qualified System.Console.Argument as Arg

-- | The default action is to create a node with the given content.
nodeAction :: Action Handler
nodeAction = withNonOption contentType $ \cnt -> io $ do
    thingy <- createThingy cnt
    node <- addThingyGr thingy
    parent <- use viewContext
    addRelGr (parent, node, "child")
    saveDB
    printChildren [] [] =<< use viewContext

nodeCreateAction :: Action Handler
nodeCreateAction =
    withOption parentOpt $ \nd ->
    withNonOption relType $ \rel ->
    withNonOption contentType $ \cnt ->
    io $ do
        thingy <- createThingy cnt
        node <- addThingyGr thingy
        parent <- if nd >= 0 then return nd else use viewContext
        addRelGr (parent, node, rel)
        printNode node
        saveDB

nodeUpdateAction :: Action Handler
nodeUpdateAction = withNonOption nodeType $ \node -> withNonOption Arg.string $ \cnt -> io $ do
    updateContentGr node cnt
    saveDB

nodeFlagAction :: Flag -> Action Handler
nodeFlagAction flag = withNonOption nodeType $ \node -> io $ do
    setFlagGr node flag ""
    saveDB

nodePriorityAction :: Action Handler
nodePriorityAction = withNonOption nodeType $ \node ->
    withNonOption Arg.integer $ \int ->
    io $ do
        setFlagGr node Priority (show int)
        saveDB

