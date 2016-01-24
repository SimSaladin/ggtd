------------------------------------------------------------------------------
-- |
-- Module         : GGTD.CLI.Edge
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.CLI.Edge where

import           GGTD.Base
import           GGTD.CLI.Option
import           GGTD.CLI.Render
import           GGTD.DB (saveDB)
import           GGTD.DB.Update

import           Control.Monad.Trans.Class
import           System.Console.Command

edgeAction :: Action Handler
edgeAction = io $ do
    lift $ putStrLn "Not yet implemented"

edgeChangeAction :: Action Handler
edgeChangeAction = withNonOption nodeType $ \node -> withNonOption relType $ \rel -> io $ do
    setRelGr node rel
    printNode node
    saveDB

edgeParentAction :: Action Handler
edgeParentAction = withNonOption nodeType $ \node -> withNonOption nodeType $ \newParent -> io $ do
    setParentGr node newParent
    printNode node
    saveDB
