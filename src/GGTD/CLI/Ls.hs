------------------------------------------------------------------------------
-- |
-- Module         : GGTD.CLI.Ls
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.CLI.Ls where

import           GGTD.Base
import           GGTD.CLI.Option
import           GGTD.CLI.Render
import           GGTD.CLI.Base
import           GGTD.Filter
import           GGTD.Sort

import           Control.Lens hiding ((&), Context, Context')
import           System.Console.Command

-- | Tree
lsAction :: Action IO
lsAction =
    withNonOption (nodeOptType (-1)) $ \nodeP ->
    foldingOpts filters $ \fltr ->
    withOption sortOpt $ \srt ->
    handler $ fromNodeP nodeP >>= \case
        Nothing -> return ()
        Just node -> do
            node' <- if node == -1 then use viewContext else return node
            printChildren fltr srt node'
