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
import           GGTD.Filter
import           GGTD.Sort

import           Control.Lens hiding ((&), Context, Context')
import           System.Console.Command

-- | Tree
lsAction :: Action Handler
lsAction = foldingOpts filters $ \fltr -> foldingOpts sorts $ \srt -> io $ do
    node <- use viewContext
    printChildren fltr srt node
