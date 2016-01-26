------------------------------------------------------------------------------
-- |
-- Module         : GGTD.CLI.Todo
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.CLI.Todo where

import GGTD.Base
import GGTD.CLI.Option
import GGTD.CLI.Base
import GGTD.CLI.Render
import GGTD.Filter
import GGTD.Sort

import Control.Lens
import System.Console.Command

-- | Actionable items.
--
-- Arguments: <sorts> <filters> [<NODE>]
todoAction :: Action IO
todoAction =
    foldingOpts filters $ \flt_opt ->
    foldingOpts sorts $ \srt_opt ->
    withNonOption (nodeOptType (-1)) $ \nodeP ->
    handler $ fromNodeP nodeP >>= \case
        Nothing -> return ()
        Just nodeS -> do
            node <- if nodeS == -1 then use viewContext else return nodeS
            let flt = FNotContent "someday/maybe" : flt_opt
                srt = sortFirstRel "child" : srt_opt
            printChildrenFlat flt srt node

