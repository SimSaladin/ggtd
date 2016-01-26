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

import GGTD.CLI.Option
import GGTD.CLI.Base
import GGTD.CLI.Render
import GGTD.Filter
import GGTD.Sort

import System.Console.Command

todoAction :: Action IO
todoAction =
    foldingOpts filters $ \flt_opt ->
    foldingOpts sorts $ \srt_opt ->
    handler $ do
        let flt = FNotContent "someday/maybe" : flt_opt
            srt = sortFirstRel "child" : srt_opt
        printChildrenFlat flt srt 0

