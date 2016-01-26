------------------------------------------------------------------------------
-- |
-- Module         : GGTD.CLI.Base
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.CLI.Base where

import GGTD.Base
import GGTD.DB
import System.Console.Command

handler :: Handler () -> Action IO
handler = io . runHandler
