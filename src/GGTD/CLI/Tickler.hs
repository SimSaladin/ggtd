------------------------------------------------------------------------------
-- |
-- Module         : GGTD.CLI.Tickler
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.CLI.Tickler where

import GGTD.Tickler
import GGTD.CLI.Base
import GGTD.CLI.Render
import GGTD.CLI.Option

import System.Console.Command

-- | List all active ticklers and status of tickler worker
ticklerAction :: Action IO
ticklerAction =
    handler $ listTicklers >>= mapM_
        (\((n,th), ta) -> pp $ renderNodeTicklers n th ta)

-- | Add a tickler to a node
--
-- Arguments: [NODE] [TICKLER] [ACTION]
--
-- where TICKLER conforms to one of the following:
--   * every [ma|ti|ke|to|pe|la] <[until DD.MM]>
--   * (others?)
--
-- and ACTION
--   * flag: (unset flag)
ticklerAddAction :: Action IO
ticklerAddAction =
    withNonOption nodeType $ \nodeP ->
    withNonOption ticklerType $ \tickler ->
    withNonOption flagType $ \flag ->
    handler $ fromNodeP nodeP >>= \case
        Nothing -> return ()
        Just node -> attachTickler tickler (TSetFlag flag Nothing) node

-- | Remove ALL ticklers associated with a node
--
-- Arguments: [NODE]
ticklerRmAction :: Action IO
ticklerRmAction =
    withNonOption nodeType $ \nodeP ->
    handler $ fromNodeP nodeP >>= \case
        Nothing -> return ()
        Just node -> removeTicklers node
