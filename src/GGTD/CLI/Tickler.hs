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

import GGTD.Base
import GGTD.Tickler
import GGTD.DB.Update
import GGTD.CLI.Base
import GGTD.CLI.Render
import GGTD.CLI.Option

import Data.Time
import Control.Monad.IO.Class (liftIO)
import System.Console.Command

-- | List all active ticklers and status of tickler worker
ticklerAction :: Action IO
ticklerAction =
    handler $ do
        now <- liftIO getCurrentTime
        listTicklers >>= mapM_ (\((n,th), ta) -> pp $ renderNodeTicklers now n th ta)

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

-- | Defer a given node to a later date.
--
-- Arguments: -p[DAYS] [NODE]
ticklerDeferAction :: Action IO
ticklerDeferAction =
    withOption daysOpt $ \days ->
    withNonOption nodeType $ \nodeP ->
    handler $ fromNodeP nodeP >>= \case
        Nothing -> return ()
        Just node -> do
            now <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
            alterFlagGr node Wait (Just "deferred")
            attachTickler (TDay $ addDays days now) (TSetFlag Wait Nothing) node
