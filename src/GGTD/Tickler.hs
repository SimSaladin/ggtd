------------------------------------------------------------------------------
-- |
-- Module         : GGTD.Tickler
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- Adding a tickler to a node allows for the node to change whenever the
-- tickler activates.
------------------------------------------------------------------------------
module GGTD.Tickler where

import GGTD.Base
import GGTD.DB (runHandler)
import GGTD.DB.Update

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Graph.Inductive.Graph
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate (toOrdinalDate, mondayStartWeek)
import Data.Time.Calendar.MonthDay (dayOfYearToMonthAndDay)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)

-- * Types

type Year = Int
type Month = Int
type Week = Int
type DayOfWeek = Int

data Tickler = TDayOfWeek DayOfWeek -- ^ Every given day of week
             | TMonth Month -- ^ Beginning of the month
             | TMonthly -- ^ Beginning of any month
             | TYear Year -- ^ Beginning of a year
             | TDay Day -- ^ A specific day
             deriving (Show, Read, Eq)

-- | What actions a tickler may do.
data TicklerAction = TSetFlag Flag (Maybe String)
                   deriving (Show, Read, Eq)

-- * Worker

forkTicklerWorker :: IO ThreadId
forkTicklerWorker = forkIO $ forever goTickle
  where
    goTickle = do
        prev    <- runHandler $ use ticklerLast
        current <- getZonedTime <&> localDay . zonedTimeToLocalTime
        if prev < current
            then runHandler $ runTicklers (addDays 1 prev) current
            else threadDelay duration
    
    duration = 1 * 60 * 60 * 1000000 -- one hour

-- * Handlers

-- | Attaches a tickler to a node.
attachTickler :: Tickler -> TicklerAction -> Node -> Handler ()
attachTickler tickler action node =
    overNode node $ _3.flags %~ Map.alter ins Ticklers
  where
    ins :: Maybe String -> Maybe String
    ins (Just str) = Just . show . (++ [(tickler, action)]) $ read str
    ins Nothing    = Just (show [(tickler, action)])

-- | Remove ALL assigned ticklers from a node. 
removeTicklers :: Node -> Handler ()
removeTicklers node =
    overNode node $ _3.flags %~ Map.alter (const Nothing) Ticklers

listTicklers
    :: Handler [ ( LNode Thingy
                 , [(Tickler, TicklerAction)] ) ]
listTicklers = use gr <&> mapMaybe go . labNodes
  where
    go ln@(_, th) = do
        ts <- read <$> Map.lookup Ticklers (_flags th)
        return (ln, ts)

-- | 
runTicklers
    :: Day -- ^ Start day, inclusive
    -> Day -- ^ End day, inclusive
    -> Handler ()
runTicklers start end = do
    listTicklers >>= mapM_ go
    ticklerLast .= end
  where
    go ((n, _), ts) = mapM_ (runTicklerAction n . snd) $ filter (matches . fst) ts

    matches (TDayOfWeek day) = day `elem` triggeredWeekDays
    matches (TMonth month)   = month `elem` triggeredMonths
    matches (TYear year)     = fromIntegral year `elem` triggeredYears
    matches (TDay day)       = start <= day && day <= end
    matches TMonthly         = not $ null triggeredMonths

    triggeredWeekDays = map (snd . mondayStartWeek) $ take 7 [start .. end]
    triggeredMonths   = take 12 $ map snd triggeredMonthsYears
    triggeredYears    = [ year | (year, 1) <- triggeredMonthsYears ]

    triggeredMonthsYears =
        [ (year, month)
            | day <- [start .. end]
            , let (year, yearDay) = toOrdinalDate day
            , (month, 1) <- [dayOfYearToMonthAndDay (isLeapYear year) yearDay]
        ]

runTicklerAction :: Node -> TicklerAction -> Handler ()
runTicklerAction node (TSetFlag flag mcontent) =
    overNode node $ _3.flags %~ Map.alter (const mcontent) flag

-- * Time utilities

-- | Set the time to Monday 0:00 of the running week.
toStartOfWeek :: LocalTime -> LocalTime
toStartOfWeek LocalTime{..} =
    let (y, w, _) = toWeekDate localDay
    in LocalTime { localDay = fromWeekDate y w 1, localTimeOfDay = midnight }

toStartOfMonth :: LocalTime -> LocalTime
toStartOfMonth LocalTime{..} =
    let (y, m, _) = toGregorian localDay
    in LocalTime { localDay = fromGregorian y m 1, localTimeOfDay = midnight }

addWeeks :: Integer -> LocalTime -> LocalTime
addWeeks n time = time { localDay = addDays (7 * n) (localDay time) }

-- | Clips to the end of month if necessary.
addMonths :: Integer -> LocalTime -> LocalTime
addMonths n time = time { localDay = addGregorianMonthsClip n (localDay time) }

getMonth :: LocalTime -> Month
getMonth = view _2 . toGregorian . localDay

getLocalTime :: MonadIO m => m LocalTime
getLocalTime = liftIO $ getZonedTime <&> zonedTimeToLocalTime
