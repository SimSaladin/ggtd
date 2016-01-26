------------------------------------------------------------------------------
-- |
-- Module         : GGTD.DB
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.DB where

import GGTD.Base
import GGTD.DB.Update (createThingy)

import Control.Concurrent.STM
import Control.Monad.Trans.State.Strict (runStateT)
import Data.Graph.Inductive.Graph
import Data.Time.Calendar
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)

dbVar :: TMVar DB
dbVar = unsafePerformIO newEmptyTMVarIO

dbFile :: FilePath
dbFile = "./ggtd.db"

-- | lock the database and execute a handler
runHandler :: Handler a -> IO a
runHandler h = do
    db <- atomically $ takeTMVar dbVar
    (res, db') <- runStateT h db
    writeFile dbFile (show db')
    atomically $ putTMVar dbVar db'
    return res

-- | Read the DB from ./ggtd.db
--
-- TODO: should use acid-state
loadDB :: IO DB
loadDB = do
    exists <- doesFileExist dbFile
    if exists
        then do db <- read <$> readFile "ggtd.db"
                _gr db `seq` return db
        else do root <- createThingy "All"
                return $ DB
                    { _gr = insNode (0, root) empty
                    , _rootNode = 0
                    , _nodeEnum = 1
                    , _viewContext = 0
                    , _ticklerLast = ModifiedJulianDay 0
                    }

setDB :: DB -> IO ()
setDB = atomically . putTMVar dbVar
