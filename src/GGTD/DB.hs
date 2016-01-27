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
import Control.Monad
import Control.Monad.Trans.State.Strict (runStateT)
import Data.Graph.Inductive.Graph
import Data.Time.Calendar
import qualified Data.Yaml as Yaml
import System.FilePath
import System.Directory
import System.Exit
import System.IO.Unsafe (unsafePerformIO)

dbVar :: TMVar DB
dbVar = unsafePerformIO newEmptyTMVarIO

-- * Paths

appName :: FilePath
appName = "ggtd"

-- | Database file.
dbLocation :: IO FilePath
dbLocation = do
    dir <- getXdgDirectory XdgData "ggtd"
    createDirectoryIfMissing True dir
    return $ dir </> "current.yaml"

-- * Execute

-- | lock the database and execute a handler
runHandler :: Handler a -> IO a
runHandler h = do
    db <- atomically $ takeTMVar dbVar
    (res, db') <- runStateT h db
    writeDB db'
    atomically $ putTMVar dbVar db'
    return res

writeDB :: DB -> IO ()
writeDB db = do
    loc <- dbLocation
    exists <- doesFileExist loc
    when exists $ copyFile loc (loc <.> ".backup")
    Yaml.encodeFile loc db

-- | Read the DB from "dbLocation".
loadDB :: IO DB
loadDB = do
    loc <- dbLocation
    exists <- doesFileExist loc
    if exists then Yaml.decodeFileEither loc >>= either failed return else newDB
  where
    newDB = do
        root <- createThingy "All"
        return $ DB
            { _gr = insNode (0, root) empty
            , _rootNode = 0
            , _nodeEnum = 1
            , _viewContext = 0
            , _ticklerLast = ModifiedJulianDay 0
            }
    failed err = putStrLn (show err) >> exitFailure

setDB :: DB -> IO ()
setDB = atomically . putTMVar dbVar
