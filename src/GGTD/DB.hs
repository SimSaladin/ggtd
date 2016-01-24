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

import           GGTD.Base
import           GGTD.DB.Update (createThingy)

import           Control.Monad.Trans.State.Strict (get)
import           Control.Monad.Trans.Class
import           Data.Graph.Inductive.Graph
import           System.Directory (doesFileExist)

dbFile :: FilePath
dbFile = "./ggtd.db"

-- | Read the DB from ./ggtd.db
loadDB :: IO DB
loadDB = do
    exists <- doesFileExist dbFile
    if exists
        then do db <- read <$> readFile "ggtd.db"
                _gr db `seq` return db -- TODO: should use acid-state?
        else do root <- createThingy "All"
                return $ DB
                    { _gr = insNode (0, root) empty
                    , _rootNode = 0
                    , _nodeEnum = 1
                    , _viewContext = 0
                    }

-- | Save the DB to disk.
saveDB :: Handler ()
saveDB = do
    db <- get
    lift $ writeFile dbFile (show db)
