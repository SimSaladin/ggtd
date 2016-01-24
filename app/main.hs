module Main where

import GGTD.CLI (commands)
import GGTD.DB (loadDB)
import Control.Monad (void)
import Control.Monad.Trans.State.Strict (runStateT)
import System.Console.Program (interactive)

main :: IO ()
main = do
    db <- loadDB
    void $ runStateT (interactive commands) db
