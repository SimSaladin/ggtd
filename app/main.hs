module Main where

import GGTD.CLI (commands)
import GGTD.Tickler (forkTicklerWorker)
import GGTD.DB (loadDB, setDB)
import System.Console.Program (interactive)

main :: IO ()
main = do
    loadDB >>= setDB
    _ <- forkTicklerWorker
    interactive commands
