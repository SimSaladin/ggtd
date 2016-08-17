module Main where

import GGTD.CLI (commands)
import GGTD.TUI (terminalUI)
import GGTD.Tickler (forkTicklerWorker)
import GGTD.DB (loadDB, setDB)
import System.Console.Program (interactive)

import System.Environment (getArgs)

main :: IO ()
main = do
    loadDB >>= setDB
    _ <- forkTicklerWorker
    args <- getArgs
    case args of
        ["b"] -> terminalUI
        _     -> interactive commands
