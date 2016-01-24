------------------------------------------------------------------------------
-- |
-- Module         : GGTD.CLI.Set
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.CLI.Set where

import           GGTD.Base
import           GGTD.CLI.Option
import           GGTD.CLI.Render

import           Control.Lens hiding ((&), Context, Context')
import           Control.Monad.IO.Class
import           System.Console.Command
import qualified System.Console.Argument as Arg

setContextAction :: Action Handler
setContextAction = withNonOption (Arg.optional (-1) nodeType) $ \node -> io $ do
    current <- use viewContext
    let onSuccess            = viewContext .= node >> printChildren [] [] node
        onError | node == -1 = liftIO $ putStrLn $ "Current context is " ++ show current
                | otherwise  = nodeNotFound
    ifExists node onError onSuccess
