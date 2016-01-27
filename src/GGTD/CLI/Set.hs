------------------------------------------------------------------------------
-- |
-- Module         : GGTD.CLI.Set
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- Set options that control what and how things are displayed in the CLI.
------------------------------------------------------------------------------
module GGTD.CLI.Set where

import           GGTD.Base
import           GGTD.CLI.Base
import           GGTD.CLI.Option
import           GGTD.CLI.Render
import           GGTD.Filter
import           GGTD.Sort

import           Control.Lens hiding ((&), Context, Context')
import           Control.Monad.IO.Class
import           System.Console.Command

setContextAction :: Action IO
setContextAction =
    withNonOption (nodeOptType (-1)) $ \nodeP ->
    foldingOpts filters $ \fltr ->
    withOption sortOpt $ \srt ->
    handler $ fromNodeP nodeP >>= \case
        Nothing -> return ()
        Just node -> do
            current <- use viewContext
            let onSuccess            = viewContext .= node >> printChildren fltr srt node
                onError | node == -1 = liftIO $ putStrLn $ "Current context is " ++ show current
                        | otherwise  = nodeNotFound
            ifExists node onError onSuccess
