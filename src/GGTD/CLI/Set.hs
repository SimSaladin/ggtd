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
import           GGTD.DB.Query (getContext)
import           GGTD.CLI.Base
import           GGTD.CLI.Option
import           GGTD.CLI.Render
import           GGTD.Filter
import           GGTD.Relation
import           GGTD.Sort

import           Control.Lens hiding ((&), Context, Context')
import           Control.Monad (msum)
import           Control.Monad.IO.Class
import           Data.Graph.Inductive.Graph
import qualified Data.List as L
import           System.Console.Command

setContextAction :: Action IO
setContextAction =
    withNonOption (nodeOptType (-1)) $ \nodeP ->
    handler $ fromNodeP nodeP >>= \case
        Nothing -> return ()
        Just node -> do
            current <- use viewContext
            let onSuccess            = switchParent node
                onError | node == -1 = liftIO $ putStrLn $ "Current context is " ++ show current
                        | otherwise  = nodeNotFound
            ifExists node onError onSuccess

-- | Traverse in context up one parent
--
-- Takes the most coherent route, i.e. first @relGroup>@, if not present
-- then @relLink>@.
setUpAction :: Action IO
setUpAction =
    handler $ do
        herectx <- use viewContext >>= getContext
        case herectx of
            Nothing -> nodeNotFound
            Just ctx -> do
                let best = msum
                        [ L.find ( (== relChild) . snd) (lpre' ctx)
                        , L.find ( (== relGroup) . snd) (lpre' ctx)
                        , L.find ( (== relLink) . snd) (lpre' ctx)
                        ]
                case best of
                    Nothing -> liftIO $ putStrLn $ "No suitable parent could be determined"
                    Just (nd,_) -> switchParent nd

-- * Utility

switchParent :: Node -> Handler ()
switchParent nd = viewContext .= nd >> printChildren [FNotFlag Done] defaultSort nd
