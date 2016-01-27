------------------------------------------------------------------------------
-- |
-- Module         : GGTD.CLI.Node
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.CLI.Node where

import           GGTD.Base
import           GGTD.CLI.Option
import           GGTD.CLI.Render
import           GGTD.CLI.Base
import           GGTD.DB.Update
import           GGTD.DB.Query
import           GGTD.Relation

import           Control.Lens hiding ((&), Context, Context')
import           Control.Monad.IO.Class (liftIO)
import           Data.Time (getCurrentTime)
import           Data.Graph.Inductive.Graph
import           System.Console.Command
import qualified System.Console.Argument as Arg

-- | Arguments: -p[PARENT] -r[RELATION] [CONTENT]
nodeCreateAction :: Action IO
nodeCreateAction =
    withOption parentOpt $ \parentP ->
    withOption relOpt $ \rel ->
    withNonOption contentType $ \cnt ->
    handler $ fromNodeP parentP >>= \case
        Nothing -> return ()
        Just parent' -> do
            parent <- if parent' >= 0 then return parent' else use viewContext
            thingy <- createThingy cnt
            node <- addThingyGr thingy
            addRelGr (parent, node, rel)
            printNode node

-- | Arguments: [NODE] [CONTENT]
nodeUpdateAction :: Action IO
nodeUpdateAction =
    withNonOption nodeType $ \nodeP ->
    withNonOption Arg.string $ \cnt ->
    handler $ fromNodeP nodeP >>= \case
        Nothing -> return ()
        Just node -> do
            updateContentGr node cnt

-- | Sets the flag
--
-- Arguments: [-d] [NODE]
nodeFlagAction :: Flag -> Action IO
nodeFlagAction flag =
    withNonOption nodeType $ \nodeP ->
    withOption deleteOpt $ \del ->
    withOption contentOpt $ \cnt ->
    handler $ fromNodeP nodeP >>= \case
        Nothing -> return ()
        Just node -> alterFlagGr node flag $
            if del then Nothing else Just cnt

-- | Arguments: [NODE] [PRIORITY]
nodePriorityAction :: Action IO
nodePriorityAction =
    withNonOption nodeType $ \nodeP ->
    withNonOption Arg.integer $ \int ->
    handler $ fromNodeP nodeP >>= \case
        Nothing -> return ()
        Just node -> alterFlagGr node Priority (Just (show int))

-- | Arguments: [CONTENT]
inAction :: Action IO
inAction =
    withNonOption contentType $ \cnt ->
    handler $ getRootChildByLabel "in" >>= \case
        Nothing -> liftIO $ putStrLn "The in-node was not found as a parent of root (0). (You may need to create it first)"
        Just parent -> do
            thingy <- createThingy cnt
            node <- addThingyGr thingy
            addRelGr (parent, node, relChild)
            printNode node

-- | Show the context of a node.
--
-- Arguments: [NODE]
nodeShowAction :: Action IO
nodeShowAction =
    withNonOption nodeType $ \nodeP ->
    handler $ fromNodeP nodeP >>= \case
        Nothing -> return ()
        Just node -> do
            (mctx, _) <- use gr <&> match node
            case mctx of
                Just ctx -> liftIO getCurrentTime >>= \now -> pp (render (now, ctx))
                Nothing -> nodeNotFound

-- | Grep nodes
--
-- Arguments: [QUERY]
nodeGrepAction :: Action IO
nodeGrepAction =
    withNonOption contentType $ \qstr ->
    handler $ do now <- liftIO getCurrentTime
                 findNodes qstr >>= pp . renderNodesFlat now
