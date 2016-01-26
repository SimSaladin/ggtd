------------------------------------------------------------------------------
-- |
-- Module         : GGTD.CLI.Edge
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.CLI.Edge where

import GGTD.Base
import GGTD.CLI.Base
import GGTD.CLI.Option
import GGTD.CLI.Render
import GGTD.DB.Update

import Control.Lens
import Control.Applicative
import System.Console.Command

-- |
-- Arguments: [FROM] [TO] [RELATION]
edgeChangeAction :: Action IO
edgeChangeAction =
    withNonOption nodeType $ \nodeP ->
    withNonOption nodeType $ \node'P ->
    withNonOption relType $ \rel ->
    handler $ fromNodeP nodeP >>= \nodePM -> fromNodeP node'P >>= \node'PM -> case liftA2 (,) nodePM node'PM of
        Nothing -> return ()
        Just (node, node') ->
            overNode node $ sucadj.each.filtered ((== node') . snd) .adjlab .~ rel

edgeParentAction :: Action IO
edgeParentAction =
    withNonOption nodeType $ \nodeP ->
    withNonOption nodeType $ \newParentP ->
    handler $ fromNodeP nodeP >>= \nodePM -> fromNodeP newParentP >>= \parentPM -> case liftA2 (,) nodePM parentPM of
        Nothing -> return ()
        Just (node, newParent) -> do
            setParentGr node newParent
            printNode node

-- | Create a new edge
--
-- Arguments: -r[REL] [FROM] [TO] 
edgeCreateAction :: Action IO
edgeCreateAction =
    withOption relOpt $ \rel ->
    withNonOption nodeType $ \fromP ->
    withNonOption nodeType $ \toP ->
    handler $ fromNodeP fromP >>= \fromPM -> fromNodeP toP >>= \toPM -> case liftA2 (,) fromPM toPM of
        Nothing -> return ()
        Just (fromN, toN) -> addRelGr (fromN, toN, rel)
