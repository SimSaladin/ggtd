------------------------------------------------------------------------------
-- |
-- Module         : GGTD.CLI.Render
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.CLI.Render where

import           GGTD.Base
import           GGTD.Filter
import           GGTD.Sort
import           GGTD.DB.Query

import           Control.Lens hiding ((&), Context, Context')
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict
import           Data.Tree
import           Data.Graph.Inductive.Graph
import qualified Data.Map as Map
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Text.Regex as Regex
import qualified Text.Regex.Base as Regex

data Renderer = Renderer
              { renderViewRoot :: View -> P.Doc
              , renderViewForest :: [View] -> P.Doc
              , renderThingyLine :: Node -> Thingy -> P.Doc
              , renderFlag :: Flag -> P.Doc
              , renderContent :: String -> P.Doc
              }

relGroup, relChild :: Relation
relGroup = "group"
relChild = "child"

relations :: [Relation]
relations = [relGroup, relChild]

defaultRenderer :: Renderer
defaultRenderer = Renderer
    { renderViewRoot = \(Node (_, ctx) sub) ->
        P.hang 2 $ P.dullblue (renderThingyLine defaultRenderer (node' ctx) (lab' ctx))
            P.<$$> renderViewForest defaultRenderer sub

    , renderViewForest = P.vcat . map (\(Node (rel, ctx) sub) ->
        let header = if | rel == relGroup -> P.magenta
                        | rel == relChild -> (P.red "*" P.<+>)
                        | otherwise       -> (P.blue (P.text rel) P.<+>)

        in P.hang 2 $ if null sub
                then header (renderThingyLine defaultRenderer (node' ctx) (lab' ctx))
                else header (renderThingyLine defaultRenderer (node' ctx) (lab' ctx))
                        P.<$$> renderViewForest defaultRenderer sub
        )

    , renderThingyLine = \node Thingy{..} ->
        renderContent defaultRenderer _content
        P.<+>
        P.yellow (P.brackets $ P.dullmagenta $ P.text (show node))
        P.<+>
        (if Map.null _flags then P.empty else P.tupled $ map (renderFlag defaultRenderer) $ Map.keys _flags)

    , renderContent = \cnt ->
        let rticket = Regex.makeRegex ("(#[0-9]*)" :: String) :: Regex.Regex
        in case Regex.getAllSubmatches $ Regex.match rticket cnt of
            [_,(start,len)]
                | (s1, s2) <- splitAt start cnt -> P.text s1 P.<> P.dullcyan (P.text (take len s2)) P.<> P.text (drop len s2)
            _ -> P.text cnt

    , renderFlag = \case
        Done -> P.dullgreen "done"
        Wait -> P.dullyellow "waiting"
        Priority -> P.red "PRIORITY"
    }

data DocFx = DocFx P.Doc [DocFx]

printTreeAt :: [Filter] -> [Sort] -> Node -> Handler ()
printTreeAt fltr srt node = liftIO . mapM_ (P.putDoc . renderViewRoot defaultRenderer) . fst =<< getViewAtGr fltr srt node

-- | Print the (direct or indirect) children of a node.
printChildren :: [Filter] -> [Sort] -> Node -> Handler ()
printChildren fltrs srts nd = do
    DB{..} <- get
    printTreeAt fltrs srts nd
    liftIO $ putStrLn ""

printChildrenFlat :: [Filter] -> [Sort] -> Node -> Handler ()
printChildrenFlat fltr srt node = do
    (forest, _) <- getViewAtGr fltr srt node
    liftIO $ mapM_ (\(_, ctx) -> P.putDoc $ renderThingyLine defaultRenderer (node' ctx) (lab' ctx) P.<> P.hardline) $ filter isActionable $ concatMap flatten forest

-- | Only @relChild@'s are actionable
isActionable :: (Relation, Context') -> Bool
isActionable (rel, _) = rel == relChild

printNode :: Node -> Handler ()
printNode node = do
    mthingy <- use gr <&> flip lab node
    maybe nodeNotFound (liftIO . P.putDoc . renderThingyLine defaultRenderer node) mthingy 
