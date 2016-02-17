{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
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
import           GGTD.DB.Query
import           GGTD.Filter
import           GGTD.Relation
import           GGTD.Sort
import           GGTD.Tickler

import           Control.Lens hiding ((&), Context, Context')
import           Control.Monad.IO.Class
import           Data.Time
import           Data.Tree
import           Data.Graph.Inductive.Graph
import qualified Data.Map as Map
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Text.Regex as Regex
import qualified Text.Regex.Base as Regex
import           Data.Time.Format.Human (humanReadableTime')
import           System.IO (stdout)

-- * New class-interface

class Renderable x where
    render :: x -> P.Doc

instance Renderable Tickler where
    render (TDayOfWeek dayOfWeek) = P.text "Every" P.<+> P.dullcyan (P.text (formatDayOfWeek dayOfWeek))
    render (TMonth month)         = P.text "Beginning of" P.<+> P.dullblue (P.text (formatMonth month))
    render (TYear year)           = P.text "Year" P.<+> P.dullgreen (P.text (show year))
    render (TDay day)             = P.dullgreen (P.text (formatTime defaultTimeLocale "%F" day))
    render TMonthly               = P.dullblue "Every month"

instance Renderable TicklerAction where
    render (TSetFlag flag Nothing)   = P.text "unset flag" P.<+> render flag
    render (TSetFlag flag (Just "")) = P.text "set flag" P.<+> render flag
    render (TSetFlag flag (Just x))  = P.text "set flag" P.<+> render flag P.<+> "to" P.<+> P.text x

instance Renderable Flag where
    render flag = case flag of
        Done -> P.dullgreen "done"
        Wait -> P.green "waiting"
        Priority -> P.red "PRIORITY"
        Ticklers -> P.dullcyan "ticklers"

instance Renderable (Tickler, TicklerAction) where
    render (t,ta) = render t P.<> ":" P.<+> render ta

instance Renderable (UTCTime, Context') where
    render (now, (ap, node, thingy, as)) =
        P.vsep (map renderPre ap)
        P.<$$>
        "  " P.<> renderThingyLine now node thingy
        P.<$$>
        P.hang 5 ("     " P.<> P.vsep (map renderSuc as))

-- | (now, target)
instance Renderable (UTCTime, UTCTime) where
    render (now, target) = P.green . P.text $ humanReadableTime' now target

renderNodeTicklers :: UTCTime -> Node -> Thingy -> [(Tickler, TicklerAction)] -> P.Doc
renderNodeTicklers now n th ta =
    P.hang 2 (renderThingyLineFlat now Nothing n th P.<$$> P.vsep (map render ta))
        P.<$$> P.empty

-- | A generic single-line view of a thingy.
renderThingyLine :: UTCTime -> Node -> Thingy -> P.Doc
renderThingyLine now node Thingy{..} =
    let color = if Map.member Wait _flags then P.green else id
    in color (renderContent _content)
        P.<+> renderNode node
        P.<+> (if Map.null _flags then id else (P.tupled (map render $ Map.keys _flags) P.<+>))
            (render (now, _created))

-- | Like @renderThingyLine@, but for a flat rendering.
renderThingyLineFlat
    :: UTCTime
    -> Maybe String -- ^ Some extra stuff to be shown in brackets
    -> Node
    -> Thingy
    -> P.Doc
renderThingyLineFlat now mextra node Thingy{..} =
    let color = if Map.member Wait _flags then P.green else id
    in renderNode node
        P.<+> color (renderContent _content)
        P.<+> (maybe P.empty (P.yellow . P.brackets . P.dullyellow . P.text) mextra)
        P.<+> (if Map.null _flags then P.empty else P.tupled $ map render $ Map.keys _flags)
        P.<+> render (now, _created)

renderNodesFlat :: UTCTime -> [(Node, Thingy)] -> P.Doc
renderNodesFlat now = P.vcat . map (uncurry $ renderThingyLineFlat now Nothing)

renderViewRoot :: UTCTime -> Tree (Relation, Context') -> P.Doc
renderViewRoot now (Node (_, ctx) sub) =
  P.hang 2 $ P.dullblue (renderThingyLine now (node' ctx) (lab' ctx))
      P.<$$> renderViewForest now sub

renderViewForest :: UTCTime -> Forest (Relation, Context') -> P.Doc
renderViewForest now = P.vcat . map (\(Node (rel, ctx) sub) ->
  let header = if | rel == relGroup -> P.yellow
                  | rel == relChild -> (P.red "*" P.<+>)
                  | rel == relLink  -> P.dullmagenta
                  | otherwise       -> (P.red (P.text rel) P.<+>)

  in -- (if rel == relGroup then (P.empty P.<$$>) else id) $
      P.hang 2 $ if null sub
          then header (renderThingyLine now (node' ctx) (lab' ctx))
          else header (renderThingyLine now (node' ctx) (lab' ctx))
                  P.<$$> renderViewForest now sub
  )

renderContent :: String -> P.Doc
renderContent cnt =
  let rticket = Regex.makeRegex ("(#[0-9]*)" :: String) :: Regex.Regex
  in case Regex.getAllSubmatches $ Regex.match rticket cnt of
      [_,(start,len)]
          | (s1, s2) <- splitAt start cnt -> P.text s1 P.<> P.dullcyan (P.text (take len s2)) P.<> P.text (drop len s2)
      _ -> P.text cnt

-- | I.e. [x]
renderNode :: Node -> P.Doc
renderNode node = P.yellow (P.brackets $ P.dullmagenta $ P.text (show node))

-- | In conjunction with Render Context' instance.
renderPre, renderSuc :: (Relation, Node) -> P.Doc
renderPre (rel, node) = renderNode node P.<+> P.text (show rel)
renderSuc (rel, node) = P.text (show rel) P.<+> renderNode node

-- * Time units

formatDayOfWeek :: DayOfWeek -> String
formatDayOfWeek n = fst $ wDays defaultTimeLocale !! n

formatMonth :: Month -> String
formatMonth n = fst $ months defaultTimeLocale !! n

-- * Trees

data DocFx = DocFx P.Doc [DocFx]

printTreeAt :: [Filter] -> Sort -> Node -> Handler ()
printTreeAt fltr srt node = do
    now <- liftIO getCurrentTime
    (trees, _) <- getViewAtGr fltr srt node
    pp $ P.vcat $ map (renderViewRoot now) trees

-- | Print the (direct or indirect) children of a node.
printChildren :: [Filter] -> Sort -> Node -> Handler ()
printChildren fltrs srts nd = printTreeAt fltrs srts nd

-- | Flattens the tree before printing.
printChildrenFlat
    :: [Filter] -- ^ Filter (at forests)
    -> Sort -- ^ Sort (at forest)
    -> Sort -- ^ Sort (at the flat list)
    -> Node
    -> Handler ()
printChildrenFlat fltr srt srt' node = do
    (forest, _) <- getViewAtGr fltr srt node
    let flat     = concatMap flatten $ map (tagging "root" (_content . lab')) forest
        thingies = applySort' fst srt' $ filter (isActionable . fst) flat

    now <- liftIO getCurrentTime
    let totext ((_, ctx), parent) = renderThingyLineFlat now (Just parent) (node' ctx) (lab' ctx)

    pp $ P.vcat $ map totext thingies

-- | Actually context
printNode :: Node -> Handler ()
printNode node = do
    now <- liftIO getCurrentTime
    (mctx,_) <- use gr <&> match node
    maybe nodeNotFound (pp . render . (now,)) mctx 

-- * Utility

-- | Adds a newline to the end.
pp :: P.Doc -> Handler ()
pp = liftIO . P.displayIO stdout . P.renderPretty 0.4 300 . (P.<$$> P.empty)

-- | Only @relChild@'s are actionable
isActionable :: (Relation, Context') -> Bool
isActionable (rel, _) = rel == relChild
