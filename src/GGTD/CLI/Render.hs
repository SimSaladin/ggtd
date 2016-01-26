{-# LANGUAGE FlexibleInstances #-}
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
import           Control.Monad.Trans.State.Strict
import           Data.Time
import           Data.Tree
import           Data.Graph.Inductive.Graph
import qualified Data.Map as Map
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Text.Regex as Regex
import qualified Text.Regex.Base as Regex

-- * New class-interface

class Renderable x where
    render :: x -> P.Doc

instance Renderable Tickler where
    render (TDayOfWeek dayOfWeek) = P.text "Every" P.<+> P.dullcyan (P.text (formatDayOfWeek dayOfWeek))
    render (TMonth month)         = P.text "Beginning of" P.<+> P.dullblue (P.text (formatMonth month))
    render (TYear year)           = P.text "Year" P.<+> P.dullgreen (P.text (show year))
    render (TDay day)             = P.dullgreen (P.text (formatTime defaultTimeLocale "%F" day))

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

renderNodeTicklers :: Node -> Thingy -> [(Tickler, TicklerAction)] -> P.Doc
renderNodeTicklers n th ta =
    P.hang 2 (renderThingyLine defaultRenderer n th P.<$$> P.vsep (map render ta))
        P.<$$> P.empty

-- * Time units

formatDayOfWeek :: DayOfWeek -> String
formatDayOfWeek n = fst $ wDays defaultTimeLocale !! n

formatMonth :: Month -> String
formatMonth n = fst $ months defaultTimeLocale !! n

-- * Old interface

-- | The old interface
-- deprecated; please use the 'Render'-class.
data Renderer = Renderer
              { renderViewRoot :: View -> P.Doc
              , renderViewForest :: [View] -> P.Doc
              , renderThingyLine :: Node -> Thingy -> P.Doc
              , renderContent :: String -> P.Doc
              }

defaultRenderer :: Renderer
defaultRenderer = Renderer
    { renderViewRoot = \(Node (_, ctx) sub) ->
        P.hang 2 $ P.dullblue (renderThingyLine defaultRenderer (node' ctx) (lab' ctx))
            P.<$$> renderViewForest defaultRenderer sub

    , renderViewForest = P.vcat . map (\(Node (rel, ctx) sub) ->
        let header = if | rel == relGroup -> P.yellow
                        | rel == relChild -> (P.red "*" P.<+>)
                        | rel == relLink  -> P.dullmagenta
                        | otherwise       -> (P.red (P.text rel) P.<+>)

        in P.hang 2 $ if null sub
                then header (renderThingyLine defaultRenderer (node' ctx) (lab' ctx))
                else header (renderThingyLine defaultRenderer (node' ctx) (lab' ctx))
                        P.<$$> renderViewForest defaultRenderer sub
        )

    , renderThingyLine = \node Thingy{..} ->
        let color = if Map.member Wait _flags then P.green else id
        in
            color (renderContent defaultRenderer _content)
            P.<+>
            P.yellow (P.brackets $ P.dullmagenta $ P.text (show node))
            P.<+>
            (if Map.null _flags then P.empty else P.tupled $ map render $ Map.keys _flags)

    , renderContent = \cnt ->
        let rticket = Regex.makeRegex ("(#[0-9]*)" :: String) :: Regex.Regex
        in case Regex.getAllSubmatches $ Regex.match rticket cnt of
            [_,(start,len)]
                | (s1, s2) <- splitAt start cnt -> P.text s1 P.<> P.dullcyan (P.text (take len s2)) P.<> P.text (drop len s2)
            _ -> P.text cnt
    }

-- * Trees

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

printNode :: Node -> Handler ()
printNode node = do
    mthingy <- use gr <&> flip lab node
    maybe nodeNotFound (liftIO . P.putDoc . renderThingyLine defaultRenderer node) mthingy 

-- * Utility

pp :: P.Doc -> Handler ()
pp = liftIO . P.putDoc

-- | Only @relChild@'s are actionable
isActionable :: (Relation, Context') -> Bool
isActionable (rel, _) = rel == relChild
