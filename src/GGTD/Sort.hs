------------------------------------------------------------------------------
-- |
-- Module         : GGTD.Sort
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.Sort where

import           GGTD.Base
import           GGTD.Relation

import           Data.Function                  ( on )
import           Data.Ord                       ( comparing, Down(..) )
import qualified Data.List                      as L
import           Data.Graph.Inductive.Graph
import qualified Data.Map                       as Map
import           Control.Applicative            ( liftA2 )
import           System.Console.Argument        ( option )
import qualified System.Console.Argument        as Arg

data SortAtom =
     SFlag Flag
   | SCreated
   | SRelation (Relation -> Relation -> Ordering)
   | SDesc SortAtom

-- | The sort atoms are performed sequentially, and the head of the list
-- always takes precedence.
type Sort = [SortAtom]

defaultSort :: Sort
defaultSort =
    [ sortLastRel relLink -- Links go always last
    , sortLastRel relGroup -- Then we have groups almost at the bottom
    , SDesc (SFlag Wait)
    , SFlag Priority
    , SCreated
    ]

sortOpt :: Arg.Option Sort
sortOpt = option "s" ["sort"] sortType defaultSort "Sort the results"
  where
    sortType = Arg.Type parse "[SORT]" (Just defaultSort)

    parse :: String -> Either String Sort
    parse []  = return []
    parse str = go $ L.span (== ',') str

    go (opt, str) = liftA2 (:) (parseOpt opt) (parse str)

    parseOpt ('-':x)   = SDesc <$> parseOpt x
    parseOpt "created" = pure SCreated
    parseOpt "done"    = pure (SFlag Done)
    parseOpt "wait"    = pure (SFlag Wait)
    parseOpt str       = fail $ "Unknown sort predicate " ++ str

applySort :: Sort -> [(Relation, Context')] -> [(Relation, Context')]
applySort srt = L.sortBy (buildSortOrd srt)
  where
    buildSortOrd :: Sort -> (Relation, Context') -> (Relation, Context') -> Ordering
    buildSortOrd []     = \_ _ -> EQ
    buildSortOrd (x:xs) = sortFunc x `combining` buildSortOrd xs
        
    sortFunc :: SortAtom -> (Relation, Context') -> (Relation, Context') -> Ordering
    sortFunc x = case x of
        SFlag flag   -> comparing $ Down . viewFlag flag
        SCreated     -> comparing $ _created . lab' . snd
        SRelation f  -> f `on` fst
        SDesc s      -> \a b -> case sortFunc s a b of
                                    LT -> GT
                                    GT -> LT
                                    EQ -> EQ

    viewFlag flag (_, ctx) = Map.lookup flag . _flags $ lab' ctx

    combining :: (a -> a -> Ordering) -> (a -> a -> Ordering) -> a -> a -> Ordering
    combining f g x y = case (f x y, g x y) of
                            (EQ, r) -> r
                            (r, _)  -> r

sortFirstRel, sortLastRel :: Relation -> SortAtom
sortFirstRel rel = SRelation $ \a b -> if | a == rel && b == rel -> EQ
                                          | a == rel             -> LT
                                          | b == rel             -> GT
                                          | otherwise            -> EQ
sortLastRel = SDesc . sortFirstRel
