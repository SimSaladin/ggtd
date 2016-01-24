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

import           Data.Function (on)
import           Data.Char
import           Data.Ord (comparing, Down(..))
import           Data.List (sortBy)
import           Data.Graph.Inductive.Graph
import qualified Data.Map as Map
import           System.Console.Argument (option)
import qualified System.Console.Argument as Arg

-- | Sorting stuff.
data Sort = SFlag Flag
          | SCreated
          | SRelation (Relation -> Relation -> Ordering)
          | SDesc Sort
          | SNone

flagSortDefault :: Flag -> Sort
flagSortDefault Wait = SDesc (SFlag Wait)
flagSortDefault Priority = SFlag Priority
flagSortDefault _ = SNone

-- | Note that the order is important: sorts deciding at the head of the
-- list always takes precedence.
sorts :: [Arg.Option Sort]
sorts = flagSorts ++ [createdSort]
  where
    createdSort =
        let parse :: String -> Either String Sort
            parse "desc" = Right $ SDesc SCreated
            parse "asc"  = Right SCreated
            parse ""     = Right SCreated
            parse x      = Left $ "Expected desc, asc or nothing but got: \"" ++ x ++ "\" instead."

            in option "c" ["created"] (Arg.Type parse "ASC" Nothing) (SDesc SCreated) "Sort by creation date"

    flagSorts = do
        flg <- [minBound..maxBound]

        let parse :: String -> Either String Sort
            parse "desc" = Right (SDesc $ SFlag flg)
            parse "asc"  = Right (SFlag flg)
            parse ""     = Right (SDesc $ SFlag flg)
            parse x      = Left $ "Expected desc, asc or nothing but got: \"" ++ x ++ "\" instead."

            short = head (show flg)
            long  = "sort-by-" ++ map toLower (show flg)
            typ   = Arg.Type parse "ASC" Nothing
            desc  = "Sort by flag \"" ++ show flg ++ "\""

        return $ option [short] [long] typ (flagSortDefault flg) desc

applySorts :: [Sort] -> [(Relation, Context')] -> [(Relation, Context')]
applySorts srt = sortBy (buildSortOrd srt)
  where
    buildSortOrd :: [Sort] -> (Relation, Context') -> (Relation, Context') -> Ordering
    buildSortOrd []     = \_ _ -> EQ
    buildSortOrd (x:xs) = sortFunc x `combining` buildSortOrd xs
        
    sortFunc :: Sort -> (Relation, Context') -> (Relation, Context') -> Ordering
    sortFunc x = case x of
        SFlag flag   -> comparing $ Down . viewFlag flag
        SCreated     -> comparing $ _created . lab' . snd
        SRelation f  -> f `on` fst
        SDesc s      -> \a b -> case sortFunc s a b of
                                    LT -> GT
                                    GT -> LT
                                    EQ -> EQ
        SNone        -> \_ _ -> EQ

    viewFlag flag (_, ctx) = Map.lookup flag . _flags $ lab' ctx

    combining :: (a -> a -> Ordering) -> (a -> a -> Ordering) -> a -> a -> Ordering
    combining f g x y = case (f x y, g x y) of
                            (EQ, r) -> r
                            (r, _)  -> r

sortFirstRel :: Relation -> Sort
sortFirstRel rel = SRelation $ \a b -> if | a == rel && b == rel -> EQ
                                          | a == rel             -> LT
                                          | b == rel             -> GT
                                          | otherwise            -> EQ

