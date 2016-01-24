------------------------------------------------------------------------------
-- |
-- Module         : GGTD.Filter
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.Filter where

import           GGTD.Base

import           Data.Char
import qualified Data.Map as Map
import           System.Console.Argument (option)
import qualified System.Console.Argument as Arg

data Filter = FFlag Flag
            | FNotFlag Flag
            | FNotContent String
            | FNone

-- | For this autogeneration to be sensible, every Flag constructor must
-- have unique first character.
filters :: [Arg.Option Filter]
filters = do
    flg <- [minBound..maxBound]

    let parse :: String -> Either String Filter
        parse "no"  = Right (FNotFlag flg)
        parse "yes" = Right (FFlag flg)
        parse ""    = Right (FFlag flg)
        parse x     = Left $ "Expected yes, no or nothing but got: \"" ++ x ++ "\" instead."

        short = toLower $ head (show flg)
        long  = map toLower $ show flg
        typ   = Arg.Type parse "NO" Nothing
        desc  = "Filter by flag \"" ++ show flg ++ "\""

    return $ option [short] [long] typ (flagFilterDefault flg) desc

-- | Set default filter options here.
flagFilterDefault :: Flag -> Filter
flagFilterDefault Done = FNotFlag Done
flagFilterDefault _    = FNone

-- | True if passes all filters.
applyFilters :: [Filter] -> (Relation, Context') -> Bool
applyFilters fltrs (_, (_,_,thingy,_)) = all applyFilter fltrs
  where
    applyFilter FNone = True
    applyFilter (FNotFlag flg) = Map.notMember flg (_flags thingy)
    applyFilter (FFlag flg) = Map.member flg (_flags thingy)
    applyFilter (FNotContent str) = _content thingy /= str
