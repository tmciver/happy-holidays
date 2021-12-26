module Lib (solve) where

import           Data.List (permutations)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Data.Set (Set)
import qualified Data.Set as Set

type LetterMap = Map Char Char

letters :: [Char]
letters = ['a', 'd', 'h', 'i', 'l', 'o', 'p', 's', 'y']

numbers :: [Char]
numbers = "0123456789"

perms :: [[Char]]
perms = take 9 <$> permutations numbers

-- |Return the list of all LetterMaps for the given set of characters.
mappings :: [LetterMap]
mappings = Map.fromList <$> f <$> zip (repeat letters) perms
  where f :: ([Char], [Char]) -> [(Char, Char)]
        f (cs, is) = zip cs is

isSolution :: LetterMap
           -> String
           -> String
           -> String
           -> Bool
isSolution lm word1 word2 result =
  let i = toInt lm word1
      j = toInt lm word2
      k = toInt lm result
  in i + j == k

-- |Given a LetterMap and a word return the integer that corresponds to that word.
toInt :: LetterMap -> String -> Int
toInt lm word = read . fromJust $ traverse (flip Map.lookup lm) word

-- |Return the LetterMaps, if any, that, when used to sum the corresponding
-- integers of the first two words yield the corresponding integer of the third
-- word.
solve :: String -> String -> String -> [LetterMap]
solve word1 word2 sum =
  [lm | lm <- mappings, isSolution lm word1 word2 sum]
