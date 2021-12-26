{-# LANGUAGE NoImplicitPrelude #-}

module Lib (solve) where

import Protolude

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

type LetterMap = Map Char Integer
  deriving Show

letters :: [Char]
letters = ['a', 'd', 'h', 'i', 'l', 'o', 'p', 's', 'y']

isSolution :: LetterMap
           -> Text
           -> Text
           -> Text
           -> Bool
isSolution lm word1 word2 result =
  let i = toInt lm word1
      j = toInt lm word2
      k = toInt lm result
  in i + j == k

-- |Given a LetterMap and a word return the integer that corresponds to that word.
toInt :: LetterMap -> Text -> Int
toInt lm word = let (_, i) = foldr f (0, 0) (T.unpack word) in i
  where f :: Int -> (Int, Int) -> (Int, Int)
        f x (n, acc) = (n+1, acc + x*10^n)

-- |Return the Set of characters from both of the given words.
charSet :: Text -> Text -> Set Char
charSet word1 word2 =
  toSet word1 `Set.union` toSet word2
  where toSet :: Text -> Set Char
        toSet = Set.fromList . T.unpack

-- |Return the list of all LetterMaps for the given set of characters.
letterMaps :: Set Char -> [LetterMap]
letterMaps chars = letterMaps' (Set.toList chars) Map.empty
  where letterMaps' :: [Char] -> LetterMap -> LetterMap
        letterMaps' chars lm = do
          l <- chars
          n <- [0..9]

lms :: [Char] -> [Int] -> [LetterMap]
lms chars ints = 
          

-- |Return the LetterMaps, if any, that, when used to sum the corresponding
-- integers of the first two words yield the corresponding integer of the third
-- word.
solve :: Text -> Text -> Text -> [LetterMap]
solve word1 word2 sum =
  let chars = charSet word1 word2
      lms = letterMaps chars
      solutions = [lm | lm <- lms, isSolution lm word1 word2 sum]
  in solutions
    
  --    num1 = wordToNum lm word1
  -- num2 = wordToNum lm word2
      
      
