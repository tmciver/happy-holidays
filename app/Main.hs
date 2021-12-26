{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude

import Lib

main :: IO ()
main = do
  let solns = solve "happy" "holidays" "hohohoho"
  print (show solns :: Text)
