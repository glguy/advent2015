{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Scientific

main :: IO ()
main =
  do input <- loadInput
     print (sumOfNumbers input)

     let pruned = transform trim input
     print (sumOfNumbers pruned)

sumOfNumbers :: Value -> Scientific
sumOfNumbers = sumOf (deep _Number)

trim :: Value -> Value
trim v
  | elemOf (_Object . folded . _String) "red" v = _Number # 0
  | otherwise = v

loadInput :: IO Value
loadInput = (^?! _Value) <$> readFile "input12.txt"

