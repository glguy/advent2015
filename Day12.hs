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
     print (sumOfNonredNumbers input)

-- | Sum of all numbers in a JSON value.
sumOfNumbers :: Value -> Scientific
sumOfNumbers = sumOf (deep _Number)

-- | Sum of all numbers in a JSON value after
-- pruning out portions that fail the 'noRed' test.
sumOfNonredNumbers :: Value -> Scientific
sumOfNonredNumbers = sumOf (deepOf (plate . filtered noRed) _Number)

-- | Returns True for all Values that are not Objects
-- with any @"red"@ values.
noRed :: Value -> Bool
noRed = notElemOf (_Object . folded) "red"

-- | Load the input file as a JSON value.
loadInput :: IO Value
loadInput = (^?! _Value) <$> readFile "input12.txt"

