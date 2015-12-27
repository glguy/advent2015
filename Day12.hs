{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.Foldable
import Data.Scientific
import qualified Data.ByteString as B

main :: IO ()
main =
  do input <- loadInput
     print (sumOfNumbers input)
     print (sumOfNonredNumbers input)

-- | Sum of all numbers in a JSON value.
sumOfNumbers :: Value -> Scientific
sumOfNumbers = sumOfNumbers' 0

-- | Sum of all numbers in a JSON value after
-- pruning out portions that fail the 'noRed' test.
sumOfNonredNumbers :: Value -> Scientific
sumOfNonredNumbers = sumOfNonredNumbers' 0

-- | Load the input file as a JSON value.
loadInput :: IO Value
loadInput =
  do contents <- B.readFile "input12.txt"
     case decodeStrict' contents of
       Just v -> return v
       Nothing -> fail "Bad JSON document"

sumOfNumbers' :: Scientific -> Value -> Scientific
sumOfNumbers' acc v =
  case v of
    Number n -> acc+n
    Object o -> foldl' sumOfNumbers' acc o
    Array a  -> foldl' sumOfNumbers' acc a
    _        -> acc

sumOfNonredNumbers' :: Scientific -> Value -> Scientific
sumOfNonredNumbers' acc v =
  case v of
    Number n -> acc+n
    Object o | "red" `notElem` o -> foldl' sumOfNonredNumbers' acc o
    Array a  -> foldl' sumOfNonredNumbers' acc a
    _        -> acc
