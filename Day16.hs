module Main where

import Data.Char (isPunctuation)

main :: IO ()
main =
  do input <- loadInput
     print [lookup "Sue" props | props <- input, matchesClues1 props]
     print [lookup "Sue" props | props <- input, matchesClues2 props]

matchesClues1 :: [(String,Int)] -> Bool
matchesClues1 = all $ \(prop,memory) ->
  case lookup prop clues of
    Nothing           -> True
    Just mfcsam       -> mfcsam == memory

matchesClues2 :: [(String,Int)] -> Bool
matchesClues2 = all $ \(prop,memory) ->
  case lookup prop clues of
    Nothing           -> True
    Just mfcsam       ->
      case prop of
        "cats"        -> mfcsam <  memory
        "trees"       -> mfcsam <  memory
        "pomeranians" -> mfcsam >  memory
        "goldfish"    -> mfcsam >  memory
        _             -> mfcsam == memory

clues :: [(String,Int)]
clues = parseLine
  " children   : 3 \
  \ cats       : 7 \
  \ samoyeds   : 2 \
  \ pomeranians: 3 \
  \ akitas     : 0 \
  \ vizslas    : 0 \
  \ goldfish   : 5 \
  \ trees      : 3 \
  \ cars       : 2 \
  \ perfumes   : 1 "

loadInput :: IO [[(String,Int)]]
loadInput = map parseLine . lines <$> readFile "input16.txt"

parseLine :: String -> [(String,Int)]
parseLine = asProps . words'

-- | Words without punctuation
words' :: String -> [String]
words' = words . filter (not . isPunctuation)

asProps :: [String] -> [(String,Int)]
asProps []      = []
asProps (x:y:z) = (x,read y) : asProps z
asProps [_]     = error "props mismatched"
