{-# Language BangPatterns #-}
module Main where

import Data.Char
import Data.Array

data Instr = Half String | Triple String | Increment String | Jmp Int | Jie String Int | Jio String Int
  deriving Show

main :: IO ()
main =
  do input <- loadInput
     print (run 0 0 (toArray input) 1)
     print (run 1 0 (toArray input) 1)

toArray :: [a] -> Array Int a
toArray xs = listArray (1,length xs) xs

parseLine :: String -> Instr
parseLine str =
  case words (filter (\x -> isAlphaNum x || x == '-' || x == ' ') str) of
    ["hlf",r]   -> Half r
    ["tpl",r]   -> Triple r
    ["inc",r]   -> Increment r
    ["jmp",o]   -> Jmp (read o)
    ["jie",r,o] -> Jie r (read o)
    ["jio",r,o] -> Jio r (read o)
    _ -> error str

loadInput :: IO [Instr]
loadInput = map parseLine . lines <$> readFile "input23.txt"

run :: Integer -> Integer -> Array Int Instr -> Int -> Integer
run !a !b program pc
  | not (inRange (bounds program) pc) = b
  | otherwise =
      case program ! pc of
        Half      "a" -> run (a`quot`2) b program (pc+1)
        Half      "b" -> run a (b`quot`2) program (pc+1)
        Triple    "a" -> run (3*a) b program (pc+1)
        Triple    "b" -> run a (3*b) program (pc+1)
        Increment "a" -> run (a+1) b program (pc+1)
        Increment "b" -> run a (b+1) program (pc+1)
        Jmp o -> run a b program (pc+o)
        Jie "a" o | even a -> run a b program (pc+o)
        Jie "b" o | even b -> run a b program (pc+o)
        Jie _   _ -> run a b program (pc+1)
        Jio "a" o | a == 1 -> run a b program (pc+o)
        Jio "b" o | b == 1 -> run a b program (pc+o)
        Jio _   _ -> run a b program (pc+1)
        i -> error (show i)
