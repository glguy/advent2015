module Main where

import Data.List

main :: IO ()
main =
  do step0 <- loadInput

     let step40 = iterate lookAndSay step0 !! 40
     print (length step40)

     let step50 = iterate lookAndSay step40 !! 10
     print (length step50)

loadInput :: IO String
loadInput = head . words <$> readFile "input10.txt"

lookAndSay :: String -> String
lookAndSay = foldr aux [] . group
  where
  aux xs = shows (length xs)
         . showChar (head xs)
