module Main where

import Data.List

main :: IO ()
main =
  do let step40 = iterate step key !! 40
     print (length step40)

     let step50 = iterate step step40 !! 10
     print (length step50)

key :: String
key = "1113222113"

step :: String -> String
step = foldr aux [] . group
  where
  aux xs = shows (length xs)
         . showChar (head xs)
