module Main where

import qualified Data.Map as Map
import Data.Map ( Map )
import Data.List

data Edge = Edge String String    deriving (Eq, Ord, Show, Read)

edge :: String -> String -> Edge
edge x y
  | x < y     = Edge x y
  | otherwise = Edge y x

edgeParts :: Edge -> [String]
edgeParts (Edge x y) = [x,y]

main :: IO ()
main =
  do input <- loadInput
     let places = uniques (concatMap edgeParts (Map.keys input))
         costs  = tripLength input <$> permutations places
     print (minimum costs)
     print (maximum costs)

loadInput :: IO (Map Edge Int)
loadInput = Map.fromList . map parse . lines <$> readFile "input9.txt"

parse :: String -> (Edge, Int)
parse ln =
  case words ln of
    [x,"to",y,"=",z] -> (edge x y,read z)
    _                -> error ("Bad line: " ++ ln)

tripLength :: Map Edge Int -> [String] -> Int
tripLength m xs
  = sum
  $ map (m Map.!)
  $ zipWith edge xs (tail xs)

uniques :: Ord a => [a] -> [a]
uniques = map head . group . sort
