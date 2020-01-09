module Main where

import Data.Array.Unboxed (UArray, assocs, accumArray)
import Data.List (find)

main :: IO ()
main =
  do print (findHouse solve1)
     print (findHouse solve2)

findHouse :: UArray Int Int -> Maybe Int
findHouse = fmap fst . find (\x -> snd x >= target) . assocs

target :: Int
target = 36000000

solve1 :: UArray Int Int
solve1 =
  let top = target `quot` 10 in
  accumArray (+) 0 (1,top)
   [ (house, elf * 10)
   | elf   <- [1..top]
   , house <- [elf, elf+elf .. top]
   ]

solve2 :: UArray Int Int
solve2 =
  let top = target `quot` 11 in
  accumArray (+) 0 (1,top)
    [ (house, elf*11)
    | elf   <- [1..top]
    , house <- [elf, elf+elf .. min top (elf*50)]
    ]
