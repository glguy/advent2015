module Day17 where

import Data.Array
import Data.List
import Data.Maybe

main :: IO ()
main =
  do input <- loadInput
     print (combinations input eggnog)
     print (minimumCombinations input eggnog)

loadInput :: IO [Int]
loadInput = map read . words <$> readFile "input17.txt"

eggnog :: Int
eggnog = 150

-- | Figure out how many combinations of the containers given
-- can add up to the initial amount using dynamic programming.
combinations ::
  [Int]   {- ^ container sizes -} ->
  Int     {- ^ initial amount  -} ->
  Integer {- ^ combinations    -}
combinations sizes initialAmount = foldl' addSize noContainers sizes ! initialAmount
  where
  -- make an array out of a list for some speedy indexing
  mkArray :: [a] -> Array Int a
  mkArray = listArray (0,initialAmount)

  -- having considered no containers so far there is one way to store 0
  -- liters of eggnog and no ways to store any more than that.
  noContainers = mkArray (1:repeat 0)

  -- prev: Each index in the array is the number of combinations that contain
  --       that much eggnog using the containers seen so far
  -- size: the next container to consider
  -- result: Each index in the array is the number of combinations that contain
  --       that much eggnog considering this new container, too
  addSize :: Array Int Integer -> Int -> Array Int Integer
  addSize prev size = mkArray
                  [ if amt < size then prev!amt
                                  else prev!amt + prev!(amt-size)
                  | amt <- [0..]
                  ]

-- | This approach is slower than the one above, but it runs fast enough
-- because the problem size is tiny and I didn't want to add the extra
-- parameter to the solution above.
minimumCombinations :: [Int] -> Int -> Int
minimumCombinations sizes amount
  = fromMaybe 0
  $ find (/= 0)
  $ map ways [0 .. length sizes]
  where
  ways i = sums amount i sizes

-- | Ways to sum @n@ distinct elements from a list to hit the given target
sums :: Int -> Int -> [Int] -> Int
sums target = aux 0
  where
  aux acc _ _ | acc > target = 0
  aux acc 0 _ | acc == target = 1
              | otherwise = 0
  aux acc n (x:xs) = aux (acc+x) (n-1) xs + aux acc n xs
  aux _   _ _      = 0
