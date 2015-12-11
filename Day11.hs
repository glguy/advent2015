module Main where

import Data.List

main :: IO ()
main =
  do key <- loadInput
     mapM_ putStrLn (take 2 (solutions key))

-- | Compute the list of valid passwords starting from a given one.
-- Note: This process works on reversed passwords with the rules
-- updated to work on reversed strings. This is to make 'nextPassword'
-- easier to write.
solutions :: String -> [String]
solutions = map reverse . filter isGoodPassword . iterate nextPassword . reverse

-- | Check that a string satisfies the descending and duplicate letter rules.
isGoodPassword :: String -> Bool
isGoodPassword p = hasPairs 2 p && hasDesc p

-- | Test that a string has at least @count@ non-overlapping double, adjacent
-- letters.
hasPairs :: Int {- ^ count -} -> String -> Bool
hasPairs 0 _  = True
hasPairs n (x:y:z)
  | x == y    = hasPairs (n-1) z
  | otherwise = hasPairs n (y:z)
hasPairs _ _  = False

-- | Test that a string has a 3-length descending sequence.
hasDesc :: String -> Bool
hasDesc = any aux . tails
  where
  aux (x:y:z:_) = x == succ y && y == succ z
  aux _         = False

-- | Load starting password from input file
loadInput :: IO String
loadInput = head . words <$> readFile "input11.txt"

-- | Test that a character is not in the set of @"iol"@
isGoodLetter :: Char -> Bool
isGoodLetter c = 'i' /= c && 'o' /= c && 'l' /= c

-- | Increment a string from left to right while skipping
-- the prohibited characters.
nextPassword :: String -> String
nextPassword []     = "a"
nextPassword (x:xs) =
  case x of
    'z' -> 'a' : nextPassword xs
    _ | isGoodLetter x' -> x' : xs
      | otherwise       -> nextPassword (x':xs)
  where
  x' = succ x
