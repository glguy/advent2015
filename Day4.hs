module Main where

import Crypto.Hash
import Data.List
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main =
  do [key] <- B8.words <$> B8.readFile "input4.txt"
     print (solve 5 key)
     print (solve 6 key)

solve :: Int -> B8.ByteString -> Int
solve n key = search 1
  where
  ctx0 :: Context MD5
  ctx0 = hashUpdate hashInit key

  digest = show . hashFinalize . hashUpdate ctx0
         . B8.pack . show

  prefix = replicate n '0'

  search i
    | prefix `isPrefixOf` digest i = i
    | otherwise = search (i+1)
