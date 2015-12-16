module Main where

import Control.Monad
import Data.Binary.Get
import Data.Bits
import Data.Foldable
import Data.List
import Data.Monoid
import Data.Vector (Vector)
import Data.Word
import Data.Int
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Vector as Vector
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra
                (untrimmedStrategy, toLazyByteStringWith)

main :: IO ()
main =
  do print (solve 5)
     print (solve 6)

key :: String
key = "yzbqklnj"

solve :: Int64 -> Maybe Int
solve n = find (\i -> zeros n (md5 (L8.pack (key ++ show i)))) [1..]

-- | Test that the first @n@ digits in hex-representation of
-- the digest are @0@.
zeros :: Int64 -> L.ByteString -> Bool
zeros n bs = L.all (==0) (L.take n2 bs)
          && (even n || L.index bs n2 < 0x10)
  where
  n2 = n`quot`2

data Context = Context !Word32 !Word32 !Word32 !Word32

-- > md5 ""
-- d41d8cd98f00b204e9800998ecf8427e
--
-- > md5 "The quick brown fox jumps over the lazy dog."
-- e4d909c290d0fb1ca068ffaddf22cbd0
md5 :: L.ByteString -> L.ByteString
md5 = finish . foldl' addBlock initialState . toBlocks . envelope

-- | Extract the final MD5 digest from a context
finish :: Context -> L.ByteString
finish (Context a b c d)
  = toFixedByteString 16
  $ word32LE a <> word32LE b <> word32LE c <> word32LE d

-- | Pad out an input string to be suitable for breaking into
-- blocks for MD5
envelope :: L.ByteString -> L.ByteString
envelope xs = toLazyByteString
   $ lazyByteString xs
  <> word8          0x80 -- 0b10000000
  <> lazyByteString (L.replicate padLen 0)
  <> word64LE       (fromIntegral bitLen)
  where
  padLen   = (55 - L.length xs) `mod` 64
  bitLen   = 8 * L.length xs

-- | Break a bytestring with a length that is a multiple of 64
-- into blocks of 16 32-bit words loaded in little-endian order.
toBlocks :: L.ByteString -> [Vector Word32]
toBlocks
  = map       (Vector.fromList . runGet (replicateM 16 getWord32le))
  . takeWhile (not . L.null)
  . iterate   (L.drop 64)

addState :: Context -> Context -> Context
addState (Context a b c d) (Context w x y z) = Context (a+w) (b+x) (c+y) (d+z)

addBlock ::
  Context ->
  Vector Word32 {- ^ message chunk, 16 elements -} ->
  Context
addBlock st m = addState st (foldl' (doRound m) st rounds)

rounds :: [(Mixer, Int, Word32, Int)]
rounds = zip4 mixers stable ktable gtable

type Mixer = Word32 -> Word32 -> Word32 -> Word32

mixers :: [Mixer]
mixers = replicate 16 =<< [m1,m2,m3,m4]
  where
  m1 b c d = d `xor` (b .&. (c `xor` d))
  m2 b c d = c `xor` (d .&. (b `xor` c))
  m3 b c d = b `xor` c `xor` d
  m4 b c d = c `xor` (b .|. complement d)

doRound ::
  Vector Word32             {- ^ message chunk                       -} ->
  Context                   {- ^ incoming state                      -} ->
  (Mixer, Int, Word32, Int) {- ^ mixer, rotation, magic, chunk index -} ->
  Context
doRound m (Context a b c d) (mixer,s,k,g) = Context d (b + z) b c
  where
  f = mixer b c d
  y = a + f + k + m Vector.! g
  z = rotateL y s

toFixedByteString :: Int -> Builder -> L.ByteString
toFixedByteString n = toLazyByteStringWith (untrimmedStrategy n 0) L.empty

------------------------------------------------------------------------
-- Magic numbers
------------------------------------------------------------------------

stable :: [Int]
stable =
  [  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22
  ,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20
  ,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23
  ,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
  ]

ktable :: [Word32]
ktable =
  [ 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee
  , 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501
  , 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be
  , 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821
  , 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa
  , 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8
  , 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed
  , 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a
  , 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c
  , 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70
  , 0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05
  , 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665
  , 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039
  , 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1
  , 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1
  , 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
  ]

gtable :: [Int]
gtable =
  [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15
  ,  1,  6, 11,  0,  5, 10, 15,  4,  9, 14,  3,  8, 13,  2,  7, 12
  ,  5,  8, 11, 14,  1,  4,  7, 10, 13,  0,  3,  6,  9, 12, 15,  2
  ,  0,  7, 14,  5, 12,  3, 10,  1,  8, 15,  6, 13,  4, 11,  2,  9
  ]

initialState :: Context
initialState = Context 0x67452301 0xefcdab89 0x98badcfe 0x10325476
