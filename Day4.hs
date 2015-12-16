module Main where

import Control.Monad                 (replicateM)
import Data.Binary.Get               (runGet, getWord32le)
import Data.Bits                     ((.|.), (.&.), complement, rotateL, xor)
import Data.ByteString.Builder       (Builder, toLazyByteString, lazyByteString, word8, word32LE, word64LE)
import Data.ByteString.Builder.Extra (untrimmedStrategy, toLazyByteStringWith)
import Data.Int                      (Int64)
import Data.List                     (find, foldl', zipWith4)
import Data.Monoid                   ((<>))
import Data.Vector                   (Vector)
import Data.Word                     (Word32)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Vector as Vector

main :: IO ()
main =
  do [key] <- words <$> readFile "input4.txt"
     print (solve key 5)
     print (solve key 6)

-- | Find the smallest, positive integer that has the specified
-- number of leading zeros in its hex representation.
solve :: String -> Int64 -> Maybe Int
solve key n = find (zeros n . adventHash key) [1..]

-- | The "advent hash" of a number is the MD5 digest of a key string
-- and a ASCII, base-10 representation of the number.
adventHash ::
  String  {- ^ player key -} ->
  Int     {- ^ number to hash -} ->
  L.ByteString
adventHash key i = md5 (L8.pack (key ++ show i))

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
-- blocks for MD5. This algorithm pads with a @1@ and then
-- as many @0@ bytes as needed so that when the 8-byte length
-- is added that the whole message's length is a multiple of
-- 64-bytes.
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

-- | Point-wise addition of the components of a 'Context'
addState :: Context -> Context -> Context
addState (Context a b c d) (Context w x y z) = Context (a+w) (b+x) (c+y) (d+z)

addBlock ::
  Context ->
  Vector Word32 {- ^ message chunk, 16 elements -} ->
  Context
addBlock st m = addState st (foldl' (doRound m) st rounds)

data Round = Round Mixer !Int !Word32 !Int

-- | Each element of this list corresponds to one of the 64 rounds require
-- when incorporating a block of message into the MD5 context.
rounds :: [Round]
rounds = zipWith4 Round mixers stable ktable gtable

doRound ::
  Vector Word32 {- ^ message chunk                       -} ->
  Context       {- ^ incoming state                      -} ->
  Round         {- ^ mixer, rotation, magic, chunk index -} ->
  Context
doRound m (Context a b c d) (Round mixer s k g) = Context d (b + z) b c
  where
  f = mixer b c d
  y = a + f + k + m Vector.! g
  z = rotateL y s

type Mixer = Word32 -> Word32 -> Word32 -> Word32

mixers :: [Mixer]
mixers = replicate 16 =<< [m1,m2,m3,m4]
  where
  m1 b c d = d `xor` (b .&. (c `xor` d))
  m2 b c d = c `xor` (d .&. (b `xor` c))
  m3 b c d = b `xor` c `xor` d
  m4 b c d = c `xor` (b .|. complement d)

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
