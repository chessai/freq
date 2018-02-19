{-# LANGUAGE BangPatterns #-}

import Data.Bits (shiftR)
import Data.ByteString.Internal (ByteString(..))

import Numeric.LinearAlgebra 

n = 1 / (sqrt 2) :: Double

hadamard :: Int -> Matrix Double
hadamard n
  | n <= 1 = hIdent
  | otherwise = hadamard 1 `kronecker` hadamard (n - 1)

hIdent :: Matrix Double
hIdent = (2><2) [  n,  n
                ,  n, -n ]

log2 :: Int -> Int
log2 x = go (-1) x
  where
    go !p q
      | q <= 0 = p
      | otherwise = go (p + 1) (q `shiftR` 1)

f :: Int -> Int
f x
  | x < 0     = -1
  | otherwise =  1
