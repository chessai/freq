{-# language BangPatterns #-}
{-# language NoImplicitPrelude #-}
{-# language ScopedTypeVariables #-}

{-# OPTIONS_GHC -O2 -Wall #-}

module Freq.Digram.Builder
  ( -- * Frequency table type
    FreqTable
    
    -- * Construction
  , tabulate
    
    -- * Using a trained model
  , probDigramTable
  , measureTable
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad.ST (ST,runST)
import Data.ByteString.Internal (ByteString(..))
import Data.Foldable
import Data.Map.Strict.Internal (Map)
import Data.Maybe (fromMaybe)
import Data.Primitive (ByteArray)
import Data.Set (Set)
import Data.Word (Word8)
import Freq.Digram.Internal
import GHC.Base hiding (empty)
import Prelude ((+), (-), (/), (*))

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Map.Strict as DMS
import qualified Data.Primitive as PM
import qualified Data.Set as S
import qualified GHC.OldList as L
import qualified Prelude as P

-- | A variant of 'Freq' that holds identical information but
--   is optimised for reads. There are no operations that
--   append additional information to a 'FreqTable'.
data FreqTable = FreqTable
  {-# UNPACK #-} !Int       -- ^ Width and height of square 2d array
  {-# UNPACK #-} !ByteArray -- ^ Square two-dimensional array of Double, maps first char and second char to probability
  {-# UNPACK #-} !ByteArray -- ^ Array of Word8, length 256, acts as map from Word8 to table row/column index

word8ToInt :: Word8 -> Int
word8ToInt = P.fromIntegral
{-# INLINE word8ToInt #-}

intToWord8 :: Int -> Word8
intToWord8 = P.fromIntegral
{-# INLINE intToWord8 #-}

probDigramTable ::
     FreqTable -- ^ Frequency table
  -> Word8     -- ^ First character
  -> Word8     -- ^ Second character
  -> Double
probDigramTable (FreqTable sz square ixs) chrFst chrSnd =
  let !ixFst = word8ToInt (PM.indexByteArray ixs (word8ToInt chrFst))
      !ixSnd = word8ToInt (PM.indexByteArray ixs (word8ToInt chrSnd))
   in PM.indexByteArray square (sz * ixFst + ixSnd)

measureTable :: FreqTable
             -> BC.ByteString
             -> Double
measureTable _ (PS _ _ 0) = 0
measureTable _ (PS _ _ 1) = 1
measureTable f !b         = go 0 0 / (P.fromIntegral (BC.length b - 1))
  where
    l :: Int
    !l = BC.length b - 1

    go :: Int -> Double -> Double
    go !p !acc
      | p == l = acc
      | otherwise =
          let k = BU.unsafeIndex b p
              r = BU.unsafeIndex b (p + 1)
          in go (p + 1) (probDigramTable f k r + acc)

-- | Optimise a 'Freq' for read access.
tabulate :: Freq -> FreqTable
tabulate (Freq m) = runST comp where
  comp :: forall s. ST s FreqTable
  comp = do
    let allChars :: Set Word8
        !allChars = S.union (DMS.keysSet m) (foldMap DMS.keysSet m)
        m' :: Map Word8 (Double, Map Word8 Double)
        !m' = fmap (\x -> (sum x, x)) m
        !sz = min (S.size allChars + 1) 256
        !szSq = sz * sz
        ixedChars :: [(Int,Word8)]
        !ixedChars = L.zip (P.enumFrom (0 :: Int)) (S.toList allChars)
    ixs <- PM.newByteArray 256
    square <- PM.newByteArray (szSq * PM.sizeOf (undefined :: Double))
    let fillSquare :: Int -> ST s ()
        fillSquare !i = if i < szSq
          then do
            PM.writeByteArray square i (0 :: Double)
            fillSquare (i + 1)
          else pure ()
    fillSquare 0
    PM.fillByteArray ixs 0 256 (intToWord8 sz)
    forM_ ixedChars $ \(ixFst,w8Fst) -> do
      PM.writeByteArray ixs ixFst w8Fst
      forM_ ixedChars $ \(ixSnd,w8Snd) -> do
        let r = fromMaybe 0 $ do
              (total, m'') <- DMS.lookup w8Fst m'
              v <- DMS.lookup w8Snd m''
              pure (v / total)
        PM.writeByteArray square (sz * ixFst + ixSnd) r
    frozenIxs <- PM.unsafeFreezeByteArray ixs
    frozenSquare <- PM.unsafeFreezeByteArray square
    pure (FreqTable sz frozenSquare frozenIxs)

