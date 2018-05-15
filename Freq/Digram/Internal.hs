{-# language BangPatterns #-}
{-# language MagicHash    #-}
{-# language NoImplicitPrelude #-}
{-# language ScopedTypeVariables #-}

{-# OPTIONS_GHC -O2 -Wall #-}

module Freq.Digram.Internal
  ( -- * Frequency table type
    Freq(..)
    
    -- * Construction
  , empty 
  , singleton
    
    -- * Training
  , tally
  , createWith
  , createWithMany
  , defWeight
    
    -- * Using a trained model
  , probDigram 
  , measure

    -- * Pretty Printing
  , prettyFreq
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad ((>>))
import Data.ByteString.Internal (ByteString(..), w2c)
import Data.Foldable
import Data.Map.Strict.Internal (Map)
import Data.Monoid
import Data.Semigroup
import Data.Word (Word8)
import GHC.Base hiding (empty)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Map.Strict as DMS
import qualified Prelude as P
import Prelude (FilePath, (+), (-), (/))

-- | A 'Freq' is a digram-based frequency table.
--   @'mappend == <>'@ will add the values of each
--   of the matching keys.
newtype Freq = Freq ( Map Word8 (Map Word8 Double) )

instance Semigroup Freq where
  {-# INLINE (<>) #-} 
  (Freq a) <> (Freq b) = Freq $ union a b

instance Monoid Freq where
  {-# INLINE mempty #-} 
  mempty  = empty
  {-# INLINE mappend #-} 
  (Freq a) `mappend` (Freq b) = Freq $ union a b

-- | Pretty-print a Frequency table.
prettyFreq :: Freq -> IO ()
prettyFreq (Freq m)
  = DMS.foldMapWithKey
      (\c1 m' ->
         P.putStrLn (if c1 == 10 then "\\n" else [w2c c1])
           >> DMS.foldMapWithKey
               (\c2 prob -> P.putStrLn ("  " ++ [w2c c2] ++ " " ++ P.show (P.round prob :: Int))) m') m

-- | /O(1)/. The empty frequency table.
empty :: Freq
empty = Freq DMS.empty
{-# INLINE empty #-}

-- | /O(1)/. A Frequency table with a single entry.
singleton :: Word8  -- ^ Outer key
          -> Word8  -- ^ Inner key
          -> Double -- ^ Weight
          -> Freq   -- ^ The singleton frequency table
singleton k ka w = Freq $ DMS.singleton k (DMS.singleton ka w)
{-# INLINE singleton #-}

-- | Given a Frequency table and a ByteString, 'measure'
--   returns the probability that the ByteString is not
--   randomised. A higher probability means that it is 
--   it is less random, while a lower probability indicates
--   a higher degree of randomness. This is heavily affected
--   by your training data.
measure :: Freq          -- ^ Frequency table
        -> BC.ByteString -- ^ ByteString in question
        -> Double        -- ^ Probability that the ByteString is not randomised
measure _ (PS _ _ 0) = 0
measure _ (PS _ _ 1) = 0
measure f !b         = (go 0 0) / (P.fromIntegral (BC.length b - 1))
  where
    l :: Int
    l = BC.length b - 1

    go :: Int -> Double -> Double
    go !p !acc
      | p == l = acc
      | otherwise =
          let k = BU.unsafeIndex b p
              r = BU.unsafeIndex b (p + 1)
          in go (p + 1) (probDigram f k r + acc)
{-# INLINE measure #-}

-- | Given a Frequency table and characters 'c1' and 'c2',
--   what is the probability that 'c1' follows 'c2'?
probDigram :: Freq   -- ^ Frequency table
           -> Word8  -- ^ Char 1
           -> Word8  -- ^ Char 2
           -> Double -- ^ Probability that Char 2 follows Char 1 
probDigram (Freq f) w1 w2 =
  case DMS.lookup w1 f of
    Nothing -> 0
    Just g ->
      case DMS.lookup w2 g of
        Nothing -> 0
        Just weight -> ratio weight g
{-# INLINE probDigram #-}

-- | Build a Frequency table from data contained within multiple files inside of the IO monad.
createWithMany :: [FilePath] -- ^ List of filepaths containing training data
               -> IO Freq    -- ^ Frequency table generated as a result of training, inside of IO.
createWithMany !paths = foldMap createWith paths
{-# INLINE createWithMany #-}

-- | Build a Frequency table inside of the IO monad.
createWith :: FilePath -- ^ Filepath containing training data
           -> IO Freq  -- ^ Frequency table generated as a result of training, inside of IO.
createWith !path = BC.readFile path >>= (pure . tally)
{-# INLINE createWith #-}

-- | Build a frequency table from a ByteString.
tally :: BC.ByteString -- ^ ByteString with which the Freq will be built
      -> Freq          -- ^ Resulting Freq
tally (PS _ _ 0) = empty
tally !b = go 0 mempty
  where
    l :: Int
    l = BC.length b - 1

    go :: Int -> Freq -> Freq
    go !p !fr
      | p == l = fr
      | otherwise =
          let k = BU.unsafeIndex b p
              r = BU.unsafeIndex b (p + 1)
          in go (p + 1) (mappend (singleton k r 1) fr)

-- | Default weight associated with each character.
--   This is just @1.0@.
defWeight :: Double
defWeight = 1.0
{-# INLINE defWeight #-}

{--------------------------------------------------------------------
  Internal Section
--------------------------------------------------------------------}

ratio :: Double -> Map Word8 Double -> Double
ratio !weight g = weight / (sum g)
{-# INLINE ratio #-}

-- A convenience type synonym for internal use.
-- Please /do not/ ask me to rename this to "Tally".
-- https://github.com/andrewthad did this.
-- He is no longer permitted to leave the cave.
--
-- Tal is named after Mikhail Tal.
type Tal = Map Word8 (Map Word8 Double)

-- union two 'Tal', summing the weights belonging to the same keys.
union :: Tal -> Tal -> Tal
union a b = DMS.unionWith (DMS.unionWith (+)) a b
{-# INLINE union #-}
