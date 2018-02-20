{-# language BangPatterns #-}
{-# language MagicHash    #-}
{-# language NoImplicitPrelude #-}

{-# OPTIONS_GHC -O2 -Wall #-}

module Freq 
  ( -- * Frequency table type
    Freq(..)
    
    -- * Construction
  , empty 
  , singleton
    
    -- * Training
  , tally
  , tallyWeighted
  , create
  , createMany
  , defWeight
    
    -- * Using a trained model
  , probDigram 
  , measure
  , measureWeighted
  , maxDefProb

    -- * Pretty Printing
  , prettyFreq
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad ((>>))
import Data.ByteString.Internal (ByteString(..), w2c)
import Data.Foldable
import Data.Map.Strict.Internal (Map(..))
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
newtype Freq = Freq { freq :: Map Word8 (Map Word8 Double) }

instance Semigroup Freq where
  {-# INLINE (<>) #-} 
  (Freq a) <> (Freq b) = Freq $ union a b

instance Monoid Freq where
  {-# INLINE mempty #-} 
  mempty  = empty
  {-# INLINE mappend #-} 
  (Freq a) `mappend` (Freq b) = Freq $ union a b

-- | pretty-print a Frequency table.
prettyFreq :: Freq -> IO ()
prettyFreq (Freq m) = DMS.foldMapWithKey (\c1 m' -> P.putStrLn (if c1 == 10 then "\\n" else [w2c c1]) >> DMS.foldMapWithKey (\c2 prob -> P.putStrLn ("  " ++ [w2c c2] ++ " " ++ P.show (P.round prob :: Int))) m') m

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
--   For a version that lets you specify a maximum probability
--   that can be returned from this operation, see 'measureWeighted'.
measure :: Freq -> BC.ByteString -> Double
measure f !b = measureWeighted f b maxDefProb
{-# INLINE measure #-}

-- | Calculates the probability that a given ByteString is
--   nonrandom given a Frequency table. As an additional
--   argument, 'measureWeighted' lets you specify a maximum
--   probability that this function can return for a given
--   ByteString. For a version that uses a default maximum
--   probability, see 'measure'.
measureWeighted :: Freq -> BC.ByteString -> Double -> Double
measureWeighted _ (PS _ _ 0) _ = 0
measureWeighted _ (PS _ _ 1) _ = 0
measureWeighted f !b !prob = (go 0 0) / (P.fromIntegral (BC.length b - 1))
  where
    l :: Int
    l = BC.length b - 1

    go :: Int -> Double -> Double
    go !p !acc
      | p == l = acc
      | otherwise =
          let k = BU.unsafeIndex b p
              r = BU.unsafeIndex b (p + 1)
          in go (p + 1) (probDigram f k r prob + acc)
{-# INLINE measureWeighted #-}

-- | Given a Frequency table and characters 'c1' and 'c2',
--   what is the probability that 'c1' follows 'c2'?
--   'probDigram' allows you to specify a maximum probability
--   that this function can return.
probDigram :: Freq -> Word8 -> Word8 -> Double -> Double 
probDigram (Freq f) w1 w2 p =
  case DMS.lookup w1 f of
    Nothing -> 0
    Just g ->
      case DMS.lookup w2 g of
        Nothing -> 0
        Just weight -> ratio p weight g
{-# INLINE probDigram #-}

-- | Build multiple Frequency tables inside of the IO monad.
createMany :: [FilePath] -> IO Freq
createMany !paths = foldMapA create paths
{-# INLINE createMany #-}

-- | Build a Frequency table inside of the IO monad.
create :: FilePath -> IO Freq
create !path = do
  text <- BC.readFile path
  pure $ tally text
{-# INLINE create #-}

-- | Build a frequency table from a ByteString.
--   As a first argument it takes a weight, that it
--   assigns to character. For a version that uses
--   a default weight, see 'tally'.
tallyWeighted :: Double -> BC.ByteString -> Freq
tallyWeighted _ (PS _ _ 0) = empty
tallyWeighted !w !b = go 0 mempty
  where
    l :: Int
    l = BC.length b - 1

    go :: Int -> Freq -> Freq
    go !p !fr
      | p == l = fr
      | otherwise =
          let k = BU.unsafeIndex b p
              r = BU.unsafeIndex b (p + 1)
          in go (p + 1) (mappend (singleton k r w) fr)
{-# INLINE tallyWeighted #-}

-- | Build a frequency table from a ByteString.
--   For a version that lets you specify a weight to
--   assign to each character, see 'tallyWeighted'.
tally :: BC.ByteString -> Freq
tally !b = tallyWeighted defWeight b
{-# INLINE tally #-}

-- | Default weight associated with each character.
--   This is just @1.0@.
defWeight :: Double
defWeight = 1.0
{-# INLINE defWeight #-}

-- | Default maximum probability that a digram can have as probable.
--   This is just @1.0@.
maxDefProb :: Double
maxDefProb = 1.0
{-# INLINE maxDefProb #-}

{--------------------------------------------------------------------
  Internal Section
--------------------------------------------------------------------}

ratio :: Double -> Double -> Map Word8 Double -> Double
ratio !p !weight g = P.min p (weight / sum g)
{-# INLINE ratio #-}

-- A convenience type synonym for internal use.
-- Please /do not/ ask me to rename this to "Tally".
-- It is named after Mikhail Tal.
type Tal = Map Word8 (Map Word8 Double)

-- union two 'Tal', summing the weights belonging to the same keys.
union :: Tal -> Tal -> Tal
union a b = DMS.unionWith (DMS.unionWith (+)) a b
{-# INLINE union #-}

newtype Ap f a = Ap { getAp :: f a }

instance (Applicative f, Monoid a) => Monoid (Ap f a) where
  {-# INLINE mempty #-} 
  mempty = Ap $ pure mempty
  {-# INLINE mappend #-} 
  mappend (Ap x) (Ap y) = Ap $ liftA2 mappend x y

foldMapA :: (Foldable t, Monoid m, Applicative f) => (a -> f m) -> t a -> f m
foldMapA f = getAp . foldMap (Ap . f)
