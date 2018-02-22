{-# language BangPatterns #-}
{-# language MagicHash    #-}
{-# language NoImplicitPrelude #-}
{-# language ScopedTypeVariables #-}

{-# OPTIONS_GHC -O2 -Wall #-}

module Data.Freq 
  ( -- * Frequency table type
    Freq
  , FreqTable
    
    -- * Construction
  , empty 
  , singleton
  , tabulate
    
    -- * Training
  , tally
  , tallyWeighted
  , createWith
  , createWithMany
  , defWeight
    
    -- * Using a trained model
  , probDigram 
  , probDigramTable
  , measure

    -- * Pretty Printing
  , prettyFreq
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad ((>>))
import Control.Monad.ST (ST,runST)
import Data.ByteString.Internal (ByteString(..), w2c)
import Data.Foldable
import Data.Map.Strict.Internal (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Primitive (ByteArray)
import Data.Semigroup
import Data.Set (Set)
import Data.Word (Word8)
import GHC.Base hiding (empty)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Map.Strict as DMS
import qualified Data.Primitive as PM
import qualified Data.Set as S
import qualified GHC.OldList as L
import qualified Prelude as P
import Prelude (FilePath, (+), (-), (/), (*))

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

-- | Pretty-print a Frequency table.
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
--   As a first argument it takes a weight, that it
--   assigns to character. For a version that uses
--   a default weight, see 'tally'.
tallyWeighted :: Double        -- ^ Maximum weight to assign a single Char
              -> BC.ByteString -- ^ ByteString with which the Frequency table will be built
              -> Freq          -- ^ Resulting Frequency table.
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
tally :: BC.ByteString -- ^ ByteString with which the Frequency table will be built.
      -> Freq          -- ^ Resulting Frequency table.
tally !b = tallyWeighted defWeight b
{-# INLINE tally #-}

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
-- It is named after Mikhail Tal.
type Tal = Map Word8 (Map Word8 Double)

-- union two 'Tal', summing the weights belonging to the same keys.
union :: Tal -> Tal -> Tal
union a b = DMS.unionWith (DMS.unionWith (+)) a b
{-# INLINE union #-}
