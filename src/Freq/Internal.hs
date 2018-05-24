--------------------------------------------------------------------------------

{-# language BangPatterns #-}
{-# language MagicHash    #-}
{-# language NoImplicitPrelude #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}
{-# language TypeFamilies #-}

{-# OPTIONS_GHC -O2 -Wall #-}

--------------------------------------------------------------------------------

{-| This is the internal module to 'Freq'.
    The primary differences are that this
    module exports the typeclass 'Freaky',
    as well as the data constructors of
    'FreqTrain' and 'Freq'.
-}

module Freq.Internal
  ( -- * Frequency table type
    FreqTrain(..)

    -- * Construction
  , empty 
  , singleton
  
  , tabulate

    -- * Training
  , train 
  , trainWith
  , trainWithMany

    -- * Using a trained model
  , Freq(..)
  , measure
  , Freaky(prob)

    -- * Pretty Printing
  , prettyFreqTrain
  ) where

--------------------------------------------------------------------------------

import Control.Applicative (Applicative(..))
import Control.Monad ((>>))
import Control.Monad.ST (ST,runST)
import Data.ByteString.Internal (ByteString(..), w2c)
import Data.Foldable
import Data.Map.Strict.Internal (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Primitive.ByteArray (ByteArray)
import Data.Semigroup
import Data.Set (Set)
import Data.Word (Word8)
import GHC.Base hiding (empty)
import Prelude (FilePath, (+), (*), (-), (/), show, mod)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Map.Strict as DMS
import qualified Data.Primitive.ByteArray as PM
import qualified Data.Primitive.Types as PM
import qualified Data.Set as S
import qualified GHC.OldList as L
import qualified Numeric as Numeric
import qualified Prelude as P

--------------------------------------------------------------------------------

-- | @'Freaky'@ is a typeclass that wraps the @'prob'@ function,
--   which allows for an extensible definition of @'measure'@.
--
--   It is used internally.
class Freaky a where
  -- | Given a Frequency table and characters 'c1' and 'c2',
  --   what is the probability that 'c1' follows 'c2'?
  prob :: a -> Word8 -> Word8 -> Double

-- | Given a Frequency table and a @'ByteString'@, @'measure'@
--   returns the probability that the @'ByteString'@ is not
--   randomised. The accuracy of @'measure'@ is is heavily affected
--   by your training data.
measure :: Freaky a => a -> BC.ByteString -> Double
measure _ (PS _ _ 0) = 0
measure _ (PS _ _ 1) = 0
measure f !b         = (go 0 0) / (P.fromIntegral l)
  where
    l :: Int
    l = BC.length b - 1

    go :: Int -> Double -> Double
    go !p !acc
      | p == l = acc
      | otherwise =
          let k = BU.unsafeIndex b p
              r = BU.unsafeIndex b (p + 1)
          in go (p + 1) (prob f k r + acc)
{-# INLINE measure #-}

--------------------------------------------------------------------------------

-- | A @'FreqTrain'@ is a digram-based frequency table.
--   
--   One can construct a @'FreqTrain'@ with @'train'@,
--   @'trainWith'@, or @'trainWithMany'@.
--
--   One can use a trained @'FreqTrain'@ with @'prob'@
--   and @'measure'@.
--   
--   @'mappend' == '<>'@ will add the values of each
--   of the matching keys.
--
--   It is highly recommended to convert a @'FreqTrain'@
--   to a @'Freq'@ with @'tabulate'@ before using the trained model,
--   because @'Freq'@s have /O(1)/ reads as well as significantly
--   faster constant-time operations, however keep in mind
--   that @'Freq'@s cannot be neither modified nor converted
--   back to a @'FreqTrain'@.
--
newtype FreqTrain = FreqTrain
  { _getFreqTrain :: Map Word8 (Map Word8 Double) }

instance Freaky FreqTrain where
  prob (FreqTrain f) w1 w2 =
    case DMS.lookup w1 f of
      Nothing -> 0
      Just g -> case DMS.lookup w2 g of
        Nothing -> 0
        Just weight -> ratio weight g
  {-# INLINE prob #-}

instance Semigroup FreqTrain where
  {-# INLINE (<>) #-} 
  (FreqTrain a) <> (FreqTrain b) = FreqTrain $ union a b

instance Monoid FreqTrain where
  {-# INLINE mempty #-} 
  mempty  = empty
  {-# INLINE mappend #-} 
  (FreqTrain a) `mappend` (FreqTrain b) = FreqTrain $ union a b

--------------------------------------------------------------------------------

-- | /O(1)/. The empty frequency table.
empty :: FreqTrain
empty = FreqTrain DMS.empty
{-# INLINE empty #-}

-- | /O(1)/. A Frequency table with a single entry.
singleton :: Word8  -- ^ Outer key
          -> Word8  -- ^ Inner key
          -> Double -- ^ Weight
          -> FreqTrain   -- ^ The singleton frequency table
singleton k ka w = FreqTrain $ DMS.singleton k (DMS.singleton ka w)
{-# INLINE singleton #-}

-- | Optimise a 'FreqTrain' for /O(1)/ read access.
tabulate :: FreqTrain -> Freq
tabulate = tabulateInternal
{-# INLINE tabulate #-}
 
--------------------------------------------------------------------------------

-- | Given a @'ByteString'@ consisting of training data,
--   build a Frequency table.
train :: BC.ByteString
      -> FreqTrain
train !b = tally b
{-# INLINE train #-}

-- | Given a @'FilePath'@ containing training data, build a
--   Frequency table inside of the @'IO'@ monad.
trainWith :: FilePath -- ^ @'FilePath'@ containing training data
          -> IO FreqTrain  -- ^ Frequency table generated as a result of training, inside of @'IO'@.
trainWith !path = BC.readFile path >>= (pure . tally)
{-# INLINE trainWith #-}

-- | Given a list of @'FilePath'@ containing training data,
--   build a Frequency table inside of the @'IO'@ monad.
trainWithMany :: Foldable t
              => t FilePath -- ^ @'FilePath'@s containing training data
              -> IO FreqTrain    -- ^ Frequency table generated as a result of training, inside of @'IO'@.
trainWithMany !paths = foldMap trainWith paths
{-# INLINE trainWithMany #-}

--------------------------------------------------------------------------------

-- | Pretty-print a @'FreqTrain'@.
--
prettyFreqTrain :: FreqTrain -> IO ()
prettyFreqTrain (FreqTrain m)
  = DMS.foldMapWithKey
      (\c1 m' ->
         P.putStrLn (if c1 == 10 then "\\n" else [w2c c1])
           >> DMS.foldMapWithKey
               (\c2 prb -> P.putStrLn ("  " ++ [w2c c2] ++ " " ++ P.show (P.round prb :: Int))) m') m

--------------------------------------------------------------------------------

-- | A variant of @'FreqTrain'@ that holds identical information but
--   is optimised for reads. There are no operations that imbue
--   a @'Freq'@ with additional information.
--
--   Reading from a @'Freq'@ is orders of magnitude faster
--   than reading from a @'FreqTrain'@. It is /highly/
--   recommended that you use your trained model by first
--   converting a @'FreqTrain'@ to a @'Freq'@ with @'tabulate'@.
data Freq = Freq
  { _Dim :: !Int
    -- ^ Width and height of square 2d array
  , _2d  :: !ByteArray
    -- ^ Square two-dimensional array of Double, maps first char and second char to probability
  , _Flat :: !ByteArray
    -- ^ Array of Word8, length 256, acts as map from Word8 to table row/column index
  }

instance Freaky Freq where
  {-# INLINE prob #-} 
  prob (Freq sz square ixs) chrFst chrSnd =
     let !ixFst = word8ToInt (PM.indexByteArray ixs (word8ToInt chrFst))
         !ixSnd = word8ToInt (PM.indexByteArray ixs (word8ToInt chrSnd))
     in PM.indexByteArray square (sz * ixFst + ixSnd)

-- This exists for debugging purposes
instance P.Show Freq where
  show (Freq i arr ixs) =
      P.show i ++ "x" ++ show i
   ++ "\n"
   ++ "\n2D Array: \n"
   ++ go 0
   ++ "\n256 Array: \n"
   ++ ho 0
    where
      ho :: Int -> String
      ho !ix = if ix < PM.sizeofByteArray ixs
        then
          let col = ix `mod` 16
              extra = if col == 15 then "\n" else ""
          in show (PM.indexByteArray ixs ix :: Word8) ++ " " ++ extra ++ ho (ix + 1)
        else ""
      
      go :: Int -> String
      go !ix = if ix < elemSz
        then
          let col = ix `mod` i
              extra = if col == (i - 1) then "\n" else "" 
          in showFloat (PM.indexByteArray arr ix :: Double) ++ " " ++ extra ++ go (ix + 1) 
        else ""
        where
          !elemSz = P.div (PM.sizeofByteArray arr) (PM.sizeOf (undefined :: Double))
          showFloat :: P.RealFloat a => a -> String
          showFloat !x = Numeric.showFFloat (Just 2) x ""

--------------------------------------------------------------------
--  Internal Section                                              --
--------------------------------------------------------------------

word8ToInt :: Word8 -> Int
word8ToInt !w = P.fromIntegral w
{-# INLINE word8ToInt #-}

intToWord8 :: Int -> Word8
intToWord8 !i = P.fromIntegral i
{-# INLINE intToWord8 #-}

-- | Optimise a 'FreqTrain' for /O(1)/ read access.
--
tabulateInternal :: FreqTrain -> Freq
tabulateInternal (FreqTrain m) = runST comp where
  comp :: forall s. ST s Freq
  comp = do
    let allChars :: Set Word8
        !allChars = S.union (DMS.keysSet m) (foldMap DMS.keysSet m)
        m' :: Map Word8 (Double, Map Word8 Double)
        !m' = fmap (\x -> (sum x, x)) m
        !sz = min (S.size allChars + 1) 256
        !szSq = sz * sz
        ixedChars :: [(Word8,Word8)]
        !ixedChars = L.zip (P.enumFrom (0 :: Word8)) (S.toList allChars)
    ixs <- PM.newByteArray 256
    square <- PM.newByteArray (szSq * PM.sizeOf (undefined :: Double))
    let fillSquare :: Int -> ST s ()
        fillSquare !i = if i < szSq
          then do
            PM.writeByteArray square i (0 :: Double)
            fillSquare (i + 1)
          else pure ()
    fillSquare 0
    PM.fillByteArray ixs 0 256 (intToWord8 (sz - 1))
    forM_ ixedChars $ \(ixFst,w8Fst) -> do
      PM.writeByteArray ixs (word8ToInt w8Fst) ixFst --w8Fst
      forM_ ixedChars $ \(ixSnd,w8Snd) -> do
        let r = fromMaybe 0 $ do
              (total, m'') <- DMS.lookup w8Fst m'
              v <- DMS.lookup w8Snd m''
              pure (v / total)
        PM.writeByteArray square (sz * (word8ToInt ixFst) + (word8ToInt ixSnd)) r
    frozenIxs <- PM.unsafeFreezeByteArray ixs
    frozenSquare <- PM.unsafeFreezeByteArray square
    pure (Freq sz frozenSquare frozenIxs)

-- | Build a frequency table from a ByteString.
tally :: BC.ByteString -- ^ ByteString with which the FreqTrain will be built
      -> FreqTrain          -- ^ Resulting FreqTrain
tally (PS _ _ 0) = empty
tally !b = go 0 mempty
  where
    l :: Int
    l = BC.length b - 1

    go :: Int -> FreqTrain -> FreqTrain
    go !p !fr
      | p == l = fr
      | otherwise =
          let k = BU.unsafeIndex b p
              r = BU.unsafeIndex b (p + 1)
          in go (p + 1) (mappend (singleton k r 1) fr)

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
