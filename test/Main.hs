{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Word (Word8)
import Freq
import Hedgehog
import qualified Data.ByteString as B
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.List as List

main :: IO Bool
main = do
  !freak <- trainWithMany trainTexts
  let !freakTable = tabulate freak
  Prelude.putStrLn "done loading frequencies"
  Prelude.putStrLn "now testing with Hedgehog"
  checkParallel $ Group "Freq Equality"
    [ ("prop_Equal", prop_Equal freak freakTable)
    , ("prop_Fail", prop_Fail freak freakTable) 
    ]

trainTexts :: [FilePath]
trainTexts
  = fmap (\x -> "txtdocs/" ++ x ++ ".txt")
      [ "2000010"
      , "2city10"
      , "80day10"
      , "alcott-little-261"
      , "byron-don-315"
      , "carol10"
      , "center_earth"
      , "defoe-robinson-103"
      , "dracula"
      , "freck10"
      , "invisman"
      , "kipling-jungle-148"
      , "lesms10"
      , "london-call-203"
      , "london-sea-206"
      , "longfellow-paul-210"
      , "madambov"
      , "monroe-d"
      , "moon10"
      , "ozland10"
      , "plgrm10"
      , "sawy210"
      , "speckldb"
      , "swift-modest-171"
      , "time_machine"
      , "top-1m" 
      , "war_peace"
      , "white_fang"
      , "zenda10"
      ]

type FreqProp = Freq -> FreqTable -> Property

epsilon :: Double
epsilon = 0.0003

threshold :: Double
threshold = 5.0

failing :: Double -> Bool
failing x = x <= threshold - epsilon

passing :: Double -> Bool
passing x = x > threshold + epsilon

sizedByteString :: Range.Size -> Gen ByteString
sizedByteString (Range.Size n) = do
  m <- Gen.enum 0 n
  fmap B.pack $ Gen.list (Range.constant 0 m) (randWord8)
    where
      randWord8 :: Gen Word8
      randWord8 = Gen.word8 Range.constantBounded

sizedByteStringRandom :: Range.Size -> Gen ByteString
sizedByteStringRandom (Range.Size n) = do
  m <- Gen.enum 0 n
  fmap B.pack $ Gen.list (Range.constant 0 m) (randWord8)
    where
      randWord8 :: Gen Word8
      randWord8 = Gen.frequency $ fmap (\(x,y) -> (x, pure (c2w y))) $
        List.zip (List.replicate l 1 :: [Int]) w
        where
          w = (['a'..'z'] ++ ['1'..'0'])
          l = length w
       
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

prop_Fail :: FreqProp
prop_Fail f ft =
  property $ do
    b <- forAll $ Gen.sized sizedByteStringRandom
    let fmeasure  = measure f b
        ftmeasure = measure ft b
    if failing ftmeasure
      then success
      else failure

prop_Equal :: FreqProp
prop_Equal f ft =
  property $ do
    b <- forAll $ Gen.sized sizedByteString 
    measure f b === measure ft b
