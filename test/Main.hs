{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Freq.Digram
import Freq.Digram.Builder
import Hedgehog
import qualified Data.ByteString as B
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

main :: IO Bool
main = do
  !freak <- createWithMany trainTexts
  let !freakTable = tabulate freak
  Prelude.putStrLn "done loading frequencies"
  Prelude.putStrLn "now testing with Hedgehog"
  checkParallel $ Group "Freq Equality"
    [ ("prop_Equal", prop_Equal freak freakTable)
    ]

trainTexts :: [FilePath]
trainTexts
  = fmap (\x -> "txtdocs/" ++ x ++ ".txt")
      [ "2000010"
      , "center_earth"
      , "lesms10"
      , "moon10"
      , "time_machine"
      , "2city10"
      , "defoe-robinson-103"
      , "london-call-203"
      , "ozland10"
      , "war_peace"
      , "80day10"
      , "dracula"
      , "london-sea-206"
      , "plgrm10"
      , "white_fang"
      , "alcott-little-261"
      , "freck10"
      , "longfellow-paul-210"
      , "sawy210"
      , "zenda10"
      , "byron-don-315"
      , "invisman"
      , "madambov"
      , "speckldb"
      , "carol10"
      , "kipling-jungle-148"
      , "monroe-d"
      , "swift-modest-171"
      ]

epsilon :: Double
epsilon = 0.0003

sizedByteString :: Range.Size -> Gen ByteString
sizedByteString (Range.Size n) = do
  m <- Gen.enum 0 n
  fmap B.pack $ Gen.list (Range.constant 0 m) (randWord8)
    where
      randWord8 :: Gen Word8
      randWord8 = Gen.word8 Range.constantBounded

prop_Equal :: Freq -> FreqTable -> Property
prop_Equal f ft =
  property $ do
    b <- forAll $ Gen.sized sizedByteString 
    measure f b === measureTable ft b
