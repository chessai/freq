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
  checkParallel $ Group "Freak Equality"
    [ ("Freak Equality", prop_Equal freak freakTable)
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

sizedByteString :: Range.Size -> Gen ByteString
sizedByteString (Range.Size n) = do
  m <- Gen.enum 0 n
  fmap B.pack $ Gen.list (Range.constant 0 m) (randWord8)
    where
      randWord8 :: Gen Word8
      randWord8 = Gen.word8 Range.constantBounded

prop_Equal :: FreqTrain -> Freq -> Property
prop_Equal f ft =
  property $ do
    b <- forAll $ Gen.sized sizedByteString 
    measure f b === measure ft b
