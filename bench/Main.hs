{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Gauge.Main
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Data.Char (ord)
import Freq
import qualified Data.ByteString as B
import qualified Data.List as L

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

c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

main :: IO ()
main = do
  !freak <- trainWithMany trainTexts
  let !freakTable = tabulate freak
  Prelude.putStrLn "done loading frequencies"
  let !bs = B.pack $ fmap c2w $ (L.take 50000 (L.cycle ['a'..'z']))
  Prelude.putStrLn "done making test bytestrings"
  Prelude.putStrLn "now gauging....!!!"

  defaultMain
    [ 
      bgroup "FREAKY MAP VS. FREAKY TABLE: "
        [ bench "FREAKY MAP"   $ whnf (measure freak) bs
        , bench "FREAKY TABLE" $ whnf (measure freakTable) bs
        ]
    ]
