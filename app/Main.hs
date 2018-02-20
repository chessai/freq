{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Freq
import Data.ByteString.Char8 (ByteString, getLine)
import Prelude hiding (getLine)
import Control.Monad (forever)

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

main :: IO ()
main = do
  !freak <- createMany trainTexts
  putStrLn "done loading frequencies"
  -- prettyFreq freak 
  forever $ do
    putStrLn "Enter sample text:"
    bs <- getLine 
    putStrLn ("Score: " ++ show (measure freak bs))
