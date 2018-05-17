{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Freq
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC (getLine)

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
      --, "top-1m" 
      , "war_peace"
      , "white_fang"
      , "zenda10"
      ]

main :: IO ()
main = do
  !freak <- trainWithMany trainTexts
  let !freakTable = tabulate freak 
  putStrLn "done loading frequencies"
  --putStrLn "now writing to file"
  --writeFile "ft.txt" (show freakTable)
  forever $ do
    putStrLn "Enter sample text:"
    !bs <- BC.getLine 
    putStrLn ("Score: " ++ show (measure freakTable bs))
