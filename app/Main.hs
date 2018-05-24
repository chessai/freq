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
      , "war_peace"
      , "white_fang"
      , "zenda10"
      ]

passes :: Double -> String
passes x
  | x < 0.05 = "Too random!"
  | otherwise = "Looks good to me!"

main :: IO ()
main = do
  !freak <- trainWithMany trainTexts
  let !freakTable = tabulate freak 
  putStrLn "Done loading frequencies"
  forever $ do
    putStrLn "Enter text:"
    !bs <- BC.getLine 
    let !score = measure freakTable bs 
    putStrLn $ "Score: " ++ show score ++ "\n"
      ++ passes score
