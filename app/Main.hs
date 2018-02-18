{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Freq
import Data.ByteString (ByteString, getLine)
import Prelude hiding (FilePath, getLine)

toby :: FilePath
toby = "tests/toby.freq"

main :: IO Double
main = do
  bs <- getLine
  freak <- create [toby]
  pure $ measure freak bs 
