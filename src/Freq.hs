{-# OPTIONS_GHC -O2 -Wall #-}

module Freq
  ( -- * Frequency table type
    Freq
    
    -- * Construction
  , empty 
  , singleton

  , tabulate

    -- * Training
  , train 
  , trainWith
  , trainWithMany
    
    -- * Using a trained model
  , FreqTable 
  , prob
  , measure

    -- * Pretty Printing
  , prettyFreq
  ) where

import Freq.Internal
