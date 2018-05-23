{-# OPTIONS_GHC -O2 -Wall #-}

module Freq
  ( -- * Frequency Table Builder (Trainer) Type
    FreqTrain
    
    -- * Construction
  , empty 
  , singleton

    -- * Training
  , train 
  , trainWith
  , trainWithMany
  , unsafeTrainWith
  , unsafeTrainWithMany

    -- * Using a trained model
  , tabulate
  , Freq
  , measure
  , prob
  
    -- * Pretty Printing
  , prettyFreqTrain
  ) where

import Freq.Internal
