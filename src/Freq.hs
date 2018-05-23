{-# OPTIONS_GHC -O2 -Wall #-}

{-| A @'Freq'@ assigns to a @'ByteString'@ a randomness
    probability value of type @'Double'@, indicating  
    of type @'Double'@. A @'Freq'@ is constructed after



-}

module Freq
  ( -- * Frequency table builder (trainer) type
    FreqTrain
    
    -- * Construction
  , empty 
  , singleton

    -- * Training
  , train 
  , trainWith
  , trainWithMany

    -- * Using a trained model
  , tabulate
  , Freq
  , measure
  , prob
  
    -- * Pretty Printing
  , prettyFreqTrain
  ) where

import Freq.Internal
