{-# OPTIONS_GHC -O2 -Wall #-}

module Freq.Digram
  ( -- * Frequency table type
    Freq
    
    -- * Construction
  , empty 
  , singleton
    
    -- * Training
  , tally
  , createWith
  , createWithMany
  , defWeight
    
    -- * Using a trained model
  , probDigram 
  , measure

    -- * Pretty Printing
  , prettyFreq
  ) where

import Freq.Digram.Internal
