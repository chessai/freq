{-# OPTIONS_GHC -O2 -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This library provides a way to train a model
--   that predicts the "randomness" of an input @'ByteString'@,
--   and two datatypes to facilitate this:
--
--  @'FreqTrain'@ is a datatype that can be constructed via
--    training functions that take @'ByteString'@s as input, and
--    can be used with the @'measure'@ function to gather an
--    estimate of the aforementioned probability of "randomness".
--
--    @'Freq'@ is a datatype that is constructed by calling the @'tabulate'@
--    function on a @'FreqTrain'@. @'Freq'@s are meant solely for using (accessing
--    the "randomness" values) the trained model in practise, by making
--    significant increases to speed in exchange for less extensibility;
--    you can neither make a change to a @'Freq'@ or convert it back to
--    a @'FreqTrain'@. In practise this however proves to not be a problem,
--    because training usually only happens once.
--
--    Laws:
--    
--    @'measure' (f :: 'FreqTrain') b ≡ 'measure' ('tabulate' f) b@
--
--    
--    Below is a simple illustration of how to use this library.
--    We are going to write a small command-line application that
--    trains on some data, and scores @'ByteString'@s according to how
--    random they are. We will say that a @'ByteString'@ is 'random'
--    if it scores less than 0.05 (on a scale of 0 to 1), and not random
--    otherwise.
--    
--    First, some imports:
--  
-- @
-- import Freq
-- import Control.Monad (forever)
--  
-- import qualified Data.ByteString.Char8 as BC
-- @
--  
--    Next, a list of @'FilePath'@s containing training data.
--    The training data here is the same as is provided in
--    the sample executable of this library. It consists solely
--    of books in the Public Domain.
--  
-- @ 
-- trainTexts :: [FilePath]
-- trainText
--   = fmap (\x -> "txtdocs/" ++ x ".txt")
--     -- ^
--     -- | this line just tells us that all
--     --   of the training data is in the 'txtdocs'
--     --   directory, and has a '.txt' file extension.
--       [ "2000010"
--       , "2city10"
--       , "80day10"
--       , "alcott-little-261"
--       , "byron-don-315"
--       , "carol10"
--       , "center_earth"
--       , "defoe-robinson-103"
--       , "dracula"
--       , "freck10"
--       , "invisman"
--       , "kipling-jungle-148"
--       , "lesms10"
--       , "london-call-203"
--       , "london-sea-206"
--       , "longfellow-paul-210"
--       , "madambov"
--       , "monroe-d"
--       , "moon10"
--       , "ozland10"
--       , "plgrm10"
--       , "sawy210"
--       , "speckldb"
--       , "swift-modest-171"
--       , "time_machine"
--       , "war_peace"
--       , "white_fang"
--       , "zenda10"
--       ]
-- @
--
--    We are going to use a function provided by this library
--    called @'trainWithMany'@. Its type signature is:
--
-- @
-- trainWithMany
--   :: Foldable t
--   => t FilePath   -- ^ FilePaths containing training data
--   -> IO FreqTrain -- ^ Frequency table generated as a result of training, inside of 'IO'
-- @
--    
--    In other words, @'trainWithMany'@ takes a bunch of files,
--    trains a model with all of the training data contained therein,
--    and returns a @'FreqTrain'@ inside of @'IO'@.
--
--    And now, we get freaky:
--
-- @
-- -- | "passes" returns a message letting the user know whether
-- --   or not their input 'ByteString' was most likely random.
-- --   Recall that our threshold is 0.05 on a scale of 0 to 1.
-- passes :: Double -> String
-- passes x
--   | x < 0.05  = "Too random!"
--   | otherwise = "Looks good to me!"
--
-- main :: IO ()
-- main = do
--   !freak <- trainWithMany trainTexts
--   -- ^
--   -- | create the trained model
--   -- | Note that we do this strictly,
--   -- | so that the model is ready to
--   -- | go when we say it is. 
--   
--   let !freakTable = tabulate freak
--   -- ^
--   -- | optimise the trained model for
--   --   read access
--    
--   putStrLn "Done loading frequencies."
--   -- ^
--   -- | let the user know that our model
--   --   is done training and has finished
--   --   optimising into a 'Freq'
--   
--   forever $ do
--   -- ^
--   -- | make the following code loop forever 
--     
--     putStrLn "Enter text:"
--     -- ^
--     -- | ask the user for some text
--     
--     !bs <- BC.getLine
--     -- ^
--     -- | bs is the input 'ByteString' to score
--     
--     let !score = measure freakTable bs
--     -- ^
--     -- | score of the 'ByteString'!
--     
--     putStrLn $ "Score: " ++ show score ++ "\n"
--       ++ passes score
--     -- ^  
--     -- | print out what the score of the 'ByteString' was,
--     --   along with its 'passing status'.
-- @
--
--    This results in the following interactions, split up for readability:
--
--  >>> Done loading frequencies.
--  >>> Enter text:
--  >>> freq
--  >>> Score: 0.10314131395591991
--  >>> Looks good to me!
--  
--  >>> Enter text:
--  >>> kjdslfkajdslkfjsd
--  >>> Score: 6.693203041828383e-3
--  >>> Too random!
--  
--  >>> Enter text:
--  >>> William
--  >>> Score: 7.086442245879888e-2
--  >>> Looks good to me!
--
--  >>> Enter text:
--  >>> 8op3u92jf
--  >>> Score: 6.687182330334067e-3
--  >>> Too random!
--    
--    As we can see, it rejects the keysmashed text as being too random,
--    while the human-readable text is A-OK. I actually made the threshold
--    of 0.05 too high - it should be somewhere between 0.01 and 0.03, but
--    even then the outcomes would have still been the same. The digram-based
--    approach that 'freq' uses may seem ridiculously naive, but still
--    maintains a high degree of accuracy.
--
--    As an example of a real-world use case, I wrote 'freq' to use at my
--    workplace (I work at a Network Security company) as a way to score
--    domain names according to how random they are. Malicious
--    users spin up fake domains frequently using strings of random characters.
--    This can also be used to score Windows executables, since
--    those follow the same pattern of malicious naming.
--
--    An obvious weakness of this library is that it suffers from what can
--    be referred to as the "xkcd problem". It can score things such as 'xkcd'
--    poorly, even though they are perfectly legitimate domains. The fix I use is
--    to use something like the alexa top 1 million list of domains, along with a
--    HashMap(s) for whitelisting/blacklisting.
--
--    As a wise man once told me - "And then I freaked it."

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

import Data.ByteString (ByteString)
import Freq.Internal
