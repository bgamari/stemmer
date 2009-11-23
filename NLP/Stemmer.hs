-- | Haskell bindings for the Snowball stemming library
-- A 'pure' stemming interface. Strings should use UTF-8 encoding. 
module NLP.Stemmer ( 
    -- * Stemming algorithms
      Stemmer(..)
    -- * Stemming functions
    , stem
    ) where

import           NLP.Stemmer.C (Stemmer)
import qualified NLP.Stemmer.C as C
import           Foreign       (unsafePerformIO)

-- | Stem a word
{-# NOINLINE stem #-}
stem :: Stemmer -> String -> String
stem algorithm input = unsafePerformIO $ C.stem algorithm input
