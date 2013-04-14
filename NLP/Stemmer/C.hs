{-# OPTIONS  -XEmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Haskell bindings for the Snowball stemming library.
-- This module contains all the low-level functions and are more or less direct
-- translations of the foreign function calls.  The 'stem' function expects
-- strings to use UTF-8 encoding.
module NLP.Stemmer.C (
    -- * Types
      Stemmer(..)
    -- * Low level functions
    , stem
    ) where

import Data.Char        (toLower)
import Foreign.C        (CString(..), CInt(..), peekCStringLen, newCString)
import Foreign.Ptr      (Ptr)

data StemmerStruct

-- | Pointer to a stemmer instance
type StemmerP  = Ptr StemmerStruct

-- | Available algorithms.  For English, 'English' is recommended over 'Porter'.
data Stemmer = Danish
             | Dutch
             | English
             | Finnish
             | French
             | German
             | Hungarian
             | Italian
             | Norwegian
             | Portuguese
             | Romanian
             | Russian
             | Spanish
             | Swedish
             | Turkish
             | Porter
             deriving Show

foreign import ccall "libstemmer.h sb_stemmer_new"     sb_stemmer_new    :: CString -> CString -> IO StemmerP
foreign import ccall "libstemmer.h sb_stemmer_delete"  sb_stemmer_delete :: StemmerP -> IO ()
foreign import ccall "libstemmer.h sb_stemmer_stem"    sb_stemmer_stem   :: StemmerP -> CString -> CInt -> IO (CString)
foreign import ccall "libstemmer.h sb_stemmer_length"  sb_stemmer_length :: StemmerP -> IO CInt

-- | Stem a word
stem :: Stemmer -> String -> IO String
stem algorithm word = do
    algorithm' <- stemmerCString algorithm
    encoding   <- newCString "UTF_8"
    stemmer    <- sb_stemmer_new algorithm' encoding
    word'      <- newCString word
    strPtr     <- sb_stemmer_stem   stemmer word' (fromIntegral $ length word)
    strLen     <- sb_stemmer_length stemmer
    result     <- peekCStringLen (strPtr, fromIntegral strLen)
    sb_stemmer_delete stemmer
    return result

stemmerCString :: Stemmer -> IO CString
stemmerCString = newCString . firstLower . show
    where firstLower (first:rest) = (toLower first) : rest 
