{-# OPTIONS  -XEmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Haskell bindings for the Snowball stemming library.
-- This module contains all the low-level functions and are more or less direct
-- translations of the foreign function calls.  The 'stem' function expects
-- strings to use UTF-8 encoding.
module NLP.Stemmer.C (
    -- * Types
      Algorithm(..)
    , Stemmer
    -- * Low level functions
    , new
    , delete
    , stem
    -- * Wrapper
    , withStemmer
    ) where

import Data.Char        (toLower)
import Foreign.C        (CString, CInt, peekCStringLen, newCString)
import Foreign.Ptr      (Ptr)

data StemmerStruct

-- | Pointer to a stemmer instance
type Stemmer = Ptr StemmerStruct

-- | Available algorithms.  For English, 'English' is recommended over 'Porter'.
data Algorithm = Danish
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

foreign import ccall "libstemmer.h sb_stemmer_new"    sb_stemmer_new    :: CString -> CString -> IO Stemmer
foreign import ccall "libstemmer.h sb_stemmer_delete" sb_stemmer_delete :: Stemmer -> IO ()
foreign import ccall "libstemmer.h sb_stemmer_stem"   sb_stemmer_stem   :: Stemmer -> CString -> CInt -> IO (CString)
foreign import ccall "libstemmer.h sb_stemmer_length" sb_stemmer_length :: Stemmer -> IO CInt

-- | Create a new stemmer instance.  When you're done using the stemmer, you
-- should 'delete' it, freeing the memory.
new :: Algorithm -> IO Stemmer
new algorithm = do
    algorithm' <- algorithmCString algorithm
    encoding   <- utf8
    sb_stemmer_new algorithm' encoding

-- | Delete a stemmer instance.  Don't use it after deleting.
delete :: Stemmer -> IO ()
delete = sb_stemmer_delete

-- | Stem a word using the stemmer instance.
stem :: Stemmer -> String -> IO String
stem stemmer word = do
    word'  <- newCString word
    strPtr <- sb_stemmer_stem   stemmer word' (fromIntegral $ length word)
    strLen <- sb_stemmer_length stemmer
    peekCStringLen (strPtr, fromIntegral strLen)

-----------------------------------------------------------

-- | Perform stemming using a wrapper handling the low-level creation and
-- deletion of the stemmer instance.
withStemmer :: Algorithm -> (Stemmer -> IO a) -> IO a
withStemmer algorithm action = do
    stemmer <- new algorithm
    result  <- action stemmer
    delete stemmer
    return result

-----------------------------------------------------------

utf8 :: IO CString
utf8 = newCString "UTF_8"

algorithmCString :: Algorithm -> IO CString
algorithmCString = newCString . firstLower . show
    where firstLower (first:rest) = (toLower first) : rest 
