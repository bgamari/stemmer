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
    , stem
    , withStemmer
    , stem'
    , stem''
    , Stemmer'
    ) where

import Data.Char        (toLower)
import Foreign.C        (CString, CInt, peekCStringLen, newCString)
import Foreign          (Ptr, FunPtr, ForeignPtr, newForeignPtr, withForeignPtr)

data StemmerStruct

-- | Pointer to a stemmer instance
type Stemmer  = ForeignPtr StemmerStruct
type Stemmer' = Ptr        StemmerStruct 

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

foreign import ccall "libstemmer.h sb_stemmer_stem"    sb_stemmer_stem   :: Stemmer' -> CString -> CInt -> IO (CString)
foreign import ccall "libstemmer.h sb_stemmer_length"  sb_stemmer_length :: Stemmer' -> IO CInt
foreign import ccall "libstemmer.h sb_stemmer_new"     sb_stemmer_new    :: CString -> CString -> IO Stemmer'
foreign import ccall "libstemmer.h &sb_stemmer_delete" sb_stemmer_delete :: FunPtr (Stemmer' -> IO ())
foreign import ccall "libstemmer.h sb_stemmer_delete" sb_stemmer_delete' :: Stemmer' -> IO ()

-- | Create a new stemmer instance. 
stemmer :: Algorithm -> IO Stemmer
stemmer algorithm = do
    algorithm' <- algorithmCString algorithm
    encoding   <- utf8
    new        <- sb_stemmer_new algorithm' encoding
    newForeignPtr sb_stemmer_delete new 

-- | Stem a word.
stem :: Algorithm -> String -> IO String
stem algorithm word = do
    stemmer' <- stemmer algorithm
    word'    <- newCString word
    strPtr   <- withForeignPtr stemmer' (\stemmer'' -> sb_stemmer_stem stemmer'' word' (fromIntegral $ length word))
    strLen   <- withForeignPtr stemmer' (\stemmer'' -> sb_stemmer_length stemmer'')
    peekCStringLen (strPtr, fromIntegral strLen)

-----------------------------------------------------------

utf8 :: IO CString
utf8 = newCString "UTF_8"

algorithmCString :: Algorithm -> IO CString
algorithmCString = newCString . firstLower . show
    where firstLower (first:rest) = (toLower first) : rest 


new :: Algorithm -> IO Stemmer'
new algorithm = do
    algorithm' <- algorithmCString algorithm
    encoding   <- utf8
    sb_stemmer_new algorithm' encoding
 
--- | Delete a stemmer instance.  Don't use it after deleting.
delete :: Stemmer' -> IO ()
delete = sb_stemmer_delete'


withStemmer :: Algorithm -> (Stemmer' -> IO a) -> IO a
withStemmer algorithm action = do
    stemmer <- new algorithm
    result  <- action stemmer
    delete stemmer
    return result

stem' :: Stemmer' -> String -> IO String
stem' stemmer word = do
    word'  <- newCString word
    strPtr <- sb_stemmer_stem   stemmer word' (fromIntegral $ length word)
    strLen <- sb_stemmer_length stemmer
    peekCStringLen (strPtr, fromIntegral strLen)
    
stem'' :: Algorithm -> String -> IO String
stem'' algorithm word = do
    algorithm' <- algorithmCString algorithm
    encoding   <- utf8
    stemmer    <- sb_stemmer_new algorithm' encoding
    word'      <- newCString word
    strPtr     <- sb_stemmer_stem   stemmer word' (fromIntegral $ length word)
    strLen     <- sb_stemmer_length stemmer
    result     <- peekCStringLen (strPtr, fromIntegral strLen)
    sb_stemmer_delete' stemmer
    return result
