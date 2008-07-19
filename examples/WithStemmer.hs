import NLP.Stemmer.C
import Control.Monad (unless)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
 
main :: IO ()
main = do
    putStrLn "Enter a sentence to stem, an empty line to stop."
    hSetBuffering stdout NoBuffering -- to print a prompt
    withStemmer English stemUserInput
 
stemUserInput :: Stemmer -> IO ()
stemUserInput stemmer = do
    putStr "> "
    string <- getLine
    unless (string == "") $ do
        string' <- mapM (stem stemmer) $ words string
        putStrLn $ (++) "< " $ unwords string'
        stemUserInput stemmer
