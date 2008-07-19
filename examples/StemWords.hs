import NLP.Stemmer
import Control.Monad (unless)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
 
main :: IO ()
main = do
    putStrLn "Enter a sentence to stem, an empty line to stop."
    hSetBuffering stdout NoBuffering -- to print a prompt
    stemUserInput
 
stemUserInput :: IO ()
stemUserInput = do
    putStr "> "
    string <- getLine
    unless (string == "") $ do 
        putStrLn $ (++) "< " $ unwords $ 
                               stemWords English $ 
                               words string
        stemUserInput
