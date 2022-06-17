module Utils where

import Data.Text ( pack, Text )
import System.Exit ( exitSuccess )
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)

-- Getline that checks fro ":q" to quit program
myGetLine :: IO String
myGetLine = do
  str <- getLine
  if str == ":q" then exitSuccess else return str

-- putStr that flushes always
myPutStr :: String -> IO ()
myPutStr str = do
  putStr str
  hFlush stdout

-- Validates an integer            
validateInt :: String -> Maybe Int
validateInt = readMaybe

intGetLine :: IO Int
intGetLine = do
  str <- myGetLine
  case validateInt str of
    Nothing -> do
      putStrLn "Invalid int."
      intGetLine
    Just a -> return a

textGetLine :: IO Text
textGetLine = pack <$> myGetLine
