module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Fountain.Script (processFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _          -> do
      putStrLn "Usage: fountain-parser <filename.fountain>"
      exitFailure
