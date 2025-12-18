{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as T
import Fountain.Parser (parseFountain)
import Fountain.AST

main :: IO ()
main = do
  input <- T.readFile "test/examples/sample_short.fountain"
  case parseFountain "sample_short.fountain" input of
    Left err   -> putStrLn ("Parse error:\n" ++ err)
    Right doc  -> print doc
