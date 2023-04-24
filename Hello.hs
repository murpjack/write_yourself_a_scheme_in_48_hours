module Main where

import System.Environment

main :: IO ()
main =
  putStrLn . (++) "Hello " . unwords =<< getArgs
