module Main where

import Control.Monad
import System.Environment

import Object

main :: IO ()
main = do
  [cmd, file] <- getArgs
  when (cmd == "show") $ showLooseObj file >>= print
      

