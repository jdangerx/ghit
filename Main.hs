module Main where

import Control.Monad
import System.Environment

import Object
import Add

main :: IO ()
main = do
  [cmd, file] <- getArgs
  when (cmd == "show") $ showLooseObj file >>= print
      

