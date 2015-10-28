module Main where

import Control.Monad
import System.Environment

import Object
import Add
import Utils

main :: IO ()
main = do
  [cmd, file] <- getArgs
  return ()
  when (cmd == "find-git") $ getGitDirectory >>= print
      

