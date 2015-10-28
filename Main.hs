module Main where

import Control.Monad
import System.Environment

import Add
import Index
import Object
import Utils

main :: IO ()
main = do
  [cmd, file] <- getArgs
  return ()
  when (cmd == "find-git") $ getGitDirectory >>= print
  when (cmd == "add") $ add file
  when (cmd == "show-index") $ showIndex file
