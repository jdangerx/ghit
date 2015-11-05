module Main where

import Control.Monad
import System.Environment

import Add
import Commit
import GitTree
import Index
import Object
import Utils

main :: IO ()
main = do
  [cmd, file, msg] <- getArgs
  return ()
  when (cmd == "find-git") $ getGitDirectory >>= print
  when (cmd == "add") $ add file
  when (cmd == "show-index") $ showIndex file
  when (cmd == "write-tree") $ writeTree
  when (cmd == "commit-tree") $ commitTree file msg
  when (cmd == "commit") $ commit file
  when (cmd == "read-tree") $ readTree (fromHex file)
