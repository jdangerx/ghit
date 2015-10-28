{-# LANGUAGE OverloadedStrings #-}
module Commit where

import Control.Monad
-- import qualified Data.ByteString as BS
import System.FilePath
import System.Posix.Files

import Test.QuickCheck
import Test.QuickCheck.Monadic
  
import Index
import Object
import Utils

data Commit = Commit {
  treeOf :: SHA1
  }

makeTreeFromIndex :: Index -> IO Tree
makeTreeFromIndex (Index { entriesOf = entries }) =
  Tree <$> mapM indexEntryToTreeEntry entries

indexEntryToTreeEntry :: Entry -> IO TreeEntry
indexEntryToTreeEntry (Entry { entryPathOf = fp, shaOf = sha' }) =
  do fs <- getFileStatus fp
     return $ TreeEntry (fileMode fs) fp sha'

writeTree :: IO ()
writeTree = readIndex
             >>= either print (makeTreeFromIndex >=> write)

mkCommitFromTree :: SHA1 -> IO Commit
mkCommitFromTree treeHash = return $ Commit treeHash
