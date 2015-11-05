module Checkout where

import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString as BS

import Commit
import GitTree
import Index
import Utils

writeEntryToWorkingTree :: FilePath -> Entry -> IO ()
writeEntryToWorkingTree fp (Entry {shaOf = sha'}) = do
  blobConts <- readSHA sha'
  either print (BS.writeFile fp) blobConts

writeIndexToWorkingTree :: Index -> IO ()
writeIndexToWorkingTree (Index {entriesOf = entries}) =
  void . sequence . M.elems . M.mapWithKey writeEntryToWorkingTree $ entries

checkoutIndex :: IO ()
checkoutIndex = readIndex >>= either print writeIndexToWorkingTree

checkout :: String -> IO ()
checkout s = do
  cont <- readSHA (fromHex s)
  let treeSHA = gitTreeOf <$> (cont >>= readObj)
  let _ = readTree <$> treeSHA
  checkoutIndex
