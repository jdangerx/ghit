module Checkout where

import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString as BS

import Blob
import Commit
import GitTree
import Index
import Ref
import Utils

writeEntryToWorkingTree :: FilePath -> Entry -> IO ()
writeEntryToWorkingTree fp (Entry {shaOf = sha'}) = do
  blobConts <- readSHA sha'
  let blob = blobConts >>= readObj :: Either String Blob
  either print (BS.writeFile fp . content) blob

writeIndexToWorkingTree :: Index -> IO ()
writeIndexToWorkingTree (Index {entriesOf = entries}) =
  void . sequence . M.elems . M.mapWithKey writeEntryToWorkingTree $ entries

checkoutIndex :: IO ()
checkoutIndex = readIndex >>= either print writeIndexToWorkingTree

checkout :: String -> IO ()
checkout s = do
  cont <- readSHA (fromHex s)
  let commitObj = cont >>= readObj
  let treeSHA = gitTreeOf <$> commitObj
  either print readTree treeSHA
  updateRef "HEAD" (Direct (fromHex s))
  checkoutIndex
