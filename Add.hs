{-# LANGUAGE OverloadedStrings #-}
module Add where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import Data.Maybe (isNothing, fromJust)
import System.Directory
import System.FilePath
import System.Posix.Files

import Crypto.Hash.SHA1 (hash)
import qualified Data.Attoparsec.ByteString as A

import Index
import Object
import Utils

writeObj :: GitObject a => a -> IO ()
writeObj obj =
  let SHA1 sha' = sha obj
      (h, t) = BS.splitAt 1 sha'
      hashPath = toHexes h </> toHexes t
  in do
    gitDirMaybe <- getGitDirectory
    unless (isNothing gitDirMaybe) $ do
      let filePath = fromJust gitDirMaybe </> "objects" </> hashPath
      alreadyThere <- doesFileExist filePath
      if not alreadyThere
        then do createDirectoryIfMissing True (takeDirectory filePath)
                print $ "writing to " ++ filePath
                BL.writeFile filePath $ compressed obj
        else print ("object file already exists, skipping :)" :: String)

add :: FilePath -> IO ()
add fp = do
  gitDirMaybe <- getGitDirectory
  unless (isNothing gitDirMaybe) $ do
    let indexPath = fromJust gitDirMaybe </> "index"
    indFileCont <- BS.readFile indexPath
    let ind' = A.parseOnly index indFileCont
    case ind' of
     Left err -> error $ "index parse failed: " ++ err
     Right ind -> do isFile <- doesFileExist fp
                     isDir <- doesDirectoryExist fp
                     unless (isFile || isDir) $ fail "File does not exist!"
                     if isFile
                       then do blob <- makeBlob fp
                               newInd <- updateIndex ind fp blob
                               print ("updating index with blob" :: String)
                               BS.writeFile indexPath (writeIndex newInd)
                               writeObj blob
                       else do tree <- makeTree fp
                               print ("updating index with tree" :: String)
                               newInd <- updateIndex ind fp tree
                               BS.writeFile indexPath (writeIndex newInd)
                               writeObj tree


updateIndex :: GitObject a => Index -> FilePath -> a -> IO Index
updateIndex ind fp obj =
  addEntry ind <$> makeEntry fp obj

addEntry :: Index -> Entry -> Index
addEntry ind@(Index {numEntriesOf = numEntries, entriesOf = entries}) ent =
  let newNEntries = numEntries + 1
      newEntries = sortOn (\e -> entryPathOf e ++ show (stageOf (flagsOf e)))
                   $ entries ++ [ent]
      indWithNewEntries =
        ind { numEntriesOf = newNEntries, entriesOf = newEntries }
      indWithOldChecksum = writeIndex indWithNewEntries
      indNoChecksum =
        BS.take (BS.length indWithOldChecksum - 20) indWithOldChecksum
  in
   indWithNewEntries { checksumOf = SHA1 $ hash indNoChecksum }

makeTree :: FilePath -> IO Tree
makeTree = error "not implemented yet"

makeBlob:: FilePath -> IO Blob
makeBlob fp = do
  cont <- BS.readFile fp
  return $ Blob (BS.length cont) cont

makeEntry :: GitObject a => FilePath -> a -> IO Entry
makeEntry fp obj = do
  fileStatus <- getFileStatus fp
  let mode = Mode { objTypeOf = if isSymbolicLink fileStatus
                                then Symlink
                                else File
                  , permissionOf = if fileMode fileStatus == ownerExecuteMode
                                   then P755
                                   else P644
        }
  let flags = Flags { assumeValidOf = False
                    , extendedOf = False
                    , stageOf = 0
                    , nameLenOf = length fp
                    , reservedOf = Nothing
                    , skipWorktreeOf = Nothing
                    , intentToAddOf = Nothing}
  return Entry { ctimeOf = fromEnum $ statusChangeTimeHiRes fileStatus
               , mtimeOf = fromEnum $ modificationTimeHiRes fileStatus
               , deviceOf = fromEnum $ deviceID fileStatus
               , inoOf = fromEnum $ fileID fileStatus
               , modeOf = mode
               , uidOf = fromEnum $ fileOwner fileStatus
               , gidOf = fromEnum $ fileGroup fileStatus
               , fileSizeOf = fromEnum $ fileSize fileStatus
               , shaOf = sha obj
               , flagsOf = flags
               , entryPathOf = fp }
