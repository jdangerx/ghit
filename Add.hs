{-# LANGUAGE OverloadedStrings #-}
module Add where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isNothing, fromJust)
import System.Directory
import System.FilePath

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
    indexExists <- doesFileExist indexPath
    ind' <- if indexExists
            then liftM (A.parseOnly index) (BS.readFile indexPath)
            else return $ Right emptyIndex
    case ind' of
     Left err -> error $ "index parse failed: " ++ err
     Right ind -> do isFile <- doesFileExist fp
                     isDir <- doesDirectoryExist fp
                     unless (isFile || isDir) $ fail "File does not exist!"
                     if isFile
                       then do newInd <- addBlob ind fp
                               -- print ("updating index with blob" :: String)
                               BS.writeFile indexPath (writeIndex newInd)
                       else do newInd <- join $ foldM addBlob ind <$> getFilesInDir fp
                               BS.writeFile indexPath (writeIndex newInd)

addBlob :: Index -> FilePath -> IO Index
addBlob ind fp = do
  blob <- makeBlob fp
  writeObj blob
  updateIndex ind fp blob

getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir dir = do
  ls <- map (dir </>) <$> filter (/= ".git") <$> getDirectoryContents dir
  -- YOU ARE NOT YOUR OWN SUBDIR NOW STOP IT
  subdirs <- filter ((`notElem` [".", ".."]) . takeFileName)
             <$> filterM doesDirectoryExist ls
  files <- filterM doesFileExist ls
  subDirFiles <- concat <$> mapM getFilesInDir subdirs
  mapM makeAbsolute (files ++ subDirFiles)
  
  
