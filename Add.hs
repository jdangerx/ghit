{-# LANGUAGE OverloadedStrings #-}
module Add where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
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
    gitDir <- getGitDirectory
    let filePath = gitDir </> "objects" </> hashPath
    alreadyThere <- doesFileExist filePath
    if not alreadyThere
      then do createDirectoryIfMissing True (takeDirectory filePath)
              print $ "writing to " ++ filePath
              BL.writeFile filePath $ compressed obj
      else print ("object file already exists, skipping :)" :: String)

add :: FilePath -> IO ()
add fp = do
  gitDir <- getGitDirectory
  let indexPath = gitDir </> "index"
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
                             BS.writeFile indexPath (writeIndex newInd)
                     else do newInd <- join $ foldM addBlob ind <$> getFilesInDir fp
                             BS.writeFile indexPath (writeIndex newInd)

addBlob :: Index -> FilePath -> IO Index
addBlob ind fpRelToRepo = do
  repoRootDir <- getRepoRootDir
  blob <- makeBlob (repoRootDir </> fpRelToRepo)
  writeObj blob
  updateIndex ind fpRelToRepo blob

getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir dir = do
  ls <- map (dir </>) <$> (filter (/= ".git") <$> getDirectoryContents dir)
  -- YOU ARE NOT YOUR OWN SUBDIR NOW STOP IT
  subDirsRel <- filter ((`notElem` [".", ".."]) . takeFileName)
                <$> filterM doesDirectoryExist ls
  print subDirsRel
  subDirs <- mapM canonicalizePath subDirsRel
  print subDirs
  files <- filterM doesFileExist ls >>= mapM canonicalizePath
  print files
  subDirFiles <- concat <$> mapM getFilesInDir subDirs
  print subDirFiles
  repoRootDir <- getRepoRootDir
  return $ makeRelative repoRootDir <$> (files ++ subDirFiles)
