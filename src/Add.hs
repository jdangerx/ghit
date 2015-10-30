{-# LANGUAGE OverloadedStrings #-}
module Add where

import Control.Monad
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath

import qualified Data.Attoparsec.ByteString as A

import Index
import Blob
import Utils

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
  blob <- mkBlobFromFile (repoRootDir </> fpRelToRepo)
  write blob
  updateIndex ind fpRelToRepo blob

getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir dir = do
  ls <- map (dir </>) <$> (filter (/= ".git") <$> getDirectoryContents dir)
  subDirsRel <- filter ((`notElem` [".", ".."]) . takeFileName)
                <$> filterM doesDirectoryExist ls
  subDirs <- mapM canonicalizePath subDirsRel
  files <- filterM doesFileExist ls >>= mapM canonicalizePath
  subDirFiles <- concat <$> mapM getFilesInDir subDirs
  repoRootDir <- getRepoRootDir
  return $ makeRelative repoRootDir <$> (files ++ subDirFiles)
