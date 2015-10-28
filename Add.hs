{-# LANGUAGE OverloadedStrings #-}
module Add where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (isNothing, fromJust)
import System.Directory
import System.FilePath

import qualified Crypto.Hash.SHA1 as SHA1

import Object
import Utils

addObj :: GitObject a => a -> IO ()
addObj obj =
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
        else print "object file already exists, skipping :)"
      
