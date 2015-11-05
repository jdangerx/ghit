{-# LANGUAGE OverloadedStrings #-}

module Object where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.Directory
import System.FilePath

import Crypto.Hash.SHA1 (hash)
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.Attoparsec.ByteString as A

import Utils

data GitFileMode = NormalMode | ExecutableMode | DirMode
                 deriving (Eq, Show)

class GitObject a where
  typeName :: a -> BS.ByteString
  content :: a -> BS.ByteString
  parser :: A.Parser a

  contLen :: a -> Int
  contLen = BS.length . content

  serialize :: a -> BS.ByteString
  serialize obj =
   BS.concat [ typeName obj
             , " "
             , BSC.pack . show . contLen $ obj
             , "\x00", content obj]

  compressed :: a -> BL.ByteString
  compressed = Zlib.compress . BL.fromStrict . serialize

  deserialize :: BS.ByteString -> Either String a
  deserialize = A.parseOnly parser

  sha :: a -> SHA1
  sha = SHA1 . hash . serialize

  write :: a -> IO ()
  write obj =
    let SHA1 sha' = sha obj
        (h, t) = BS.splitAt 1 sha'
        hashPath = toHexes h </> toHexes t
    in do
      gitDir <- getGitDirectory
      let filePath = gitDir </> "objects" </> hashPath
      alreadyThere <- doesFileExist filePath
      if not alreadyThere
        then do createDirectoryIfMissing True (takeDirectory filePath)
                putStrLn $ toHexes sha'
                BL.writeFile filePath $ compressed obj
        else print ("object file " ++ toHexes sha' ++ " already exists, skipping :)")

  readObj :: BS.ByteString -> Either String a
  readObj = deserialize . BL.toStrict . Zlib.decompress . BL.fromStrict

pContLen :: BS.ByteString -> A.Parser Int
pContLen bs = A.string bs *> A.word8 32 *> parseAsciiInt <* A.word8 0

