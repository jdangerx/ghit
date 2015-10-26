{-# LANGUAGE OverloadedStrings #-}

module Object where

import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.Attoparsec.ByteString as A

import Utils

data Object = Blob { contentOf :: Content }
            | Tree { contentOf :: Content }
            | Commit { contentOf :: Content }
            | Tag { contentOf :: Content }
              deriving Show

type Content = BS.ByteString

parseObjType :: A.Parser (Content -> Object)
parseObjType = A.choice
  [ A.string "blob" *> return Blob
  , A.string "tree" *> return Tree
  , A.string "commit" *> return Commit
  , A.string "tag" *> return Tag ]

parseContLen :: A.Parser Int
parseContLen = do
  numStr <- A.takeWhile1 digit
  return $ asciiToInt numStr

parseObj :: A.Parser Object
parseObj = do
  objCons <- parseObjType
  A.word8 32 -- ' '
  contLen <- parseContLen
  A.word8 0 -- '\0'
  cont <- A.take contLen
  return $ objCons cont

getLooseObj :: BL.ByteString -> Maybe Object
getLooseObj = A.maybeResult . A.parse parseObj . BL.toStrict . Zlib.decompress

serializeObj :: Object -> BS.ByteString
serializeObj obj =
  let cont = contentOf obj
      contSize = intToAscii $ BS.length cont
  in
   BS.concat [typeName obj
             , " "
             , contSize
             , "\x00", cont]

typeName :: Object -> BS.ByteString
typeName (Blob _) = "blob"
typeName (Tree _) = "tree"
typeName (Commit _) = "commit"
typeName (Tag _) = "tag"

hello :: BL.ByteString
hello = "x\x01K\xca\xc9OR0e\xc8H\xcd\xc9\xc9\x07\x00\x19\xaa\x04\t"

helloBlob :: Object
helloBlob = Blob "hello"

showLooseObj :: FilePath -> IO (Maybe Object)
showLooseObj fp = liftM getLooseObj (BL.readFile fp)
