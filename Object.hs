{-# LANGUAGE OverloadedStrings #-}

module Object where

import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as BS
import Data.Word (Word8)
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.Attoparsec.ByteString as A

data Object = Obj { typeOf :: ObjectType
                  , contentOf :: Content }
              deriving Show

type Content = BS.ByteString

data ObjectType = Blob
                | Tree
                | Commit
                | Tag
                  deriving (Read, Show, Eq)

byteStringToInt :: BS.ByteString -> Int
byteStringToInt =
  BS.foldl (\acc w -> acc * 10  + fromIntegral w - 48) 0

intToByteString :: Int -> BS.ByteString
intToByteString i' = BS.pack (word8s i')
  where
    word8s :: Int -> [Word8]
    word8s i
          | i < 10 = [fromIntegral i + 48]
          | otherwise = word8s (i `div` 10) ++ [fromIntegral i `mod` 10 + 48]

parseObjType :: A.Parser ObjectType
parseObjType = A.choice
  [ A.string "blob" *> return Blob
  , A.string "tree" *> return Tree
  , A.string "commit" *> return Commit
  , A.string "tag" *> return Tag ]

parseContLen :: A.Parser Int
parseContLen = do
  numStr <- A.takeWhile1 (\w -> w >= 48 && w <= 57)
  return $ byteStringToInt numStr

parseObj :: A.Parser Object
parseObj = do
  objType <- parseObjType
  A.word8 32
  contLen <- parseContLen
  A.word8 0
  cont <- A.take contLen
  return $ Obj objType cont

getLooseObj :: BL.ByteString -> Maybe Object
getLooseObj = A.maybeResult . A.parse parseObj . BL.toStrict . Zlib.decompress

putLooseObj :: Object -> BL.ByteString
putLooseObj Obj { contentOf = cont, typeOf = objType } =
  let
    typeBL Blob = "blob"
    typeBL Tree = "tree"
    typeBL Commit = "commit"
    typeBL Tag = "tag"
    contSize = intToByteString $ BS.length cont
  in
   Zlib.compress $ BL.concat [ typeBL objType
                             , " "
                             , BL.fromStrict contSize
                             , "\x00"
                             , BL.fromStrict cont]

hello :: BL.ByteString
hello = "x\x01K\xca\xc9OR0e\xc8H\xcd\xc9\xc9\x07\x00\x19\xaa\x04\t"

helloBlob :: Object
helloBlob = Obj Blob "hello"

showLooseObj :: FilePath -> IO (Maybe Object)
showLooseObj fp = liftM getLooseObj (BL.readFile fp)
