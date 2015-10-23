{-# LANGUAGE OverloadedStrings #-}
module Index where

import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString as A


data Index = Index { versionOf :: Int
                   , numEntriesOf :: Int
  }
           deriving Show

index :: A.Parser Index
index = do
  A.string "DIRC"
  version <- parse32Bit
  numEntries <- parse32Bit
  return $ Index version numEntries

parse32Bit :: A.Parser Int
parse32Bit = do
  bytes <- A.count 4 A.anyWord8
  return . foldl (\acc i -> acc * 256 + fromIntegral i) 0 $ bytes
