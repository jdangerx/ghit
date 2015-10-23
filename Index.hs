{-# LANGUAGE OverloadedStrings #-}
module Index where

import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString as A


data Index = Index {
  headerOf :: IndexHeader
  }
           deriving Show

data IndexHeader = IndexHeader { versionOf :: Int
                               , numEntriesOf :: Int }
                   deriving Show

parseHeader :: A.Parser IndexHeader
parseHeader = IndexHeader <$> (A.string "DIRC" *> parse32Bit) <*> parse32Bit

parse32Bit :: A.Parser Int
parse32Bit = do
  bit1 <- A.anyWord8
  bit2 <- A.anyWord8
  bit3 <- A.anyWord8
  bit4 <- A.anyWord8
  return . fromIntegral $ bit4 + bit3 * 2^8 + bit2 * 2^16 + bit1 * 2^24

parseIndex :: A.Parser Index
parseIndex = Index <$> parseHeader
