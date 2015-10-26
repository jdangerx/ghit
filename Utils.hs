module Utils where

import qualified Data.Attoparsec as A
import qualified Data.ByteString as BS
import Data.Char (intToDigit)
import Data.Word (Word8)

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

toHexes :: BS.ByteString -> String
toHexes =
  BS.foldr
  (\byte hexes ->
    (++ hexes) $ intToDigit <$> [fromIntegral byte `div` 16
                                , fromIntegral byte `mod` 16])
  ""

parseBytesToInt :: Int -> A.Parser Int
parseBytesToInt n = do
  bytes <- A.count n A.anyWord8
  return . foldl (\acc i -> acc * 256 + fromIntegral i) 0 $ bytes

parse32Bit :: A.Parser Int
parse32Bit = parseBytesToInt 4

parse16Bit :: A.Parser Int
parse16Bit = parseBytesToInt 2
