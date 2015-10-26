module Utils where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import Data.Char (intToDigit)
import Data.Word (Word8)

import Test.QuickCheck

digit :: Word8 -> Bool
digit w = w >= 48 && w <= 57

asciiToInt :: BS.ByteString -> Int
asciiToInt =
  BS.foldl (\acc w -> acc * 10  + fromIntegral w - 48) 0

intToAscii :: Int -> BS.ByteString
intToAscii i' = BS.pack (word8s i')
  where
    word8s :: Int -> [Word8]
    word8s i
      | i < 10 = [fromIntegral i + 48]
      | otherwise = word8s (i `div` 10) ++ [fromIntegral i `mod` 10 + 48]

parseAsciiInt :: A.Parser Int
parseAsciiInt = asciiToInt <$> A.takeWhile1 digit

prop_asciiIntRoundTrip :: Gen Bool
prop_asciiIntRoundTrip = do
  i <- arbitrary `suchThat` (>= 0)
  return $ asciiToInt (intToAscii i) == i

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
