{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Control.Monad
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import Data.Char (intToDigit, digitToInt)
import Data.Word (Word8)
import System.Directory
import System.FilePath

import Test.QuickCheck

newtype SHA1 = SHA1 BS.ByteString
               deriving (Eq, Show)

fromHex :: String -> SHA1
fromHex s =
  let fromHex' [] = []
      fromHex' (a:b:bs) =
        fromIntegral (digitToInt a * 16 + digitToInt b) : fromHex' bs
  in SHA1 (BS.pack $ fromHex' s)

hexSha :: SHA1 -> String
hexSha (SHA1 s) = toHexes s

shaPath :: SHA1 -> FilePath
shaPath s = let (h, t) = splitAt 2 $ hexSha s
            in h </> t

instance Arbitrary SHA1 where
  arbitrary = SHA1 . BS.pack <$> vectorOf 20 (choose (0, 255) :: Gen Word8)

gitRead :: FilePath -> IO (Either String BS.ByteString)
gitRead fp = do
  gitDir <- getGitDirectory
  exists <- fileInGitDir fp
  if exists
    then Right <$> BS.readFile (gitDir </> fp)
    else return $ Left "file not found"

getGitDirectory :: IO FilePath
getGitDirectory = (</> ".git") <$> getRepoRootDir

getRepoRootDir :: IO FilePath
getRepoRootDir = do
  cwd <- getCurrentDirectory >>= canonicalizePath
  gitDirIsHere <- doesDirectoryExist ".git"
  if gitDirIsHere
    then return cwd
    else if cwd == "/"
         then error "Not in Git repo!"
         else withCurrentDirectory ".." getGitDirectory

fileInGitDir :: FilePath -> IO Bool
fileInGitDir fp = do
  gitDir <- getGitDirectory
  doesFileExist (gitDir </> fp)

hashIsInGitDir :: String -> IO Bool
hashIsInGitDir hexes =
  let (h, t) = splitAt 2 hexes
  in fileInGitDir ("objects" </> h </> t)

readSHA :: SHA1 -> IO (Either String BS.ByteString)
readSHA = gitRead . shaPath

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

intToBytes :: Int -> Int -> BS.ByteString
intToBytes numBytes' int' =
  BS.pack $ word8s numBytes' int'
  where
    word8s :: Int -> Int -> [Word8]
    word8s 0 _ = []
    word8s numBytes int =
      word8s (numBytes - 1) (int `div` 256) ++ [fromIntegral $ int `mod` 256]

pack32BitInt :: Int -> BS.ByteString
pack32BitInt = intToBytes 4

pack16BitInt :: Int -> BS.ByteString
pack16BitInt = intToBytes 2
