{-# LANGUAGE OverloadedStrings #-}
module Index where

import Control.Monad
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString as A

import Utils

-- https://github.com/git/git/blob/master/Documentation/technical/index-format.txt

data Index = Index { versionOf :: Int
                   , numEntriesOf :: Int
                   , entriesOf :: [Entry]
                   , extensionsOf :: [Extension]
                   , checksumOf :: BS.ByteString
  }
           deriving Show

data Entry = Entry { ctimeOf :: Int
                   , mtimeOf :: Int
                   , deviceOf :: Int
                   , inoOf :: Int
                   , modeOf :: Mode
                   , uidOf :: Int
                   , gidOf :: Int
                   , fileSizeOf :: Int
                   , shaOf :: BS.ByteString
                   , flagsOf :: Flags
                   , entryPathOf :: BS.ByteString
  }
           -- deriving Show

data Extension = Link { sharedIndexOf :: BS.ByteString
                      , deleteBitMapOf :: BS.ByteString
                      , replaceBitMapOf :: BS.ByteString }
               | CachedTree { pathOf :: BS.ByteString
                            , numEntriesInCachedTree :: Int
                            , numSubtreesOf :: Int
                            , objNameOf :: BS.ByteString }
               | Ext { signatureOf :: BS.ByteString
                     , extContentOf :: BS.ByteString }
                 deriving Show

instance Show Entry where
  show (Entry {shaOf = sha, entryPathOf = entryPath}) =
    -- "SHA: " ++ toHexes sha ++ ", " ++ "flags: " ++ show flags ++ ", " ++ show entryPath
    "SHA: " ++ toHexes sha ++ ", " ++ show entryPath

data Mode = Mode { objTypeOf :: ObjType
                 , permissionOf :: Permission }
            deriving Show

data ObjType = File | Symlink | Gitlink deriving Show

type Permission = Int

data Flags = Flags { assumeValidOf :: Bool
                   , extendedOf :: Bool
                   , stageOf :: Int
                   , nameLenOf :: Int
                   , reservedOf :: Maybe Bool
                   , skipWorktreeOf :: Maybe Bool
                   , intentToAddOf :: Maybe Bool }
             deriving Show

index :: A.Parser Index
index = do
  A.string "DIRC"
  version <- parse32Bit
  numEntries <- parse32Bit
  entries <- A.count numEntries $ entry version
  exts <- A.many' extension
  checksum <- A.take 20
  return $ Index version numEntries entries exts checksum

entry :: Int -> A.Parser Entry
entry version =
  let toNs [s, ns] = s * billion + ns
  in
   do
     ctime <- toNs <$> A.count 2 parse32Bit -- 8 bytes
     mtime <- toNs <$> A.count 2 parse32Bit -- 16 bytes
     device <- parse32Bit -- 20 bytes
     ino <- parse32Bit -- 24 bytes
     mode <- getMode <$> parse32Bit -- 28 bytes
     uid <- parse32Bit -- 32
     gid <- parse32Bit -- 36
     fileSize <- parse32Bit -- 40
     sha <- A.take 20 -- 60
     flags <- if version == 2
              then makeV2Flags <$> parse16Bit -- 62, or...
              else do [first, second] <- A.count 2 parse16Bit -- 64
                      return $ makeV3Flags first second
     entryPath <- A.take (nameLenOf flags) -- 62 or 64 + nameLen + 1 (for NUL termination)
     -- padding needs to be 1 to 8 bytes so we add 1, unless we are in version 4
     let numBytesPadding = case version of
                            2 -> (8 - (63 + nameLenOf flags) `mod` 8) `mod` 8 + 1
                            3 -> (8 - (65 + nameLenOf flags) `mod` 8) `mod` 8 + 1
                            4 -> 0
     A.take numBytesPadding
     return $ Entry ctime mtime device ino mode uid gid fileSize sha flags entryPath

cachedTree :: A.Parser Extension
cachedTree = do
  A.string "TREE"
  parse32Bit
  path <- A.takeTill (== 0)
  A.word8 0
  numEntries <- byteStringToInt <$> A.takeWhile (\c -> c <= 57 && c >= 48)
  A.word8 32
  numSubtrees <- byteStringToInt <$> A.takeWhile (\c -> c <= 57 && c >= 48)
  A.word8 10
  objName <- A.take 20
  return $ CachedTree path numEntries numSubtrees objName

extension' :: A.Parser Extension
extension' = do
  signature <- A.choice $ A.string <$> ["TREE", "REUC", "link", "UNTR"]
  len <- parse32Bit
  cont <- A.take len
  return $ Ext signature cont

extension :: A.Parser Extension
extension = A.choice [cachedTree, extension']

makeV2Flags :: Int -> Flags
makeV2Flags i =
  let assumeValid = i .&. 2^(15::Int) == 1
      extended = i .&. 2^(14::Int) == 1
      stage = i .&. (2^(13::Int) + 2^(12::Int))
      nameLen = i `mod` 2^(12::Int)
  in Flags assumeValid extended stage nameLen Nothing Nothing Nothing

makeV3Flags :: Int -> Int -> Flags
makeV3Flags first second =
  let v2 = makeV2Flags first
      reserved = second .&. 2^(15::Int) == 1
      skipWorktree = second .&. 2^(14::Int) == 1
      intentToAdd = second .&. 2^(13::Int) == 1
  in v2 { reservedOf = Just reserved
        , skipWorktreeOf = Just skipWorktree
        , intentToAddOf = Just intentToAdd }

getMode :: Int -> Mode
getMode i =
  let typeBits = i `div` 2 ^ (12 :: Int)
      permBits = i `mod` 1024
      objType = case typeBits of
                 8 -> File
                 10 -> Symlink
                 14 -> Gitlink
  in Mode objType permBits

billion :: Int
billion = 10 ^ (9 :: Int)
