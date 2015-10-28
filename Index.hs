{-# LANGUAGE OverloadedStrings #-}
module Index where

import Control.Monad
import Data.Bits ((.&.), shift, testBit, bit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (sortOn)

import Crypto.Hash.SHA1 (hash)
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.Combinator (lookAhead)
import qualified Test.QuickCheck as QC
import System.FilePath
import System.Posix.Files

import Object
import Utils

-- https://github.com/git/git/blob/master/Documentation/technical/index-format.txt

data Index = Index { versionOf :: Int
                   , numEntriesOf :: Int
                   , entriesOf :: [Entry]
                   , extensionsOf :: [Extension]
                   , checksumOf :: SHA1
  }
           deriving (Eq, Show)

emptyIndex :: Index
emptyIndex = updateChecksum $ Index 2 0 [] [] (SHA1 "")

instance QC.Arbitrary Index where
  arbitrary = do
    -- version <- QC.choose (2, 4)
    let version = 2
    numEntries <- QC.arbitrary `QC.suchThat` (> 0)
    entries <- QC.vectorOf numEntries QC.arbitrary
    let extensions = []
    checksum <- QC.arbitrary
    return . updateChecksum
      $ Index version numEntries entries extensions checksum

data Entry = Entry { ctimeOf :: Int
                   , mtimeOf :: Int
                   , deviceOf :: Int
                   , inoOf :: Int
                   , modeOf :: Mode
                   , uidOf :: Int
                   , gidOf :: Int
                   , fileSizeOf :: Int
                   , shaOf :: SHA1
                   , flagsOf :: Flags
                   , entryPathOf :: FilePath
  }
             deriving (Eq, Show)

instance QC.Arbitrary Entry where
  arbitrary = do
    ctime <- QC.arbitrary `QC.suchThat` (> 0)
    mtime <- QC.arbitrary `QC.suchThat` (> 0)
    device <- QC.arbitrary `QC.suchThat` (> 0)
    ino <- QC.arbitrary `QC.suchThat` (> 0)
    mode <- QC.arbitrary
    uid <- QC.arbitrary `QC.suchThat` (> 0)
    gid <- QC.arbitrary `QC.suchThat` (> 0)
    fileSize' <- QC.arbitrary `QC.suchThat` (> 0)
    sha' <- QC.arbitrary
    flagsBadNameLen <- QC.arbitrary
    entryPath <- QC.arbitrary `QC.suchThat` (\s -> not (null s) && '\0' `notElem` s)
    let flags = flagsBadNameLen {nameLenOf = length entryPath}
    return $ Entry ctime mtime device ino mode uid gid fileSize' sha' flags entryPath

data Extension = Ext { signatureOf :: BS.ByteString
                     , extContentOf :: BS.ByteString }
                 deriving (Eq, Show)

instance QC.Arbitrary Extension where
  arbitrary = Ext <$> QC.elements ["TREE", "REUC", "link", "UNTR"] <*> (BSC.pack <$> QC.arbitrary)

data Mode = Mode { objTypeOf :: ObjType
                 , permissionOf :: Permission }
            deriving (Eq, Show)

instance QC.Arbitrary Mode where
  arbitrary = Mode <$> QC.elements [File, Symlink, Gitlink] <*> QC.arbitrary

data ObjType = File | Symlink | Gitlink deriving (Eq, Show)

data Permission = P755 | P644
                         deriving (Eq, Show)

instance QC.Arbitrary Permission where arbitrary = QC.elements [P755, P644]

data Flags = Flags { assumeValidOf :: Bool
                   , extendedOf :: Bool
                   , stageOf :: Int
                   , nameLenOf :: Int
                   , reservedOf :: Maybe Bool
                   , skipWorktreeOf :: Maybe Bool
                   , intentToAddOf :: Maybe Bool }
             deriving (Eq, Show)

-- only version 2 flags - no version 3 flags yet
instance QC.Arbitrary Flags where
  arbitrary = do
    assumeValid <- QC.arbitrary
    let extended = False
    stage <- QC.choose (0, 3)
    nameLen <- QC.arbitrary `QC.suchThat` (> 0)
    return $ Flags assumeValid extended stage nameLen Nothing Nothing Nothing

-- Parsers
index :: A.Parser Index
index = do
  fullContent <- lookAhead A.takeByteString
  A.string "DIRC"
  let noChecksum = BS.take (BS.length fullContent - 20) fullContent
  version <- parse32Bit
  numEntries <- parse32Bit
  entries <- A.count numEntries $ entry version
  exts <- A.many' extension
  checksum <- SHA1 <$> A.take 20
  when (SHA1 (hash noChecksum) /= checksum)
    $ fail ("checksum for index failed, content was: " ++ BSC.unpack noChecksum)
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
     fileSize' <- parse32Bit -- 40
     sha' <- SHA1 <$> A.take 20 -- 60
     flags <- if version == 2
              then makeV2Flags <$> parse16Bit -- 62, or...
              else do [first, second] <- A.count 2 parse16Bit -- 64
                      return $ makeV3Flags first second
     entryPath <- BSC.unpack <$> A.take (nameLenOf flags) -- 62 or 64 + nameLen + 1 (for NUL termination)
     let numBytesPadding = case version of
                            2 -> 8 - (62 + nameLenOf flags) `mod` 8
                            3 -> 8 - (64 + nameLenOf flags) `mod` 8
                            4 -> 0
     A.take numBytesPadding
     return $ Entry ctime mtime device ino mode uid gid fileSize' sha' flags entryPath

otherExtensions :: A.Parser Extension
otherExtensions = do
  signature <- A.choice $ A.string <$> ["TREE", "REUC", "link", "UNTR"]
  len <- parse32Bit
  cont <- A.take len
  return $ Ext signature cont

extension :: A.Parser Extension
extension = A.choice [otherExtensions]

-- Writers

writeIndex :: Index -> BS.ByteString
writeIndex (Index version numEntries entries exts (SHA1 sha')) =
  BS.concat $
  [ "DIRC"
  , pack32BitInt version
  , pack32BitInt numEntries] ++
  map writeEntry entries ++
  map writeExtension exts ++
  [sha']

writeEntry :: Entry -> BS.ByteString
writeEntry (Entry
            ctime mtime device ino mode uid gid fileSize' (SHA1 sha') flags
            entryPath) =
  let unpadded = BS.concat [ pack32BitInt $ ctime `shift` (-32)
                           , pack32BitInt ctime
                           , pack32BitInt $ mtime `shift` (-32)
                           , pack32BitInt mtime
                           , pack32BitInt device
                           , pack32BitInt ino
                           , writeMode mode
                           , pack32BitInt uid
                           , pack32BitInt gid
                           , pack32BitInt fileSize'
                           , sha'
                           , writeFlags flags
                           , BSC.pack entryPath ]
      padding = BSC.replicate (8 - BS.length unpadded `mod` 8) '\0'
  in BS.append unpadded padding

writeExtension :: Extension -> BS.ByteString
writeExtension (Ext sig cont) =
  BS.concat [sig, pack32BitInt (BS.length cont), cont]


-- edit index
updateIndex :: GitObject a => Index -> FilePath -> a -> IO Index
updateIndex ind fp obj =
  addEntry ind <$> makeEntry fp obj

readIndex :: IO (Either String Index)
readIndex =
  liftM (A.parseOnly index) $ liftM (</> "index") getGitDirectory >>= BS.readFile


addEntry :: Index -> Entry -> Index
addEntry ind@(Index {numEntriesOf = numEntries, entriesOf = entries}) ent =
  let newNEntries = numEntries + 1
      newEntriesUnsorted = if ent `notElem` entries then entries ++ [ent] else entries
      newEntries = sortOn (\e -> entryPathOf e ++ show (stageOf (flagsOf e)))
                   newEntriesUnsorted
      indWithNewEntries =
        ind { numEntriesOf = newNEntries, entriesOf = newEntries }
  in
   updateChecksum indWithNewEntries

makeEntry :: GitObject a => FilePath -> a -> IO Entry
makeEntry fpRelToRepo obj = do
  repoRootDir <- getRepoRootDir
  fileStatus <- getFileStatus (repoRootDir </> fpRelToRepo)
  let mode = Mode { objTypeOf = if isSymbolicLink fileStatus
                                then Symlink
                                else File
                  , permissionOf = if fileMode fileStatus == ownerExecuteMode
                                   then P755
                                   else P644
        }
  let flags = Flags { assumeValidOf = False
                    , extendedOf = False
                    , stageOf = 0
                    , nameLenOf = length fpRelToRepo
                    , reservedOf = Nothing
                    , skipWorktreeOf = Nothing
                    , intentToAddOf = Nothing}
  return Entry { ctimeOf = fromEnum $ statusChangeTimeHiRes fileStatus
               , mtimeOf = fromEnum $ modificationTimeHiRes fileStatus
               , deviceOf = fromEnum $ deviceID fileStatus
               , inoOf = fromEnum $ fileID fileStatus
               , modeOf = mode
               , uidOf = fromEnum $ fileOwner fileStatus
               , gidOf = fromEnum $ fileGroup fileStatus
               , fileSizeOf = fromEnum $ fileSize fileStatus
               , shaOf = sha obj
               , flagsOf = flags
               , entryPathOf = fpRelToRepo }
-- tests

prop_indRT :: Index -> Bool
prop_indRT ind = A.parseOnly index (writeIndex ind) == Right ind

prop_entryV2RT :: Entry -> Bool
prop_entryV2RT entry' = A.parseOnly (entry 2) (writeEntry entry') == Right entry'

prop_extRT :: Extension -> Bool
prop_extRT ext = A.parseOnly extension (writeExtension ext) == Right ext

prop_flagsV2RT :: Flags -> Bool
prop_flagsV2RT flags = Right flags == (makeV2Flags <$> A.parseOnly parse16Bit (writeFlags flags))


showIndex :: FilePath -> IO ()
showIndex fp = BS.readFile fp >>= print . A.parseOnly index

-- Utils
makeV2Flags :: Int -> Flags
makeV2Flags i =
  let assumeValid = i `testBit` 15
      extended = i `testBit` 14
      stage = (i .&. (bit 13 + bit 12)) `shift` (-12)
      nameLen = i `mod` bit 12
  in Flags assumeValid extended stage nameLen Nothing Nothing Nothing

makeV3Flags :: Int -> Int -> Flags
makeV3Flags first second =
  let v2 = makeV2Flags first
      reserved = second `testBit` 15
      skipWorktree = second `testBit` 14
      intentToAdd = second `testBit` 13
  in v2 { reservedOf = Just reserved
        , skipWorktreeOf = Just skipWorktree
        , intentToAddOf = Just intentToAdd }

writeFlags :: Flags -> BS.ByteString
writeFlags (Flags assumeValid extended stage nameLen Nothing Nothing Nothing) =
  let assumeInt = if assumeValid then 1 `shift` 15 else 0
      extInt = if extended then 1 `shift` 14 else 0
      stageInt = stage `shift` 12
      first16 = assumeInt + extInt + stageInt + nameLen
  in pack16BitInt first16
writeFlags (Flags assumeValid extended stage nameLen
            (Just reserved) (Just skipWorktree) (Just intentToAdd)) =
  let assumeInt = if assumeValid then 1 `shift` 15 else 0
      extInt = if extended then 1 `shift` 14 else 0
      stageInt = stage `shift` 12
      first16 = assumeInt + extInt + stageInt + nameLen
      reservedInt = if reserved then 1 `shift` 15 else 0
      skipWorktreeInt = if skipWorktree then 1 `shift` 14 else 0
      intentToAddInt = if intentToAdd then 1 `shift` 13 else 0
      second16 = reservedInt + skipWorktreeInt + intentToAddInt
  in pack32BitInt $ first16 `shift` 16 + second16

getMode :: Int -> Mode
getMode i =
  let typeBits = i `shift` (-12)
      permBits = case i `mod` 1024 of
                  493 -> P755
                  420 -> P644
                  _ -> error "Invalid permission bits."
      objType = case typeBits of
                 8 -> File
                 10 -> Symlink
                 14 -> Gitlink
  in Mode objType permBits

writeMode :: Mode -> BS.ByteString
writeMode (Mode objType perms) =
  let typeBits = case objType of
                  File -> 8
                  Symlink -> 10
                  Gitlink -> 14
      permBits = case perms of
                  P755 -> 493
                  P644 -> 420
  in
   pack32BitInt $ typeBits `shift` 12 + permBits

updateChecksum :: Index -> Index
updateChecksum ind =
  let oldBS = writeIndex ind
      noChecksum = BS.take (BS.length oldBS - 20) oldBS
      newChecksum = hash noChecksum
  in ind {checksumOf = SHA1 newChecksum}

billion :: Int
billion = 10 ^ (9 :: Int)
