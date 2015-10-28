{-# LANGUAGE OverloadedStrings #-}

module Object where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Word (Word8)
import qualified System.Process as P

import Crypto.Hash.SHA1 (hash)
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.Attoparsec.ByteString as A
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Utils

class GitObject a where
  typeName :: a -> BS.ByteString
  contLen :: a -> Int
  content :: a -> BS.ByteString
  parser :: A.Parser a

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

newtype SHA1 = SHA1 BS.ByteString
               deriving (Eq, Show)

-- instance Show SHA1 where show (SHA1 shaBS) = toHexes shaBS

data Blob = Blob Int BS.ByteString
            deriving (Eq, Show)

instance GitObject Blob where
  typeName _ = "blob"
  contLen (Blob contLen' _) = contLen'
  content (Blob _ content') = content'
  parser = do
    contLen' <- pContLen "blob"
    content' <- A.take contLen'
    return $ Blob contLen' content'

instance Arbitrary Blob where
  arbitrary = do
    content' <- BSC.pack <$> arbitrary
    return $ Blob (BS.length content') content'

data Tree = Tree Int [TreeEntry]
            deriving (Eq, Show)

data TreeEntry = TreeEntry Perms FilePath SHA1
                 deriving (Eq, Show)

type Perms = BS.ByteString

instance GitObject Tree where
  typeName _ = "tree"
  contLen (Tree i _) = i
  content (Tree _ entries) =
    BS.concat $ serEntry <$> entries

  parser = do
    contLen' <- pContLen "tree"
    if contLen' == 0
      then return $ Tree 0 []
      else do entries <- A.many' treeEntry
              return $ Tree contLen' entries
    where
      treeEntry :: A.Parser TreeEntry
      treeEntry = do
        perms <- A.takeWhile1 digit A.<?> "perms"
        fp <- A.word8 32 *> A.takeTill (== 0) <* A.word8 0 A.<?> "filepath"
        sha' <- A.take 20 A.<?> "sha"
        return $ TreeEntry perms (BSC.unpack fp) (SHA1 sha')

instance Arbitrary SHA1 where
  arbitrary = SHA1 . BS.pack <$> vectorOf 20 (choose (0, 255) :: Gen Word8)

instance Arbitrary TreeEntry where
  arbitrary = do
    perms <- elements ["40000", "100644"]
    fp <- arbitrary `suchThat` (\s -> length s > 3 && ('\0' `notElem` s))
    sha1 <- arbitrary
    return $ TreeEntry perms fp sha1

instance Arbitrary Tree where
  arbitrary = do
    entries <- arbitrary :: Gen [TreeEntry]
    let size = sum $ BS.length . serEntry <$> entries
    return $ Tree size entries

serEntry :: TreeEntry -> BS.ByteString
serEntry (TreeEntry perms fp (SHA1 shaBS)) =
  BS.concat [perms, " ", BSC.pack fp, "\0", shaBS]


makeTree :: FilePath -> IO Tree
makeTree = error "not implemented yet"

makeBlob:: FilePath -> IO Blob
makeBlob fp = do
  cont <- BS.readFile fp
  return $ Blob (BS.length cont) cont

-- tests

prop_blobRT :: Blob -> Bool
prop_blobRT blob = Right blob == deserialize (serialize blob)

-- round-trip through git: get content, make git hash it, see if our hashes match
prop_blobGit :: Blob -> Property
prop_blobGit blob =
  monadicIO $ do
    gitHashed <- run (P.readProcess
                      "git"
                      ["hash-object", "-t", "blob", "--stdin"]
                      (BSC.unpack $ content blob))
    let SHA1 shaBS = sha blob
    return $  gitHashed == BSC.unpack shaBS

prop_treeRT :: Tree -> Bool
prop_treeRT tree = Right tree == deserialize (serialize tree)

pContLen :: BS.ByteString -> A.Parser Int
pContLen bs = A.string bs *> A.word8 32 *> parseAsciiInt <* A.word8 0

