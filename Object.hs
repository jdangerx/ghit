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

  deserialize :: BS.ByteString -> Either String a
  deserialize = A.parseOnly parser

  compressed :: a -> BL.ByteString
  compressed = Zlib.compress . BL.fromStrict . serialize

  sha :: a -> SHA1
  sha = SHA1 . hash . serialize

newtype SHA1 = SHA1 BS.ByteString
               deriving (Eq, Show)

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

prop_blobRT :: Blob -> Bool
prop_blobRT blob = Right blob == deserialize (serialize blob)

prop_blobGit :: Blob -> Property
prop_blobGit blob =
  monadicIO $ do
    gitHashed <- run (P.readProcess
                      "git"
                      ["hash-object", "--stdin"]
                      (BSC.unpack $ content blob))
    let gitRead = deserialize (BSC.pack gitHashed) :: Either String Blob
    return $ gitRead == Right blob

pContLen :: BS.ByteString -> A.Parser Int
pContLen bs = A.string bs *> A.word8 32 *> parseAsciiInt <* A.word8 0


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

serEntry :: TreeEntry -> BS.ByteString
serEntry (TreeEntry perms fp (SHA1 shaBS)) =
  BS.concat [perms, " ", BSC.pack fp, "\0", shaBS]

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

prop_treeRT :: Tree -> Bool
prop_treeRT tree = Right tree == deserialize (serialize tree)

prop_treeGit :: Tree -> Property
prop_treeGit tree =
  monadicIO $ do
    gitHashed <- run (P.readProcess
                      "git"
                      ["hash-object", "--stdin"]
                      (BSC.unpack $ content tree))
    let gitRead = deserialize (BSC.pack gitHashed) :: Either String Tree
    return $ gitRead == Right tree
