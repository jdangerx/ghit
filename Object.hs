{-# LANGUAGE OverloadedStrings #-}

module Object where

import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Tree as T
import Data.Word (Word8)
import Numeric (readOct, showOct)
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Posix.Types

import Crypto.Hash.SHA1 (hash)
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.Attoparsec.ByteString as A
import qualified System.Process as P
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

  write :: a -> IO ()
  write obj =
    let SHA1 sha' = sha obj
        (h, t) = BS.splitAt 1 sha'
        hashPath = toHexes h </> toHexes t
    in do
      gitDir <- getGitDirectory
      let filePath = gitDir </> "objects" </> hashPath
      alreadyThere <- doesFileExist filePath
      if not alreadyThere
        then do createDirectoryIfMissing True (takeDirectory filePath)
                putStrLn $ toHexes sha'
                BL.writeFile filePath $ compressed obj
        else print ("object file already exists, skipping :)" :: String)

newtype SHA1 = SHA1 BS.ByteString
               deriving (Eq, Show)
               -- deriving Eq

-- instance Show SHA1 where
  -- show (SHA1 sha') = show $ toHexes sha'

data Blob = Blob BS.ByteString
            deriving (Eq, Show)

instance GitObject Blob where
  typeName _ = "blob"
  contLen (Blob content') = BS.length content'
  content (Blob content') = content'
  parser = do
    contLen' <- pContLen "blob"
    content' <- A.take contLen'
    return $ Blob content'

data GitTree = GitTree { treeOf :: T.Tree TreeEntry }
               deriving (Eq, Show)

data TreeEntry = TreeEntry FileMode FilePath SHA1 | Root
                 deriving (Eq, Show)

instance GitObject GitTree where
  typeName _ = "tree"
  contLen = BS.length . content
  content gitTree = BSC.concat
                    $ map
                    (\ (T.Node entry _) -> serEntry entry)
                    . T.subForest . treeOf $ gitTree

  parser = do
    contLen' <- pContLen "tree"
    if contLen' == 0
      then return . GitTree $ T.Node Root []
      else do entries <- A.many' treeEntry
              return . GitTree $ T.Node Root entries
    where
      treeEntry :: A.Parser (T.Tree TreeEntry)
      treeEntry = do
        [(perms, "")] <- readOct . BSC.unpack <$> A.takeWhile1 digit
        fp <- A.word8 32 *> A.takeTill (== 0) <* A.word8 0
        sha' <- A.take 20
        return $ T.Node (TreeEntry perms (BSC.unpack fp) (SHA1 sha')) []

serEntry :: TreeEntry -> BS.ByteString
serEntry (TreeEntry perms fp (SHA1 shaBS)) =
  BS.concat [BSC.pack $ showOct perms "", " ", BSC.pack fp, "\0", shaBS]

mkBlobFromFile:: FilePath -> IO Blob
mkBlobFromFile fp = liftM Blob $ BS.readFile fp

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

prop_treeRT :: GitTree -> Bool
prop_treeRT tree = Right tree == deserialize (serialize tree)

pContLen :: BS.ByteString -> A.Parser Int
pContLen bs = A.string bs *> A.word8 32 *> parseAsciiInt <* A.word8 0

instance Arbitrary Blob where
  arbitrary = do
    content' <- BSC.pack <$> arbitrary
    return $ Blob content'

instance Arbitrary SHA1 where
  arbitrary = SHA1 . BS.pack <$> vectorOf 20 (choose (0, 255) :: Gen Word8)

instance Arbitrary TreeEntry where
  arbitrary = do
    perms <- elements [16384, 33188]
    fp <- arbitrary `suchThat` (\s -> length s > 3 && ('\0' `notElem` s))
    sha1 <- arbitrary
    return $ TreeEntry perms fp sha1

instance Arbitrary GitTree where
  arbitrary = GitTree <$> arbTree

arbTree :: Gen (T.Tree TreeEntry)
arbTree = do
  ent <- arbitrary :: Gen TreeEntry
  subForest <- oneof [ return []
                     , listOf arbTree ]
  return $ T.Node ent subForest
