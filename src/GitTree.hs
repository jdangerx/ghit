{-# LANGUAGE OverloadedStrings #-}
module GitTree where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (sort)
import qualified Data.Tree as T

import qualified Data.Attoparsec.ByteString as A
import Test.QuickCheck

import Object
import Utils

data GitTree = GitTree { treeOf :: T.Tree TreeEntry }
               deriving (Eq, Show)

data TreeEntry = TreeEntry GitFileMode FilePath SHA1 | Root
                 deriving (Eq, Show)

instance GitObject GitTree where
  typeName _ = "tree"
  content gitTree = BSC.concat
                    $ sort . map
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
        fmBS <- A.takeWhile1 digit
        fp <- A.word8 32 *> A.takeTill (== 0) <* A.word8 0
        sha' <- A.take 20
        let gitFM = case fmBS of
                     "100644" -> NormalMode
                     "100755" -> ExecutableMode
                     "40000" -> DirMode
        return $ T.Node (TreeEntry gitFM (BSC.unpack fp) (SHA1 sha')) []

writeTreeRec :: GitTree -> IO ()
writeTreeRec gitTree@(GitTree (T.Node _ children)) =
  do
    write gitTree
    let childTrees = GitTree <$> filter (not . null . T.subForest) children
    void . sequence $ map writeTreeRec childTrees

serEntry :: TreeEntry -> BS.ByteString
serEntry (TreeEntry gitFM fp (SHA1 shaBS)) =
  let serGitFM ExecutableMode = "100755"
      serGitFM NormalMode = "100644"
      serGitFM DirMode = "40000"
  in BS.concat [serGitFM gitFM, " ", BSC.pack fp, "\0", shaBS]

instance Arbitrary TreeEntry where
  arbitrary = do
    fm <- elements [DirMode, ExecutableMode, NormalMode]
    fp <- arbitrary `suchThat` (\s -> length s > 3 && ('\0' `notElem` s))
    sha1 <- arbitrary
    return $ TreeEntry fm fp sha1

instance Arbitrary GitTree where
  arbitrary = GitTree <$> arbTree1Deep

arbLeaf :: Gen (T.Tree TreeEntry)
arbLeaf = do
  ent <- arbitrary
  return $ T.Node ent []

arbTree1Deep :: Gen (T.Tree TreeEntry)
arbTree1Deep = do
  subForest <- listOf1 arbLeaf
  return $ T.Node Root subForest

prop_treeRT :: GitTree -> Bool
prop_treeRT tree = Right tree == deserialize (serialize tree)
