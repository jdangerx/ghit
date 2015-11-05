{-# LANGUAGE OverloadedStrings #-}
module GitTree where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (sort)
import qualified Data.Map as M
import qualified Data.Tree as T
import System.FilePath

import qualified Data.Attoparsec.ByteString as A
import Test.QuickCheck

import Index
import Utils

data GitTree = GitTree { treeOf :: T.Tree TreeEntry }
               deriving (Eq, Show)

data TreeEntry = TreeEntry GitFileMode FilePath SHA1 | Root
                 deriving (Eq, Show)

instance GitObject GitTree where
  typeName _ = "tree"
  content gitTree = BSC.concat
                    $ sort . map
                    (\ (T.Node entry' _) -> serEntry entry')
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

-- convert to and from index
fromTreeEntry :: TreeEntry -> (FilePath, Entry)
fromTreeEntry (TreeEntry fm fp sha') =
  let mode = Mode File fm
  in (fp, emptyEntry { flagsOf = (flagsOf emptyEntry) { nameLenOf = length fp }
                     , modeOf = mode
                     , shaOf = sha' })

mkIndexFromTree :: GitTree -> Index
mkIndexFromTree (GitTree tree) =
  let entries = M.fromList . map fromTreeEntry . filter (/= Root)
                $ foldr (\te ls -> case te of
                                    TreeEntry NormalMode _ _ -> te : ls
                                    TreeEntry ExecutableMode _ _ -> te : ls
                                    _ -> ls) [] tree
  in Index 2 (M.size entries) entries [] (SHA1 "")

prependDirName :: FilePath -> T.Tree TreeEntry -> T.Tree TreeEntry
prependDirName dirName (T.Node treeEntry subForest) =
  let newLabel = case treeEntry of
                  Root -> TreeEntry DirMode dirName (SHA1 "")
                  TreeEntry fm fp sha' -> TreeEntry fm (dirName </> fp) sha'
  in
   T.Node newLabel (prependDirName dirName <$> subForest)

fillOutDirEntry :: T.Tree TreeEntry -> IO (Either String (T.Tree TreeEntry))
fillOutDirEntry (T.Node (TreeEntry DirMode dirName sha') _) = do
  eitherTree <- treeFromSHA sha'
  let withDirNames = prependDirName dirName <$> eitherTree :: Either String (T.Tree TreeEntry)
  buildTree withDirNames

fillOutDirEntry t = return . return $ t

buildTree :: Either String (T.Tree TreeEntry) -> IO (Either String (T.Tree TreeEntry))
buildTree (Right (T.Node treeEntry subForest)) = do
  newSFs <- mapM fillOutDirEntry subForest
  return $ T.Node treeEntry <$> sequence newSFs
buildTree (Left err) = return $ Left err

treeFromSHA :: SHA1 -> IO (Either String (T.Tree TreeEntry))
treeFromSHA s = do
  cont <- readSHA s
  let tree = cont >>= (treeOf <$>) . readObj
  return tree

readTreeNoWrite :: SHA1 -> IO (Either String Index)
readTreeNoWrite s = do
  tree <- treeFromSHA s >>= buildTree
  return $ mkIndexFromTree . GitTree <$> tree

readTree :: SHA1 -> IO ()
readTree s = do
  ind <- readTreeNoWrite s
  indPath <- relToRoot ".git/index"
  either print (BS.writeFile indPath . writeIndex) ind

mkTreeFromIndex :: Index -> GitTree
mkTreeFromIndex (Index { entriesOf = entries }) =
  dirMapToTree . mkDirMap $ entries

dirMapToTree :: M.Map (Maybe FilePath) (M.Map FilePath Entry) -> GitTree
dirMapToTree dirMap =
  let subForest = M.foldrWithKey pairToForest [] dirMap :: [T.Tree TreeEntry]
      rootEntry = Root
  in GitTree (T.Node rootEntry subForest)

pairToForest :: Maybe FilePath -> M.Map FilePath Entry
             -> [T.Tree TreeEntry] -> [T.Tree TreeEntry]
pairToForest Nothing blobs = (++) . M.elems . M.mapWithKey mkBlobNode $ blobs 
pairToForest (Just dirName) subs = (:) (mkTreeNode dirName subs)

mkDirMap :: M.Map FilePath Entry -> M.Map (Maybe FilePath) (M.Map FilePath Entry)
mkDirMap = M.foldrWithKey getTopLevelPath M.empty

mkBlobNode :: FilePath -> Entry -> T.Tree TreeEntry
mkBlobNode fp Entry { modeOf = (Mode _ fm), shaOf = sha'} =
  let treeEntry = TreeEntry fm (takeFileName fp) sha'
  in T.Node treeEntry []

mkTreeNode :: FilePath -> M.Map FilePath Entry -> T.Tree TreeEntry
mkTreeNode fp subs =
  let dirMap = mkDirMap subs
      tree = dirMapToTree dirMap
      sha' = sha tree
      treeEntry = TreeEntry DirMode fp sha'
  in T.Node treeEntry (T.subForest $ treeOf tree)

getTopLevelPath :: FilePath -> Entry
                -> M.Map (Maybe FilePath) (M.Map FilePath Entry)
                -> M.Map (Maybe FilePath) (M.Map FilePath Entry)
getTopLevelPath fp ent oldMap =
  let (h, t) = break isPathSeparator fp
  in
   if pathSeparator `elem` fp
   then M.insertWith M.union (Just h) (M.fromList [(drop 1 t, ent)]) oldMap
   else M.insertWith M.union Nothing (M.fromList [(fp, ent)]) oldMap


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
