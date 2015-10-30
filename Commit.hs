{-# LANGUAGE OverloadedStrings #-}
module Commit where

import qualified Data.Map as M
import qualified Data.Tree as T
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import System.FilePath

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Index
import Object
import Utils

makeTreeFromIndex :: Index -> GitTree
makeTreeFromIndex (Index { entriesOf = entries }) =
  indexEntriesToTree entries

indexEntriesToTree :: [Entry] -> GitTree
indexEntriesToTree = dirMapToTree . toMap

dirMapToTree :: M.Map (Maybe FilePath) [Entry] -> GitTree
dirMapToTree dirMap =
  let subForest = concatMap snd . M.toList . M.mapWithKey pairToForest $ dirMap
      rootEntry = Root
  in GitTree (T.Node rootEntry subForest)

pairToForest :: Maybe FilePath -> [Entry] -> T.Forest TreeEntry
pairToForest Nothing blobs = mkBlobNode <$> blobs
pairToForest (Just dirName) subs = [mkTreeNode dirName subs]

toMap :: [Entry] -> M.Map (Maybe FilePath) [Entry]
toMap = M.fromListWith (++) . map getTopLevelPath

mkBlobNode :: Entry -> T.Tree TreeEntry
mkBlobNode Entry { modeOf = (Mode _ fm), shaOf = sha', entryPathOf = fp } =
  let treeEntry = TreeEntry fm (takeFileName fp) sha'
  in T.Node treeEntry []

mkTreeNode :: FilePath -> [Entry] -> T.Tree TreeEntry
mkTreeNode fp subs =
  let dirMap = toMap subs
      tree = dirMapToTree dirMap
      sha' = sha tree
      treeEntry = TreeEntry DirMode fp sha'
  in T.Node treeEntry (T.subForest $ treeOf tree)

getTopLevelPath :: Entry -> (Maybe FilePath, [Entry])
getTopLevelPath e@(Entry { entryPathOf = path }) =
  let (h, t) = break isPathSeparator path
  in
   if pathSeparator `elem` path
   then (Just h, [e { entryPathOf = drop 1 t }])
   else (Nothing, [e])

writeTree :: IO ()
writeTree = readIndex
             -- >>= either print (putStrLn . T.drawTree . (show <$>) . treeOf . makeTreeFromIndex)
             >>= either print
             (BL.putStr . compressed . makeTreeFromIndex)
