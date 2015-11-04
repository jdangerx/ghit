{-# LANGUAGE OverloadedStrings #-}
module Commit
       ( writeTree
       , commitTree
       , commit
       , Commit
       , gitTreeOf
       , parentsOf
       , authorOf
       , committerOf
       , msgOf
       , module Object
  ) where

import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromJust, isNothing)
import qualified Data.Time as Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import qualified Data.Attoparsec.ByteString as A
import Test.QuickCheck

import GitTree
import Index
import Object
import Ref
import Utils

data Commit = Commit { gitTreeOf :: SHA1
                     , parentsOf :: [SHA1]
                     , authorOf :: PersonInfo
                     , committerOf :: PersonInfo
                     , msgOf :: String }
              deriving (Eq, Show)

data PersonInfo = PersonInfo Name Email Time.UTCTime deriving (Eq, Show)
type Name = String
type Email = String

serializePersonInfo :: PersonInfo -> String
serializePersonInfo (PersonInfo name email time) =
  name ++ " <" ++ email ++ "> " ++
  Time.formatTime Time.defaultTimeLocale "%s %z" time

instance GitObject Commit where
  typeName = const "commit"
  content (Commit treeSha parents author committer msg) =
    let
      treeS = "tree " ++ hexSha treeSha
      parentLines = ("parent " ++) . hexSha <$> parents
      authorS = "author " ++ serializePersonInfo author
      committerS = "committer " ++ serializePersonInfo committer
      msgS = "\n" ++ msg
    in
      BSC.pack . unlines $ treeS : parentLines ++ [authorS, committerS, msgS]
  parser = do
    pContLen "commit"
    tree <- pTree
    parents <- A.choice [ A.many1 pParent , return [] ]
    author <- (A.string "author " A.<?> "author tag")*> pPersonInfo
    committer <- (A.string "committer " A.<?> "committer tag") *> pPersonInfo
    msg <- stripUpTo1 . BSC.unpack <$> A.takeByteString
    return $ Commit tree parents author committer msg
    where
      dropLeadingNewline ('\n' : xs) = xs
      dropLeadingNewline s = s
      stripUpTo1 =reverse . dropLeadingNewline . reverse . dropLeadingNewline

pTree :: A.Parser SHA1
pTree = fromHex . BSC.unpack <$> (A.string "tree " *> A.take 40 <* A.word8 10)

pParent :: A.Parser SHA1
pParent = fromHex . BSC.unpack <$> (A.string "parent " *> A.take 40 <* A.word8 10)

pPersonInfo :: A.Parser PersonInfo
pPersonInfo =
  let strip = unwords . words
  in
   do
     -- ord '<' == 60
     name <- strip . BSC.unpack <$> A.takeTill (== 60) <* A.word8 60
     -- ord '>' == 62
     email <- strip . BSC.unpack <$> A.takeTill (== 62) <* A.word8 62
     timeBS <- A.takeTill (== 10) <* A.word8 10
     let time =
           Time.parseTimeM True Time.defaultTimeLocale "%s %z"
           $ BSC.unpack timeBS :: Maybe Time.UTCTime
     when (isNothing time) $ fail ("invalid time string: " ++ BSC.unpack timeBS)
     return $ PersonInfo name email (fromJust time)

instance Arbitrary PersonInfo where
  arbitrary = do
    firstName <- listOf1 $ choose ('A', 'z')
    lastName <- listOf1 $ choose ('A', 'z')
    time <- posixSecondsToUTCTime . fromInteger <$> choose (0, 2446479042)
    let name = firstName ++ " " ++ lastName
    let email = firstName ++ "@" ++ lastName ++ ".com"
    return $ PersonInfo name email time

instance Arbitrary Commit where
  arbitrary = Commit
              <$> arbitrary -- tree
              <*> listOf arbitrary -- parents
              <*> arbitrary -- author
              <*> arbitrary -- committer
              <*> listOf1 arbitrary -- msg

prop_commitRT :: Commit -> Bool
prop_commitRT cmt = deserialize (serialize cmt) == Right cmt

-- actual commit-related commands
writeTree :: IO ()
writeTree = readIndex
            >>= either print
            (writeTreeRec . mkTreeFromIndex)

me :: IO PersonInfo
me = PersonInfo "John Xia" "john.danger.xia@gmail.com"
     <$> Time.getCurrentTime

commitTree :: String -> String -> IO ()
commitTree sha' msg =
  do
    indexExists <- fileInGitDir "index"
    unless indexExists $ fail "index file does not exist"
    mkCommit sha' msg >>= print

mkCommit :: String -> String -> IO Commit
mkCommit hexes msg = do
  headContent <- readHead
  let parents = case headContent of
                 Nothing -> []
                 Just something -> case something of
                   Direct hash -> [hash]
                   _ -> []
  author <- me
  committer <- me
  return $ Commit (fromHex hexes) parents author committer msg

commit :: String -> IO ()
commit msg = do
  ind <- readIndex
  case ind of
   Left err -> print err
   Right ind' -> do
     let tree = mkTreeFromIndex ind'
     writeTreeRec tree
     theCommit <- mkCommit (hexSha . sha $ tree) msg
     write theCommit
     symRef <- readSymRef "HEAD"
     updateRef symRef (Direct $ sha theCommit)
