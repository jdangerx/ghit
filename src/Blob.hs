{-# LANGUAGE OverloadedStrings #-}
module Blob
       ( module Blob
       , module Object
  ) where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Data.Attoparsec.ByteString as A
import qualified System.Process as P
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Object
import Utils

data Blob = Blob BS.ByteString
            deriving (Eq, Show)

instance GitObject Blob where
  typeName _ = "blob"
  content (Blob content') = content'
  parser = do
    contLen' <- pContLen "blob"
    content' <- A.take contLen'
    return $ Blob content'

mkBlobFromFile:: FilePath -> IO Blob
mkBlobFromFile fp = liftM Blob $ BS.readFile fp

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

instance Arbitrary Blob where
  arbitrary = do
    content' <- BSC.pack <$> arbitrary
    return $ Blob content'
