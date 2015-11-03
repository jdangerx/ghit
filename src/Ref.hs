{-# LANGUAGE OverloadedStrings #-}
module Ref where

import qualified Data.ByteString.Char8 as BSC
import System.FilePath

import Data.Attoparsec.ByteString as A

import Utils

data Ref = Symbolic String
         | Direct String
           deriving Show

pRef :: A.Parser Ref
pRef = A.choice [pSymbolic, pDirect]

pSymbolic :: A.Parser Ref
pSymbolic = Symbolic . BSC.unpack <$> (A.string "ref: " *> A.takeTill (== 10))

pDirect :: A.Parser Ref
pDirect = Direct . BSC.unpack <$> (A.takeTill (== 10))

updateRef :: FilePath -> Ref -> IO ()
updateRef fp (Direct ref) = do
  gitDir <- getGitDirectory
  writeFile (gitDir </> fp) ref

readRef :: FilePath -> IO Ref
readRef fp = do
  ref <- A.parseOnly pRef <$> gitRead fp
  case ref of
   Left err -> fail err
   Right (Symbolic refPath) -> readRef refPath
   Right direct -> return direct

readSymRef :: FilePath -> IO FilePath
readSymRef fp = do
  ref <- A.parseOnly pRef <$> gitRead fp
  case ref of
   Left err -> fail err
   Right (Symbolic refPath) -> readSymRef refPath
   Right _ -> return fp

readHead :: IO Ref
readHead = readRef "HEAD"
