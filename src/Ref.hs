{-# LANGUAGE OverloadedStrings #-}
module Ref where

import qualified Data.ByteString.Char8 as BSC
import System.FilePath

import Data.Attoparsec.ByteString as A

import Utils

data Ref = Symbolic FilePath
         | Direct SHA1
           deriving Show

pRef :: A.Parser Ref
pRef = A.choice [pSymbolic, pDirect]

pSymbolic :: A.Parser Ref
pSymbolic = Symbolic . BSC.unpack <$> (A.string "ref: " *> A.takeTill (== 10))

pDirect :: A.Parser Ref
pDirect = Direct . fromHex . BSC.unpack <$> A.takeTill (== 10)

updateRef :: FilePath -> Ref -> IO ()
updateRef fp (Direct ref) = do
  gitDir <- getGitDirectory
  writeFile (gitDir </> fp) (hexSha ref)

readRef :: FilePath -> IO (Maybe Ref)
readRef fp = do
  ref <- (>>= A.parseOnly pRef) <$> gitRead fp
  case ref of
   Right (Symbolic refPath) -> readRef refPath
   Right direct -> return $ Just direct
   Left "file not found" -> return Nothing
   Left err -> fail err

readSymRef :: FilePath -> IO FilePath
readSymRef fp = do
  ref <- (>>= A.parseOnly pRef) <$> gitRead fp
  case ref of
    Right (Symbolic refPath) -> readSymRef refPath
    -- Left "file not found" -> return $ Just fp
    _ -> return fp
    -- Left err -> fail err

readHead :: IO (Maybe Ref)
readHead = readRef "HEAD"
