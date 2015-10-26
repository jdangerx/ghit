{-# LANGUAGE OverloadedStrings #-}
module Add where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import Object
import Utils


hashObject :: Object -> (String, BS.ByteString)
hashObject (Blob content) =
  let header = BS.concat ["blob ", intToByteString $ BS.length content, "\0"]
      fullBS = BS.concat [header, content]
  in (toHexes . SHA1.hash $ fullBS, fullBS)
hashObject _ = error "unsupported object type"

-- write index
