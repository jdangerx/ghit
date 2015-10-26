{-# LANGUAGE OverloadedStrings #-}
module Add where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import Object
import Utils


hashObject :: Object -> (String, BS.ByteString)
hashObject obj =
  let fullBS = serializeObj obj
  in (toHexes . SHA1.hash $ fullBS, fullBS)

-- write index
