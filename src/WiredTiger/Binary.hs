module WiredTiger.Binary
  ( module WiredTiger.Binary,
    module Data.Binary,
    getInt,
    putInt,
  )
where

import Data.Binary
import WiredTiger.IntPack
import Data.Binary.Get (getLazyByteStringNul)
import Data.Binary.Put (putLazyByteString)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as T

putString :: String -> Put
putString str = putLazyByteString (T.encodeUtf8 $ T.pack str) <> putWord8 0

getString :: Get String
getString = T.unpack . T.decodeUtf8 <$> getLazyByteStringNul
