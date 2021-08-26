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
import Data.Binary.Put (putStringUtf8)
import Data.ByteString.Lazy.UTF8 (toString)

putString :: String -> Put
putString str = putStringUtf8 str <> putWord8 0

getString :: Get String
getString = toString <$> getLazyByteStringNul
