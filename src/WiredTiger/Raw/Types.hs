module WiredTiger.Raw.Types where

import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Foreign (Ptr)

data ConnectionImpl

data CursorImpl

data SessionImpl

newtype Connection = Connection (Ptr ConnectionImpl)
  deriving (Show)

--
-- The IORefs are to keep the value alive for the Haskell GC. I am assuming
-- ByteStrings pointers are pinned anyway, so GC won't move them.
--

data Cursor = Cursor
  { _cursorPtr :: Ptr CursorImpl,
    _cursorKey :: IORef (Maybe ByteString),
    _cursorValue :: IORef (Maybe ByteString)
  }

newtype Session = Session (Ptr SessionImpl)
  deriving (Show)
