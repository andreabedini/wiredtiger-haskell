module WiredTiger.Raw.Types where

import Foreign (Ptr)

data ConnectionImpl
data CursorImpl
data SessionImpl

newtype Connection = Connection (Ptr ConnectionImpl)
  deriving Show

newtype Cursor = Cursor (Ptr CursorImpl)
  deriving Show

newtype Session = Session (Ptr SessionImpl)
  deriving Show

