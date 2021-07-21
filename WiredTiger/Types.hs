module WiredTiger.Types where

import Foreign (Ptr)

newtype Connection = Connection (Ptr ())
  deriving Show

newtype Cursor = Cursor (Ptr ())
  deriving Show

newtype Session = Session (Ptr ())
  deriving Show

