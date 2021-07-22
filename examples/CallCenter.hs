{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module CallCenter where

import Control.Exception
import Data.Serialize

import qualified WiredTiger.Raw as WT
import qualified WiredTiger.Schema as WT
import Control.Monad (forM_)
import Data.List (intercalate)

data Customer = Customer
  { customerName :: String
  , customerAddress :: String
  , customerPhone :: String
  } deriving Show

instance Serialize Customer where
  put Customer { customerName, customerAddress, customerPhone }
    = do
        WT.putString customerName
        WT.putString customerAddress
        WT.putString customerPhone

  get = Customer <$> WT.getString <*> WT.getString <*> WT.getString

customers :: [Customer]
customers =
  [ Customer "Professor Oak" "LeafGreen Avenue" "123-456-7890"
  , Customer "Lorelei" "Sevii Island" "098-765-4321"
  ]

data Call = Call
  { callDate :: Int
  , callCustomerId :: Int
  , callEmployerId :: Int
  , callType :: String
  , callNotes :: String
  } deriving Show

instance Serialize Call where
  put Call { callDate, callCustomerId, callEmployerId, callType, callNotes }
    = do
        WT.putInt callDate
        WT.putInt callCustomerId
        WT.putInt callEmployerId
        WT.putString callType
        WT.putString callNotes

  get = Call
    <$> (fromIntegral <$> WT.getInt)
    <*> (fromIntegral <$> WT.getInt)
    <*> (fromIntegral <$> WT.getInt)
    <*> WT.getString
    <*> WT.getString

calls :: [Call]
calls =
  [ Call 32 1 2 "billing" "unavailable"
  , Call 33 1 2 "billing" "available"
  , Call 34 1 2 "reminder" "unavailable"
  , Call 35 1 2 "reminder" "available"
  ]

withConnection :: String -> Maybe String -> (WT.Connection -> IO c) -> IO c
withConnection a b = bracket (WT.open a b) (`WT.connectionClose` Nothing)

withSession :: WT.Connection -> Maybe String -> (WT.Session -> IO c) -> IO c
withSession connection mConfig = bracket (WT.connectionOpenSession connection mConfig) (`WT.sessionClose` Nothing)

withCursor :: WT.Session -> String -> Maybe String -> (WT.Cursor -> IO c) -> IO c
withCursor session uri mConfig = bracket (WT.sessionOpenCursor session uri mConfig) WT.cursorClose

main :: IO ()
main = do
  withConnection "WT_HOME" (Just "in_memory") $ \connection ->
    withSession connection Nothing $ \session -> do

      WT.sessionCreate session "table:customers" $ Just $ intercalate ","
        [ "key_format=r"
        , "value_format=SSS"
        , "columns=(id,name,address,phone)"
        , "colgroups=(main,address)"
        ]

      WT.sessionCreate session "colgroup:customers:main" $ Just "columns=(name,phone)"
      WT.sessionCreate session "colgroup:customers:address" $ Just "columns=(address)"
      WT.sessionCreate session "index:customers:phone" $ Just "columns=(phone)"

      withCursor session "table:customers" (Just "append") $ \cursor ->
        forM_ customers $ \customer -> do
            WT.cursorSetValue cursor (encode customer)
            WT.cursorInsert cursor

      WT.sessionCreate session "table:calls" $ Just $ intercalate ","
        [ "key_format=r"
        , "value_format=qrrSS"
        , "columns=(id,call_date,cust_id,emp_id,call_type,notes)"
        ]

      WT.sessionCreate session "index:calls:cust_date" $ Just "columns=(cust_id,call_date)"

      withCursor session "table:calls" (Just "append") $ \cursor ->
        forM_ calls $ \call -> do
          WT.cursorSetValue cursor (encode call)
          WT.cursorInsert cursor
