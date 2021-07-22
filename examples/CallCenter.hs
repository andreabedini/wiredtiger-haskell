{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module CallCenter where

import Control.Exception
import Data.Serialize

import qualified WiredTiger.Bindings as WT
import qualified WiredTiger.Schema as WT
import Control.Monad (forM_)

data Customer = Customer
  { customerName :: WT.SS
  , customerAddress :: WT.SS
  , customerPhone :: WT.SS
  } deriving Show

instance Serialize Customer where
  put Customer { customerName, customerAddress, customerPhone }
    = do
        put customerName
        put customerAddress
        put customerPhone

  get = Customer <$> get <*> get <*> get

customers :: [Customer]
customers =
  [ Customer "Professor Oak" "LeafGreen Avenue" "123-456-7890"
  , Customer "Lorelei" "Sevii Island" "098-765-4321"
  ]

data Call = Call
  { callDate :: WT.QQ
  , callCustomerId :: WT.QQ
  , callEmployerId :: WT.QQ
  , callType :: WT.SS
  , callNotes :: WT.SS
  } deriving Show

instance Serialize Call where
  put Call { callDate, callCustomerId, callEmployerId, callType, callNotes }
    = do
        put callDate
        put callCustomerId
        put callEmployerId
        put callType
        put callNotes

  get = Call <$> get <*> get <*> get <*> get <*> get

calls :: [Call]
calls =
  [ Call (WT.QQ 32) (WT.QQ 1) (WT.QQ 2) "billing" "unavailable"
  , Call (WT.QQ 33) (WT.QQ 1) (WT.QQ 2) "billing" "available"
  , Call (WT.QQ 34) (WT.QQ 1) (WT.QQ 2) "reminder" "unavailable"
  , Call (WT.QQ 35) (WT.QQ 1) (WT.QQ 2) "reminder" "available"
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

      WT.sessionCreate session "table:customers" $ Just $ mconcat
        [ "key_format=r,value_format=SSS,"
        , "columns=(id,name,address,phone),"
        , "colgroups=(main,address)"
        ]

      WT.sessionCreate session "colgroup:customers:main" $ Just "columns=(name,phone)"
      WT.sessionCreate session "colgroup:customers:address" $ Just "columns=(address)"
      WT.sessionCreate session "index:customers:phone" $ Just "columns=(phone)"

      withCursor session "table:customers" (Just "append") $ \cursor ->
        forM_ customers $ \customer -> do
            WT.cursorSetValue cursor (encode customer)
            WT.cursorInsert cursor

      WT.sessionCreate session "table:calls" $ Just $ mconcat
        [ "key_format=r,value_format=qrrSS,"
        , "columns=(id,call_date,cust_id,emp_id,call_type,notes)"
        ]

      WT.sessionCreate session "index:calls:cust_date" $ Just "columns=(cust_id,call_date)"

      withCursor session "table:calls" (Just "append") $ \cursor ->
        forM_ calls $ \call -> do
          WT.cursorSetValue cursor (encode call)
          WT.cursorInsert cursor
