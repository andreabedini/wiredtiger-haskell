{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified WiredTiger.Bindings as WT
import Control.Concurrent (threadDelay)
import System.Mem
import qualified Data.ByteString.Char8 as C
import Data.Char (toUpper)


main :: IO ()
main = do
  connection <- WT.open "WT_HOME" (Just "create")
  session <- WT.connectionOpenSession connection Nothing

  WT.sessionCreate session "table:access" (Just "key_format=S,value_format=S")
  cursor <- WT.sessionOpenCursor session "table:access" Nothing

  do
    WT.cursorSetKey cursor $ C.map toUpper "key1aaaaaaaaaaaaaaaaaaaa"
    WT.cursorSetValue cursor $ C.map toUpper "value1aaaaaaaaaaaaaaaaaaaa"

  threadDelay 50000

  performGC

  performMinorGC

  threadDelay 50000

  WT.cursorInsert cursor

  WT.connectionClose connection Nothing
