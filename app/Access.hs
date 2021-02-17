module Main where

import Control.Monad (void)
import Control.Monad.Extra (whileM)
import Foreign.Ptr
import qualified Bindings as WT

main :: IO ()
main = do
  connection <- WT.open "WT_HOME" Nothing "create"
  session <- WT.connectionOpenSession connection Nothing ""
  WT.sessionCreate session "table:access" "key_format=S,value_format=S"
  cursor <- WT.sessionOpenCursor session "table:access" Nothing ""
  
  WT.cursorSetKey1 cursor "key1"
  WT.cursorSetValue1 cursor "value1"
  WT.cursorInsert cursor
  
  WT.cursorSetKey1 cursor "key2"
  WT.cursorSetValue1 cursor "value2"
  WT.cursorInsert cursor
  
  WT.cursorReset cursor
  
  WT.cursorNext cursor
  key <- WT.cursorGetKey1 cursor
  print key
  value <- WT.cursorGetValue1 cursor
  print value

  WT.cursorNext cursor
  key <- WT.cursorGetKey1 cursor
  print key
  value <- WT.cursorGetValue1 cursor
  print value

  WT.cursorNext cursor
  key <- WT.cursorGetKey1 cursor
  print key
  value <- WT.cursorGetValue1 cursor
  print value

  WT.connectionClose connection ""
