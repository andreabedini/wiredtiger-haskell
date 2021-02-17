module Main where

import Control.Monad (void)
import Control.Monad.Extra (whileM)
import Foreign.Ptr
import qualified Bindings as WT

main :: IO ()
main = do
  (res, connection) <- WT.open "WT_HOME" Nothing "create"
  print res
  (res, session) <- WT.connectionOpenSession connection Nothing ""
  print res
  res <- WT.sessionCreate session "table:access" "key_format=S,value_format=S"
  print res
  (res, cursor) <- WT.sessionOpenCursor session "table:access" Nothing ""
  print res
  WT.cursorSetKey1 cursor "key1"
  WT.cursorSetValue1 cursor "value1"
  res <- WT.cursorInsert cursor
  print res
  WT.cursorReset cursor
  whileM $ do
    res <- WT.cursorNext cursor
    print res
    (res, key) <- WT.cursorGetKey1 cursor
    print res
    print key
    (res, key) <- WT.cursorGetValue1 cursor
    print res
    print key
    return $ res /= 0
  res <- WT.connectionClose connection ""
  print res
