module Main where

import Control.Monad (void)
import Foreign.Ptr
import qualified Bindings as WT

main :: IO ()
main = do
  (res, connection) <- WT.open "WT_HOME" Nothing "create"
  print res
  (res, session) <- WT.connectionOpenSession connection Nothing ""
  print res
  res <- WT.sessionClose session ""
  print res
  res <- WT.connectionClose connection ""
  print res
