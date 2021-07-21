{-# LANGUAGE OverloadedStrings #-}
module Access where

import Control.Exception
import Control.Monad.Extra (whileM)
import Data.IORef
import qualified WiredTiger.Bindings as WT
import Test.Hspec


access :: Spec
access =
  describe "access" $
    it "works" $ do
      connection <- WT.open "WT_HOME" (Just "in_memory")
      print connection

      session <- WT.connectionOpenSession connection Nothing
      print session

      WT.sessionCreate session "table:access" (Just "key_format=S,value_format=S")
      cursor <- WT.sessionOpenCursor session "table:access" Nothing

      WT.cursorSetKey cursor "key1"
      WT.cursorSetValue cursor "value1"
      WT.cursorInsert cursor

      WT.cursorReset cursor

      cRef <- newIORef (0 :: Int)

      whileM $
        handle (\WT.WiredTigerExceptionNotFound -> return False) $ do
          WT.cursorNext cursor
          key <- WT.cursorGetKey cursor
          key `shouldBe` "key1"
          value <- WT.cursorGetValue cursor
          value `shouldBe` "value1"
          modifyIORef cRef (+1)
          return True

      c <- readIORef cRef
      c `shouldBe` 1

      WT.connectionClose connection Nothing
