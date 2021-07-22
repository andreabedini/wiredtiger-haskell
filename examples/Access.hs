{-# LANGUAGE OverloadedStrings #-}
import Control.Exception
import qualified WiredTiger.Raw as WT
import qualified Data.ByteString.Char8 as B
import Data.List (intercalate)


main :: IO ()
main = do
  connection <- WT.open "WT_HOME" (Just "in_memory")
  session <- WT.connectionOpenSession connection Nothing

  WT.sessionCreate session "table:access" $ Just $ intercalate "," ["key_format=S", "value_format=S"]
  cursor <- WT.sessionOpenCursor session "table:access" Nothing

  WT.cursorSetKey cursor "key1"
  WT.cursorSetValue cursor "value1"
  WT.cursorInsert cursor

  WT.cursorReset cursor

  whileM $
    handle (\WT.WiredTigerExceptionNotFound -> return False) $ do
      WT.cursorNext cursor
      key <- WT.cursorGetKey cursor
      value <- WT.cursorGetValue cursor
      B.putStrLn $ "Got record: " <> key <> " : " <> value
      return True

  WT.connectionClose connection Nothing

whileM :: IO Bool -> IO ()
whileM m = m >>= \r -> if r then whileM m else pure ()
