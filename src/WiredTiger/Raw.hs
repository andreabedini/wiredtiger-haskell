{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module WiredTiger.Raw
  ( open,
    WiredTigerException(..),

    -- * Connection
    Connection,
    connectionClose,
    connectionDebugInfo,
    connectionGetHome,
    connectionIsNew,
    connectionOpenSession,
    connectionReconfigure,

    -- * Session
    Session,
    sessionAlter,
    sessionBeginTransaction,
    sessionClose,
    sessionCommitTransaction,
    sessionCreate,
    sessionDrop,
    sessionOpenCursor,
    sessionPrepareTransaction,
    sessionReconfigure,
    sessionRename,
    sessionReset,
    sessionRollbackTransaction,
    sessionUpgrade,
    sessionVerify,

    -- * Cursor
    Cursor,
    cursorClose,
    cursorGetKey,
    cursorGetValue,
    cursorInsert,
    cursorKeyFormat,
    cursorNext,
    cursorPrev,
    cursorRemove,
    cursorReset,
    cursorSearch,
    cursorSetKey,
    cursorSetValue,
    cursorUpdate,
    cursorUri,
    cursorValueFormat,
  )
where

import Control.Exception (Exception, throwIO)
import Data.ByteString
import Foreign
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import qualified Language.C.Inline as C
import WiredTiger.Raw.Context
import WiredTiger.Raw.Types
import Data.IORef (newIORef, writeIORef)

C.context (C.baseCtx <> C.bsCtx <> wiredtigerCtx)
C.include "wiredtiger.h"

type Config = String

type Uri = String

withNullableCString :: Maybe String -> (CString -> IO a) -> IO a
withNullableCString = maybe ($ nullPtr) withCString

data WiredTigerException
  = WiredTigerExceptionRollback
  | WiredTigerExceptionDuplicateKey
  | WiredTigerExceptionError
  | WiredTigerExceptionNotFound
  | WiredTigerExceptionPanic
  | WiredTigerExceptionRunRecovery
  | WiredTigerExceptionCacheFull
  | WiredTigerExceptionPrepareConflict
  | WiredTigerExceptionTrySalvage
  | WiredTigerExceptionUnknown Int
  deriving (Eq, Show)

toWiredTigerException :: CInt -> WiredTigerException
toWiredTigerException (-31800) = WiredTigerExceptionRollback
toWiredTigerException (-31801) = WiredTigerExceptionDuplicateKey
toWiredTigerException (-31802) = WiredTigerExceptionError
toWiredTigerException (-31803) = WiredTigerExceptionNotFound
toWiredTigerException (-31804) = WiredTigerExceptionPanic
toWiredTigerException (-31806) = WiredTigerExceptionRunRecovery
toWiredTigerException (-31807) = WiredTigerExceptionCacheFull
toWiredTigerException (-31808) = WiredTigerExceptionPrepareConflict
toWiredTigerException (-31809) = WiredTigerExceptionTrySalvage
toWiredTigerException errno = WiredTigerExceptionUnknown (fromIntegral errno)

instance Exception WiredTigerException

errorCheck :: CInt -> IO ()
errorCheck res =
  case res of
    e | e < 0 -> throwIO (toWiredTigerException e)
    e | e > 0 -> ioError (errnoToIOError "" (Errno e) Nothing Nothing)
    _ -> pure () -- just 0 really but GHC doesn't understand that we cover all cases

withErrorCheck :: IO CInt -> IO ()
withErrorCheck x = x >>= errorCheck

--
-- Top level
--

open :: String -> Maybe Config -> IO Connection
open s mConfig =
  withCString s $ \cs ->
    withNullableCString mConfig $ \config ->
      alloca $ \_cursorPtr -> do
        withErrorCheck
          [C.exp|
            int {
              wiredtiger_open($(const char* cs), NULL, $(const char* config), $(WT_CONNECTION** _cursorPtr))
            }
          |]
        Connection <$> peek _cursorPtr

connectionClose :: Connection -> Maybe Config -> IO ()
connectionClose (Connection _cursorPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_CONNECTION* _cursorPtr)->close($(WT_CONNECTION* _cursorPtr), $(char* config)) } |]

connectionDebugInfo :: Connection -> Maybe Config -> IO ()
connectionDebugInfo (Connection _cursorPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_CONNECTION* _cursorPtr)->debug_info($(WT_CONNECTION* _cursorPtr), $(char* config)) } |]

connectionReconfigure :: Connection -> Maybe Config -> IO ()
connectionReconfigure (Connection _cursorPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_CONNECTION* _cursorPtr)->reconfigure($(WT_CONNECTION *_cursorPtr), $(char* config)) } |]

connectionGetHome :: Connection -> IO String
connectionGetHome (Connection _cursorPtr) =
  peekCString
    =<< [C.exp| const char* { $(WT_CONNECTION* _cursorPtr)->get_home($(WT_CONNECTION* _cursorPtr)) } |]

connectionIsNew :: Connection -> IO Bool
connectionIsNew (Connection _cursorPtr) =
  toBool
    <$> [C.exp| bool { $(WT_CONNECTION* _cursorPtr)->is_new($(WT_CONNECTION* _cursorPtr)) } |]

connectionOpenSession :: Connection -> Maybe Config -> IO Session
connectionOpenSession (Connection _cursorPtr) mConfig =
  withNullableCString mConfig $ \config ->
    alloca $ \sPtr -> do
      withErrorCheck
        [C.exp|
          int {
            $(WT_CONNECTION* _cursorPtr)->open_session($(WT_CONNECTION* _cursorPtr), NULL, $(char* config), $(WT_SESSION** sPtr))
          }
        |]
      Session <$> peek sPtr

sessionClose :: Session -> Maybe Config -> IO ()
sessionClose (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_SESSION* sPtr)->close($(WT_SESSION* sPtr), $(char* config)) } |]

sessionReconfigure :: Session -> Maybe Config -> IO ()
sessionReconfigure (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_SESSION* sPtr)->reconfigure($(WT_SESSION* sPtr), $(char* config)) } |]

sessionOpenCursor :: Session -> Uri -> Maybe Config -> IO Cursor
sessionOpenCursor (Session sPtr) uri mConfig =
  withCString uri $ \cUri ->
    withNullableCString mConfig $ \config ->
      alloca $ \_cursorPtr -> do
        withErrorCheck
          [C.block|
            int {
              int ret = $(WT_SESSION* sPtr)->open_cursor($(WT_SESSION* sPtr), $(char* cUri), NULL, $(char* config), $(WT_CURSOR** _cursorPtr));
              if (ret == 0) {
                (*$(WT_CURSOR **_cursorPtr))->flags |= WT_CURSTD_RAW;
              }
              return ret;
            }
          |]
        Cursor <$> peek _cursorPtr <*> newIORef Nothing <*> newIORef Nothing

sessionAlter :: Session -> String -> Maybe Config -> IO ()
sessionAlter (Session sPtr) name mConfig =
  withCString name $ \cName ->
    withNullableCString mConfig $ \config ->
      withErrorCheck
        [C.exp| int { $(WT_SESSION* sPtr)->alter($(WT_SESSION* sPtr), $(char* cName), $(char* config)) } |]

sessionCreate :: Session -> String -> Maybe Config -> IO ()
sessionCreate (Session sPtr) name mConfig =
  withCString name $ \cName ->
    withNullableCString mConfig $ \config ->
      withErrorCheck
        [C.exp| int { $(WT_SESSION* sPtr)->create($(WT_SESSION* sPtr), $(char* cName), $(char* config)) } |]

sessionDrop :: Session -> String -> Maybe Config -> IO ()
sessionDrop (Session sPtr) name mConfig =
  withCString name $ \cName ->
    withNullableCString mConfig $ \config ->
      withErrorCheck
        [C.exp| int { $(WT_SESSION* sPtr)->drop($(WT_SESSION* sPtr), $(char* cName), $(char* config)) } |]

sessionRename :: Session -> String -> String -> Maybe Config -> IO ()
sessionRename (Session sPtr) uri newUri mConfig =
  withCString uri $ \cUri ->
    withCString newUri $ \cNewUri ->
      withNullableCString mConfig $ \config ->
        withErrorCheck
          [C.exp| int { $(WT_SESSION* sPtr)->rename($(WT_SESSION* sPtr), $(char* cUri), $(char* cNewUri), $(char* config)) } |]

sessionReset :: Session -> IO ()
sessionReset (Session sPtr) =
  withErrorCheck
    [C.exp| int { $(WT_SESSION* sPtr)->reset($(WT_SESSION* sPtr)) } |]

sessionUpgrade :: Session -> String -> Maybe Config -> IO ()
sessionUpgrade (Session sPtr) name mConfig =
  withCString name $ \cName ->
    withNullableCString mConfig $ \config ->
      withErrorCheck
        [C.exp| int { $(WT_SESSION* sPtr)->upgrade($(WT_SESSION* sPtr), $(char* cName), $(char* config)) } |]

sessionVerify :: Session -> String -> Maybe Config -> IO ()
sessionVerify (Session sPtr) name mConfig =
  withCString name $ \cName ->
    withNullableCString mConfig $ \config ->
      withErrorCheck
        [C.exp| int { $(WT_SESSION* sPtr)->verify($(WT_SESSION* sPtr), $(char* cName), $(char* config)) } |]

sessionBeginTransaction :: Session -> Maybe Config -> IO ()
sessionBeginTransaction (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_SESSION* sPtr)->begin_transaction($(WT_SESSION* sPtr), $(char* config)) } |]

sessionCommitTransaction :: Session -> Maybe Config -> IO ()
sessionCommitTransaction (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_SESSION* sPtr)->commit_transaction($(WT_SESSION* sPtr), $(char* config)) } |]

sessionPrepareTransaction :: Session -> Maybe Config -> IO ()
sessionPrepareTransaction (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_SESSION* sPtr)->prepare_transaction($(WT_SESSION* sPtr), $(char* config)) } |]

sessionRollbackTransaction :: Session -> Maybe Config -> IO ()
sessionRollbackTransaction (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_SESSION* sPtr)->rollback_transaction($(WT_SESSION* sPtr), $(char* config)) } |]

cursorUri :: Cursor -> IO String
cursorUri Cursor{_cursorPtr} =
  [C.exp| const char* { $(WT_CURSOR* _cursorPtr)->uri } |] >>= peekCString

cursorKeyFormat :: Cursor -> IO String
cursorKeyFormat Cursor{_cursorPtr} =
  [C.exp| const char* { $(WT_CURSOR* _cursorPtr)->key_format } |] >>= peekCString

cursorValueFormat :: Cursor -> IO String
cursorValueFormat Cursor{_cursorPtr} =
  [C.exp| const char* { $(WT_CURSOR* _cursorPtr)->value_format } |] >>= peekCString

cursorGetKey :: Cursor -> IO ByteString
cursorGetKey Cursor{_cursorPtr} =
  alloca $ \keyPtrPtr ->
    alloca $ \keyLenPtr -> do
      withErrorCheck
        [C.block|
           int {
             WT_ITEM item;
             int ret = $(WT_CURSOR* _cursorPtr)->get_key($(WT_CURSOR* _cursorPtr), &item);
             if (ret == 0) {
               *$(const char** keyPtrPtr) = item.data;
               *$(int* keyLenPtr) = item.size;
             }
             return ret;
           }
         |]
      keyPtr <- peek keyPtrPtr
      keyLen <- peek keyLenPtr
      packCStringLen (keyPtr, fromIntegral keyLen)

cursorGetValue :: Cursor -> IO ByteString
cursorGetValue Cursor{_cursorPtr} =
  alloca $ \keyPtrPtr ->
    alloca $ \keyLenPtr -> do
      withErrorCheck
        [C.block|
           int {
             WT_ITEM item;
             int ret = $(WT_CURSOR* _cursorPtr)->get_value($(WT_CURSOR* _cursorPtr), &item);
             if (ret == 0) {
               *$(const char** keyPtrPtr) = item.data;
               *$(int* keyLenPtr) = item.size;
             }
             return ret;
           }
         |]
      keyPtr <- peek keyPtrPtr
      keyLen <- peek keyLenPtr
      packCStringLen (keyPtr, fromIntegral keyLen)

cursorSetKey :: Cursor -> ByteString -> IO ()
cursorSetKey Cursor{_cursorPtr, _cursorKey} key = do
  writeIORef _cursorKey (Just key)
  [C.block|
    void {
      WT_ITEM item;
      item.data = $bs-ptr:key;
      item.size = $bs-len:key;
      $(WT_CURSOR* _cursorPtr)->set_key($(WT_CURSOR* _cursorPtr), &item);
    }
  |]

cursorSetValue :: Cursor -> ByteString -> IO ()
cursorSetValue Cursor{_cursorPtr, _cursorValue} value = do
  writeIORef _cursorValue (Just value)
  [C.block|
    void {
      WT_ITEM item;
      item.data = $bs-ptr:value;
      item.size = $bs-len:value;
      $(WT_CURSOR* _cursorPtr)->set_value($(WT_CURSOR* _cursorPtr), &item);
    }
  |]

cursorNext :: Cursor -> IO ()
cursorNext Cursor{_cursorPtr} =
  withErrorCheck
    [C.exp| int { $(WT_CURSOR* _cursorPtr)->next($(WT_CURSOR* _cursorPtr)) } |]

cursorPrev :: Cursor -> IO ()
cursorPrev Cursor{_cursorPtr} =
  withErrorCheck
    [C.exp| int { $(WT_CURSOR* _cursorPtr)->prev($(WT_CURSOR* _cursorPtr)) } |]

cursorReset :: Cursor -> IO ()
cursorReset Cursor{_cursorPtr} =
  withErrorCheck
    [C.exp| int { $(WT_CURSOR* _cursorPtr)->reset($(WT_CURSOR* _cursorPtr)) } |]

cursorSearch :: Cursor -> IO ()
cursorSearch Cursor{_cursorPtr} =
  withErrorCheck
    [C.exp| int { $(WT_CURSOR* _cursorPtr)->search($(WT_CURSOR* _cursorPtr)) } |]

cursorInsert :: Cursor -> IO ()
cursorInsert Cursor{_cursorPtr} =
  withErrorCheck
    [C.exp| int { $(WT_CURSOR* _cursorPtr)->insert($(WT_CURSOR* _cursorPtr)) } |]

cursorUpdate :: Cursor -> IO ()
cursorUpdate Cursor{_cursorPtr} =
  withErrorCheck
    [C.exp| int { $(WT_CURSOR* _cursorPtr)->update($(WT_CURSOR* _cursorPtr)) } |]

cursorRemove :: Cursor -> IO ()
cursorRemove Cursor{_cursorPtr} =
  withErrorCheck
    [C.exp| int { $(WT_CURSOR* _cursorPtr)->remove($(WT_CURSOR* _cursorPtr)) } |]

cursorClose :: Cursor -> IO ()
cursorClose Cursor{_cursorPtr} =
  withErrorCheck
    [C.exp| int { $(WT_CURSOR* _cursorPtr)->close($(WT_CURSOR* _cursorPtr)) } |]
