{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WiredTiger.Bindings where

import Control.Exception (throwIO, Exception)
import Data.ByteString
import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import qualified Language.C.Inline as C

C.context (C.baseCtx <> C.bsCtx)
C.include "wiredtiger.h"

type Config = String
type Uri = String

newtype Connection = Connection (Ptr ())
newtype Cursor = Cursor (Ptr ())
newtype Session = Session (Ptr ())

data EventHandler

withNullableCString :: Maybe String -> (CString -> IO a) -> IO a
withNullableCString = maybe ($ nullPtr) withCString

nullablePtr :: Maybe (Ptr a) -> Ptr a
nullablePtr = fromMaybe nullPtr

newtype WiredTigerException
  = WiredTigerException Int
  deriving (Eq, Show)

instance Exception WiredTigerException

errorCheck :: CInt -> IO ()
errorCheck res =
  case res of
    e | e < 0 -> throwIO (WiredTigerException (fromIntegral e))
    e | e > 0 -> ioError (errnoToIOError "" (Errno e) Nothing Nothing)
    _ -> pure () -- just 0 really but GHC doesn't understand that we cover all cases

allocaWithErrorCheck :: Storable a => (Ptr a -> b) -> (Ptr a -> IO CInt) -> IO b
allocaWithErrorCheck c x = alloca $ \ptr -> do { x ptr >>= errorCheck; pure (c ptr) }

allocaWithErrorCheckM :: Storable a => (Ptr a -> IO b) -> (Ptr a -> IO CInt) -> IO b
allocaWithErrorCheckM c x = alloca $ \ptr -> do { x ptr >>= errorCheck; c ptr }

withErrorCheck :: IO CInt -> IO ()
withErrorCheck x = x >>= errorCheck

--
-- Top level
--

open :: String -> Maybe EventHandler -> Maybe Config -> IO Connection
open s _me mConfig =
  withCString s $ \cs ->
  withNullableCString mConfig $ \config ->
  allocaWithErrorCheck Connection $ \connectionPtr ->
  [C.exp|
    int { wiredtiger_open($(const char* cs), NULL, $(const char* config), $(void *connectionPtr)) }
  |]

--
-- Connection
--

connectionClose ::Connection -> Maybe Config -> IO ()
connectionClose (Connection cPtr) mConfig =
  withNullableCString mConfig $ \config ->
  withErrorCheck
  [C.exp|
    int {
      ((WT_CONNECTION*)$(void* cPtr))->close($(void* cPtr), $(char* config))
    }
  |]

connectionDebugInfo ::Connection -> Maybe Config -> IO ()
connectionDebugInfo (Connection cPtr) mConfig =
  withNullableCString mConfig $ \config ->
  withErrorCheck
  [C.exp|
    int {
      ((WT_CONNECTION*)$(void* cPtr))->debug_info($(void* cPtr), $(char* config))
    }
  |]

connectionReconfigure ::Connection -> Maybe Config -> IO ()
connectionReconfigure (Connection cPtr) mConfig =
  withNullableCString mConfig $ \config ->
  withErrorCheck
  [C.exp|
    int {
      ((WT_CONNECTION*)$(void* cPtr))->reconfigure($(void *cPtr), $(char* config))
    }
  |]

connectionGetHome ::Connection -> IO String
connectionGetHome (Connection cPtr) = peekCString =<<
  [C.exp|
    const char* {
      ((WT_CONNECTION*)$(void* cPtr))->get_home($(void* cPtr))
    }
  |]

connectionIsNew :: Connection -> IO Bool
connectionIsNew (Connection cPtr) = toBool <$>
  [C.exp|
    bool {
      ((WT_CONNECTION*)$(void* cPtr))->is_new($(void* cPtr))
    }
  |]

connectionOpenSession :: Connection -> Maybe EventHandler -> Maybe Config -> IO Session
connectionOpenSession (Connection cPtr) _me mConfig =
  withNullableCString mConfig $ \config ->
  allocaWithErrorCheck Session $ \sPtr ->
  [C.exp|
    int {
      ((WT_CONNECTION*)$(void* cPtr))->open_session($(void* cPtr), NULL, $(char* config), $(void* sPtr))
    }
  |]

--
-- Session
--

sessionClose :: Session -> Maybe Config -> IO ()
sessionClose (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
  withErrorCheck
  [C.exp|
    int {
      ((WT_SESSION*)$(void* sPtr))->close($(void* sPtr), $(char* config))
    }
  |]

sessionReconfigure :: Session -> Maybe Config -> IO ()
sessionReconfigure (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
  withErrorCheck
  [C.exp|
    int {
      ((WT_SESSION*)$(void* sPtr))->reconfigure($(void* sPtr), $(char* config))
    }
  |]

openCursor :: Session -> Uri -> Maybe Config -> IO Cursor
openCursor (Session sPtr) uri mConfig =
  withCString uri $ \cUri ->
  withNullableCString mConfig $ \config ->
  allocaWithErrorCheck Cursor $ \cPtr ->
  [C.exp|
    int {
      ((WT_SESSION*)$(void* sPtr))->open_cursor($(void* sPtr), $(char* cUri), NULL, $(char* config), $(void* cPtr))
    }
  |]

sessionAlter :: Session -> String -> Maybe Config -> IO ()
sessionAlter (Session sPtr) name mConfig =
  withCString name $ \cName ->
  withNullableCString mConfig $ \config ->
  withErrorCheck
  [C.exp|
    int {
      ((WT_SESSION*)$(void* sPtr))->alter($(void* sPtr), $(char* cName), $(char* config))
    }
  |]

sessionCreate :: Session -> String -> Maybe Config -> IO ()
sessionCreate (Session sPtr) name mConfig =
  withCString name $ \cName ->
  withNullableCString mConfig $ \config ->
  withErrorCheck
  [C.exp|
    int {
      ((WT_SESSION*)$(void* sPtr))->create($(void* sPtr), $(char* cName), $(char* config))
    }
  |]

sessionDrop :: Session -> String -> Maybe Config -> IO ()
sessionDrop (Session sPtr) name mConfig =
  withCString name $ \cName ->
  withNullableCString mConfig $ \config ->
  withErrorCheck
  [C.exp|
    int {
      ((WT_SESSION*)$(void* sPtr))->drop($(void* sPtr), $(char* cName), $(char* config))
    }
  |]

sessionRename :: Session -> String -> String -> Maybe Config -> IO ()
sessionRename (Session sPtr) uri newUri mConfig =
  withCString uri $ \cUri->
  withCString newUri$ \cNewUri->
  withNullableCString mConfig $ \config ->
  withErrorCheck
  [C.exp|
    int {
      ((WT_SESSION*)$(void* sPtr))->rename($(void* sPtr), $(char* cUri), $(char* cNewUri), $(char* config))
    }
  |]

sessionReset :: Session -> IO ()
sessionReset (Session sPtr) =
  withErrorCheck
  [C.exp|
    int {
      ((WT_SESSION*)$(void* sPtr))->reset($(void* sPtr))
    }
  |]

sessionUpgrade :: Session -> String -> Maybe Config -> IO ()
sessionUpgrade (Session sPtr) name mConfig =
  withCString name $ \cName ->
  withNullableCString mConfig $ \config ->
  withErrorCheck
  [C.exp|
    int {
      ((WT_SESSION*)$(void* sPtr))->upgrade($(void* sPtr), $(char* cName), $(char* config))
    }
  |]

sessionVerify :: Session -> String -> Maybe Config -> IO ()
sessionVerify (Session sPtr) name mConfig =
  withCString name $ \cName ->
  withNullableCString mConfig $ \config ->
  withErrorCheck
  [C.exp|
    int {
      ((WT_SESSION*)$(void* sPtr))->verify($(void* sPtr), $(char* cName), $(char* config))
    }
  |]

sessionBeginTransaction :: Session -> Maybe Config -> IO ()
sessionBeginTransaction (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
  withErrorCheck
  [C.exp|
    int {
      ((WT_SESSION*)$(void* sPtr))->begin_transaction($(void* sPtr), $(char* config))
    }
  |]

sessionCommitTransaction :: Session -> Maybe Config -> IO ()
sessionCommitTransaction (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
  withErrorCheck
  [C.exp|
    int {
      ((WT_SESSION*)$(void* sPtr))->commit_transaction($(void* sPtr), $(char* config))
    }
  |]

sessionPrepareTransaction :: Session -> Maybe Config -> IO ()
sessionPrepareTransaction (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
  withErrorCheck
  [C.exp|
    int {
      ((WT_SESSION*)$(void* sPtr))->prepare_transaction($(void* sPtr), $(char* config))
    }
  |]

sessionRollbackTransaction :: Session -> Maybe Config -> IO ()
sessionRollbackTransaction (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
  withErrorCheck
  [C.exp|
    int {
      ((WT_SESSION*)$(void* sPtr))->rollback_transaction($(void* sPtr), $(char* config))
    }
  |]

--
-- Cursor
--

cursorUri :: Cursor -> IO String
cursorUri (Cursor cPtr) = peekCString =<<
  [C.exp|
    const char* {
      ((WT_CURSOR*)$(void* cPtr))->uri
    }
  |]

cursorKeyFormat :: Cursor -> IO String
cursorKeyFormat (Cursor cPtr) = peekCString =<<
  [C.exp|
    const char* {
      ((WT_CURSOR*)$(void* cPtr))->key_format
    }
  |]

cursorValueFormat :: Cursor -> IO String
cursorValueFormat (Cursor cPtr) = peekCString =<<
  [C.exp|
    const char* {
      ((WT_CURSOR*)$(void* cPtr))->value_format
    }
  |]

cursorGetKey :: Cursor -> IO ByteString
cursorGetKey (Cursor cPtr) =
  alloca $ \keyPtrPtr ->
  alloca $ \keyLenPtr -> do
    r <- [C.block|
           int {
             WT_ITEM item;
             int ret = ((WT_CURSOR*)$(void* cPtr))->get_key($(void* cPtr), &item);
             if (ret == 0) {
               *$(char** keyPtrPtr) = item.data;
               *$(int* keyLenPtr) = item.size;
             }
             return ret;
           }
         |]
    errorCheck r
    keyPtr <- peek keyPtrPtr
    keyLen <- peek keyLenPtr
    packCStringLen (keyPtr, fromIntegral keyLen)

cursorGetValue :: Cursor -> IO ByteString
cursorGetValue (Cursor cPtr) =
  alloca $ \keyPtrPtr ->
  alloca $ \keyLenPtr -> do
    r <- [C.block|
           int {
             WT_ITEM item;
             int ret = ((WT_CURSOR*)$(void* cPtr))->get_value($(void* cPtr), &item);
             if (ret == 0) {
               *$(char** keyPtrPtr) = item.data;
               *$(int* keyLenPtr) = item.size;
             }
             return ret;
           }
         |]
    errorCheck r
    keyPtr <- peek keyPtrPtr
    keyLen <- peek keyLenPtr
    packCStringLen (keyPtr, fromIntegral keyLen)

cursorSetKey :: Cursor -> ByteString -> IO ()
cursorSetKey (Cursor cPtr) key =
  [C.block|
    void {
      WT_ITEM item;
      item.data = $bs-ptr:key;
      item.size = $bs-len:key;
      ((WT_CURSOR*)$(void* cPtr))->set_key($(void* cPtr), &item);
    }
  |]

cursorSetValue :: Cursor -> ByteString -> IO ()
cursorSetValue (Cursor cPtr) key =
  [C.block|
    void {
      WT_ITEM item;
      item.data = $bs-ptr:key;
      item.size = $bs-len:key;
      ((WT_CURSOR*)$(void* cPtr))->set_value($(void* cPtr), &item);
    }
  |]

cursorNext :: Cursor -> IO ()
cursorNext (Cursor cPtr) =
  withErrorCheck
  [C.exp| int { ((WT_CURSOR*)$(void* cPtr))->next($(void* cPtr)) } |]

cursorPrev :: Cursor -> IO ()
cursorPrev (Cursor cPtr) =
  withErrorCheck
  [C.exp| int { ((WT_CURSOR*)$(void* cPtr))->prev($(void* cPtr)) } |]

cursorReset :: Cursor -> IO ()
cursorReset (Cursor cPtr) =
  withErrorCheck
  [C.exp| int { ((WT_CURSOR*)$(void* cPtr))->reset($(void* cPtr)) } |]

cursorSearch :: Cursor -> IO ()
cursorSearch (Cursor cPtr) =
  withErrorCheck
  [C.exp| int { ((WT_CURSOR*)$(void* cPtr))->search($(void* cPtr)) } |]

cursorInsert :: Cursor -> IO ()
cursorInsert (Cursor cPtr) =
  withErrorCheck
  [C.exp| int { ((WT_CURSOR*)$(void* cPtr))->insert($(void* cPtr)) } |]

cursorUpdate :: Cursor -> IO ()
cursorUpdate (Cursor cPtr) =
  withErrorCheck
  [C.exp| int { ((WT_CURSOR*)$(void* cPtr))->update($(void* cPtr)) } |]

cursorRemove :: Cursor -> IO ()
cursorRemove (Cursor cPtr) =
  withErrorCheck
  [C.exp| int { ((WT_CURSOR*)$(void* cPtr))->remove($(void* cPtr)) } |]

cursorClose :: Cursor -> IO ()
cursorClose (Cursor cPtr) =
  withErrorCheck
  [C.exp| int { ((WT_CURSOR*)$(void* cPtr))->close($(void* cPtr)) } |]

--
-- Streaming interface to packing
--

-- {# fun unsafe wiredtiger_pack_start as ^
--   { `Session'
--   , `String'
--   , `Ptr ()'
--   , `Int'
--   , alloca- `PackStream' peek*
--   } -> `CInt' errorCheck*- #}

-- {# fun unsafe wiredtiger_unpack_start as ^
--   { `Session'
--   , `String'
--   , `Ptr ()'
--   , `Int'
--   , alloca- `PackStream' peek*
--   } -> `CInt' errorCheck*- #}

-- {# fun unsafe wiredtiger_pack_close as ^
--   { `PackStream'
--   , alloca- `Int' peekInt*
--   } -> `CInt' errorCheck*- #}

-- {# fun unsafe wiredtiger_pack_item as ^
--   { `PackStream'
--   , withItem* `ByteString'
--   } -> `CInt' errorCheck*- #}

-- {# fun unsafe wiredtiger_pack_int as ^
--   { `PackStream'
--   , `Int'
--   } -> `CInt' errorCheck*- #}

-- {# fun unsafe wiredtiger_pack_str as ^
--   { `PackStream'
--   , `String'
--   } -> `CInt' errorCheck*- #}

-- {# fun unsafe wiredtiger_pack_uint as ^
--   { `PackStream'
--   , fromIntegral `Word'
--   } -> `CInt' errorCheck*- #}

-- {# fun unsafe wiredtiger_unpack_item as ^
--   { `PackStream'
--   , alloca- `ByteString' peekItem*
--   } -> `CInt' errorCheck*- #}

-- {# fun unsafe wiredtiger_unpack_int as ^
--   { `PackStream'
--   , alloca- `Int' peekInt*
--   } -> `CInt' errorCheck*- #}

-- {# fun unsafe wiredtiger_unpack_str as ^
--   { `PackStream'
--   , alloca- `String' peekCString2*
--   } -> `CInt' errorCheck*- #}

-- {# fun unsafe wiredtiger_unpack_uint as ^
--   { `PackStream'
--   , alloca- `Word' peekWord*
--   } -> `CInt' errorCheck*- #}

-- --
-- -- Utilities
-- --

-- {# enum define WiredTigerException
--   { WT_ROLLBACK as WiredTigerExceptionRollback
--   , WT_DUPLICATE_KEY as WiredTigerExceptionDuplicateKey
--   , WT_ERROR as WiredTigerExceptionError
--   , WT_NOTFOUND as WiredTigerExceptionNotFound
--   , WT_PANIC as WiredTigerExceptionPanic
--   , WT_RUN_RECOVERY as WiredTigerExceptionRunRecovery
--   , WT_CACHE_FULL as WiredTigerExceptionCacheFull
--   , WT_PREPARE_CONFLICT as WiredTigerExceptionPrepareConflict
--   , WT_TRY_SALVAGE as WiredTigerExceptionTrySalvage
--   } deriving (Eq, Ord, Show) #}

-- instance Exception WiredTigerException

-- peekInt :: (Integral a, Storable a) => Ptr a -> IO Int
-- peekInt p = fromIntegral <$> peek p

-- peekWord :: (Integral a, Storable a) => Ptr a -> IO Word
-- peekWord p = fromIntegral <$> peek p

-- peekCString2 :: Ptr CString -> IO String
-- peekCString2 = peek >=> peekCString

-- peekItem :: Item -> IO ByteString
-- peekItem item = do
--   data_ <- {# get WT_ITEM->data #} item
--   size_ <- {# get WT_ITEM->size #} item
--   packCStringLen (castPtr data_, fromIntegral size_)

-- withItem :: ByteString -> (Item -> IO a) -> IO a
-- withItem b k =
--   alloca $ \item->
--   useAsCStringLen b $ \(ptr, len) -> do
--     {# set WT_ITEM->data #} item (castPtr ptr)
--     {# set WT_ITEM->size #} item (fromIntegral len)
--     k item
