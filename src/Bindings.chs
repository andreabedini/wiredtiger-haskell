{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

#include "bindings.c"

module Bindings where

import Control.Monad
import Control.Exception
import Foreign
import Foreign.C.String
import Foreign.C.Types


{# pointer *WT_COLLATOR as Collator #}
{# pointer *WT_COMPRESSOR as Compressor #}
{# pointer *WT_CONNECTION as Connection #}
{# pointer *WT_CURSOR as Cursor #}
{# pointer *WT_DATA_SOURCE as DataSource #}
{# pointer *WT_ENCRYPTOR as Encryptor #}
{# pointer *WT_EVENT_HANDLER as EventHandler #}
{# pointer *WT_EXTENSION_API as ExtensionAPI #}
{# pointer *WT_EXTRACTOR as Extractor #}
{# pointer *WT_FILE_HANDLE as FileHandle #}
{# pointer *WT_FILE_SYSTEM as FileSystem #}
{# pointer *WT_SESSION as Session #}

--
-- Cursor
--

cursorUri :: Cursor -> IO String
cursorUri = {# get __wt_cursor->uri #} >=> peekCString

cursorKeyFormat :: Cursor -> IO String
cursorKeyFormat = {# get __wt_cursor->key_format #} >=> peekCString

cursorValueFormat :: Cursor -> IO String
cursorValueFormat = {# get __wt_cursor->value_format #} >=> peekCString

--get_key1

{# fun unsafe cursor_get_key1 as ^
  { `Cursor'
  , alloca- `String' peekCString2*
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_get_value1 as ^
  { `Cursor'
  , alloca- `String' peekCString2*
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_set_key1 as ^
  { `Cursor'
  , `String'
  } -> `()' #}

{# fun unsafe cursor_set_value1 as ^
  { `Cursor'
  , `String'
  } -> `()' #}

{# fun unsafe cursor_compare as ^
  { `Cursor'
  , `Cursor'
  , alloca- `Int' peekInt*
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_equals as ^
  { `Cursor'
  , `Cursor'
  , alloca- `Int' peekInt*
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_next as ^
  { `Cursor'
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_prev as ^
  { `Cursor'
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_reset as ^
  { `Cursor'
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_search as ^
  { `Cursor'
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_search_near as ^
  { `Cursor'
  , alloca- `Int' peekInt*
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_insert as ^
  { `Cursor'
  } -> `CInt' errorCheck*- #}


-- cursor_modify

{# fun unsafe cursor_update as ^
  { `Cursor'
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_remove as ^
  { `Cursor'
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_reserve as ^
  { `Cursor'
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_close as ^
  { `Cursor'
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_reconfigure as ^
  { `Cursor'
  , `String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_cache as ^
  { `Cursor'
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_reopen as ^
  { `Cursor'
  , `Bool'
  } -> `CInt' errorCheck*- #}


--
-- Session
--

{# fun unsafe session_close as ^
  { `Session'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_reconfigure as ^
  { `Session'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_strerror as ^
  { `Session'
  , `Int'
  } -> `String' #}

{# fun unsafe session_open_cursor as ^
  { `Session'
  , `String'
  , optionally `Maybe Cursor'
  , optionalCString* `Maybe String'
  , alloca- `Cursor' peek*
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_alter as ^
  { `Session'
  , `String'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}


{# fun unsafe session_create as ^
  { `Session'
  , `String'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

#if WIREDTIGER_VERSION_MAJOR>3
  || (WIREDTIGER_VERSION_MAJOR==3 && WIREDTIGER_VERSION_MAJOR>2)
  || (WIREDTIGER_VERSION_MAJOR==3 && WIREDTIGER_VERSION_MAJOR==2 && WIREDTIGER_VERSION_MAJOR>=1)

{# fun unsafe session_import as ^
  { `Session'
  , `String'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

#endif

{# fun unsafe session_drop as ^
  { `Session'
  , `String'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_join as ^
  { `Session'
  , `Cursor'
  , `Cursor'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_rebalance as ^
  { `Session'
  , `String'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_rename as ^
  { `Session'
  , `String'
  , `String'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_reset as ^
  { `Session'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_salvage as ^
  { `Session'
  , `String'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_truncate as ^
  { `Session'
  , `String'
  , `Cursor'
  , `Cursor'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_upgrade as ^
  { `Session'
  , `String'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_verify as ^
  { `Session'
  , `String'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_begin_transaction as ^
  { `Session'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_commit_transaction as ^
  { `Session'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_prepare_transaction as ^
  { `Session'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_rollback_transaction as ^
  { `Session'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_timestamp_transaction as ^
  { `Session'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_query_timestamp as ^
  { `Session'
  , optionalCString* `Maybe String'
  , alloca- `String' peekCString*
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_checkpoint as ^
  { `Session'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_snapshot as ^
  { `Session'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_transaction_pinned_range as ^
  { `Session'
  , `Int'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_transaction_sync as ^
  { `Session'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

--
-- Connection
--

{# fun unsafe connection_async_flush as ^
  { `Connection'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_close as ^
  { `Connection'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_debug_info as ^
  { `Connection'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_reconfigure as ^
  { `Connection'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_get_home as ^
  { `Connection'
  } -> `String' #}

{# fun unsafe connection_configure_method as ^
  { `Connection'
  , `String'
  , `String'
  , `String'
  , `String'
  , `String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_is_new as ^
  { `Connection'
  } -> `Bool' #}

{# fun unsafe connection_open_session as ^
  { `Connection'
  , optionally `Maybe EventHandler'
  , optionalCString* `Maybe String'
  , alloca- `Session' peek*
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_query_timestamp as ^
  { `Connection'
  , `String'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_set_timestamp as ^
  { `Connection'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_rollback_to_stable as ^
  { `Connection'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_load_extension as ^
  { `Connection'
  , `String'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_add_data_source as ^
  { `Connection'
  , `String'
  , `DataSource'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_add_collator as ^
  { `Connection'
  , `String'
  , `Collator'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_add_compressor as ^
  { `Connection'
  , `String'
  , `Compressor'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_add_encryptor as ^
  { `Connection'
  , `String'
  , `Encryptor'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_add_extractor as ^
  { `Connection'
  , `String'
  , `Extractor'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_set_file_system as ^
  { `Connection'
  , `FileSystem'
  , optionalCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

--
--
--

{# fun unsafe wiredtiger_open as open
  { `String'
  , optionally `Maybe EventHandler'
  , optionalCString* `Maybe String'
  , alloca- `Connection' peek*
  } -> `CInt' errorCheck*- #}

--
-- Utilities
--

{# enum define WiredTigerException
  { WT_ROLLBACK as WiredTigerExceptionRollback
  , WT_DUPLICATE_KEY as WiredTigerExceptionDuplicateKey
  , WT_ERROR as WiredTigerExceptionError
  , WT_NOTFOUND as WiredTigerExceptionNotFound
  , WT_PANIC as WiredTigerExceptionPanic
  , WT_RUN_RECOVERY as WiredTigerExceptionRunRecovery
  , WT_CACHE_FULL as WiredTigerExceptionCacheFull
  , WT_PREPARE_CONFLICT as WiredTigerExceptionPrepareConflict
  , WT_TRY_SALVAGE as WiredTigerExceptionTrySalvage
  } deriving (Eq, Ord, Show) #}

instance Exception WiredTigerException 

errorCheck :: CInt -> IO ()
errorCheck res =
  case res of
    0 -> pure ()
    e | e < 0 -> throwIO $ (toEnum (fromIntegral e) :: WiredTigerException)
    e | e > 0 -> error "some standard posix error"

optionalCString :: Maybe String -> (CString -> IO a) -> IO a
optionalCString = maybe ($ nullPtr) withCString

optionally :: Maybe (Ptr a) -> Ptr a
optionally = maybe nullPtr id

peekInt :: Ptr CInt -> IO Int
peekInt p = fromIntegral <$> peek p

peekCString2 :: Ptr CString -> IO String
peekCString2 = peek >=> peekCString
