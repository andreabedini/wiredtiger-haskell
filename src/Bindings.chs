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

{# fun unsafe cursor_uri as ^
  { `Cursor'
  } -> `String' #}

{# fun unsafe cursor_key_format as ^
  { `Cursor'
  } -> `String' #}

{# fun unsafe cursor_value_format as ^
  { `Cursor'
  } -> `String' #}

--get_key1

{# fun unsafe cursor_get_key1 as ^
  { `Cursor'
  , alloca- `String' peekCString2*
  } -> `Int' errorCheck*- #}

{# fun unsafe cursor_get_value1 as ^
  { `Cursor'
  , alloca- `String' peekCString2*
  } -> `Int' errorCheck*- #}

peekCString2 :: Ptr CString -> IO String
peekCString2 = peek >=> peekCString

--get_value1

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
  } -> `Int' errorCheck*- #}


{# fun unsafe cursor_equals as ^
  { `Cursor'
  , `Cursor'
  , alloca- `Int' peekInt*
  } -> `Int' errorCheck*- #}


{# fun unsafe cursor_next as ^
  { `Cursor'
  } -> `Int' errorCheck*- #}


{# fun unsafe cursor_prev as ^
  { `Cursor'
  } -> `Int' errorCheck*- #}


{# fun unsafe cursor_reset as ^
  { `Cursor'
  } -> `Int' errorCheck*- #}


{# fun unsafe cursor_search as ^
  { `Cursor'
  } -> `Int' errorCheck*- #}


{# fun unsafe cursor_search_near as ^
  { `Cursor'
  , alloca- `Int' peekInt*
  } -> `Int' errorCheck*- #}


{# fun unsafe cursor_insert as ^
  { `Cursor'
  } -> `Int' errorCheck*- #}


-- cursor_modify

{# fun unsafe cursor_update as ^
  { `Cursor'
  } -> `Int' errorCheck*- #}


{# fun unsafe cursor_remove as ^
  { `Cursor'
  } -> `Int' errorCheck*- #}


{# fun unsafe cursor_reserve as ^
  { `Cursor'
  } -> `Int' errorCheck*- #}


{# fun unsafe cursor_close as ^
  { `Cursor'
  } -> `Int' errorCheck*- #}


{# fun unsafe cursor_reconfigure as ^
  { `Cursor'
  , `String'
  } -> `Int' errorCheck*- #}


{# fun unsafe cursor_cache as ^
  { `Cursor'
  } -> `Int' errorCheck*- #}


{# fun unsafe cursor_reopen as ^
  { `Cursor'
  , `Bool'
  } -> `Int' errorCheck*- #}


--
-- Session
--

{# fun unsafe session_close as ^
  { `Session'
  , `String'
  } -> `Int' errorCheck*- #}


{# fun unsafe session_reconfigure as ^
  { `Session'
  , `String'
  } -> `Int' errorCheck*- #}


{# fun unsafe session_strerror as ^
  { `Session'
  , `Int'
  } -> `String' #}

{# fun unsafe session_open_cursor as ^
  { `Session'
  , `String'
  , optionally `Maybe Cursor'
  , `String'
  , alloca- `Cursor' peek*
  } -> `Int' errorCheck*- #}


{# fun unsafe session_alter as ^
  { `Session'
  , `String'
  , `String'
  } -> `Int' errorCheck*- #}


{# fun unsafe session_create as ^
  { `Session'
  , `String'
  , `String'
  } -> `Int' errorCheck*- #}


#if WIREDTIGER_VERSION_MAJOR>3
  || (WIREDTIGER_VERSION_MAJOR==3 && WIREDTIGER_VERSION_MAJOR>2)
  || (WIREDTIGER_VERSION_MAJOR==3 && WIREDTIGER_VERSION_MAJOR==2 && WIREDTIGER_VERSION_MAJOR>=1)

{# fun unsafe session_import as ^
  { `Session'
  , `String'
  , `String'
  } -> `Int' errorCheck*- #}

#endif

{# fun unsafe session_drop as ^
  { `Session'
  , `String'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_join as ^
  { `Session'
  , `Cursor'
  , `Cursor'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_rebalance as ^
  { `Session'
  , `String'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_rename as ^
  { `Session'
  , `String'
  , `String'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_reset as ^
  { `Session'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_salvage as ^
  { `Session'
  , `String'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_truncate as ^
  { `Session'
  , `String'
  , `Cursor'
  , `Cursor'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_upgrade as ^
  { `Session'
  , `String'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_verify as ^
  { `Session'
  , `String'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_begin_transaction as ^
  { `Session'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_commit_transaction as ^
  { `Session'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_prepare_transaction as ^
  { `Session'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_rollback_transaction as ^
  { `Session'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_timestamp_transaction as ^
  { `Session'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_query_timestamp as ^
  { `Session'
  , `String'
  , alloca- `String' peekCString*
  } -> `Int' errorCheck*- #}

{# fun unsafe session_checkpoint as ^
  { `Session'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_snapshot as ^
  { `Session'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_transaction_pinned_range as ^
  { `Session'
  , `Int'
  } -> `Int' errorCheck*- #}

{# fun unsafe session_transaction_sync as ^
  { `Session'
  , `String'
  } -> `Int' errorCheck*- #}

--
-- Connection
--

{# fun unsafe connection_async_flush as ^
  { `Connection'
  } -> `Int' errorCheck*- #}

{# fun unsafe connection_close as ^
  { `Connection'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe connection_debug_info as ^
  { `Connection'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe connection_reconfigure as ^
  { `Connection'
  , `String'
  } -> `Int' errorCheck*- #}

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
  } -> `Int' errorCheck*- #}

{# fun unsafe connection_is_new as ^
  { `Connection'
  } -> `Bool' #}

{# fun unsafe connection_open_session as ^
  { `Connection'
  , optionally `Maybe EventHandler'
  , `String'
  , alloca- `Session' peek*
  } -> `Int' errorCheck*- #}

{# fun unsafe connection_query_timestamp as ^
  { `Connection'
  , `String'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe connection_set_timestamp as ^
  { `Connection'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe connection_rollback_to_stable as ^
  { `Connection'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe connection_load_extension as ^
  { `Connection'
  , `String'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe connection_add_data_source as ^
  { `Connection'
  , `String'
  , `DataSource'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe connection_add_collator as ^
  { `Connection'
  , `String'
  , `Collator'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe connection_add_compressor as ^
  { `Connection'
  , `String'
  , `Compressor'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe connection_add_encryptor as ^
  { `Connection'
  , `String'
  , `Encryptor'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe connection_add_extractor as ^
  { `Connection'
  , `String'
  , `Extractor'
  , `String'
  } -> `Int' errorCheck*- #}

{# fun unsafe connection_set_file_system as ^
  { `Connection'
  , `FileSystem'
  , `String'
  } -> `Int' errorCheck*- #}

--
--
--

{# fun unsafe wiredtiger_open as open
  { `String'
  , optionally `Maybe EventHandler'
  , `String'
  , alloca- `Connection' peek*
  } -> `Int' errorCheck*- #}

--
-- Utilities
--

newtype WiredTigerException = WiredTigerException String 
  deriving Show

instance Exception WiredTigerException 

errorCheck res =
  case res of
    0 -> pure ()
    _ -> throwIO $ WiredTigerException "something didn't work"

optionally = maybe nullPtr id

peekInt p = fromIntegral <$> peek p

