{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

#include <wiredtiger.h>

module WiredTiger.Bindings where

import Control.Monad
import Control.Exception
import Data.ByteString (ByteString, packCStringLen, useAsCStringLen)
import Foreign
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types

{# context prefix = "wiredtiger" #}

{# pointer *WT_ASYNC_CALLBACK as AsyncCallback #}
{# pointer *WT_ASYNC_OP as AsyncOp #}
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
{# pointer *WT_ITEM  as Item #}
{# pointer *WT_SESSION as Session #}
{# pointer *WT_PACK_STREAM as PackStream #}

{# enum define AsyncOpType
  { WT_AOP_NONE as AsyncOpNone
  , WT_AOP_COMPACT as AsyncCompact
  , WT_AOP_INSERT as AsyncInsert
  , WT_AOP_REMOVE as AsyncRemove
  , WT_AOP_SEARCH as AsyncSearch
  , WT_AOP_UPDATE as AsyncUpdate
  } deriving (Eq, Ord, Show) #}

--
-- Cursor
--

cursorUri :: Cursor -> IO String
cursorUri = {# get __wt_cursor->uri #} >=> peekCString

cursorKeyFormat :: Cursor -> IO String
cursorKeyFormat = {# get __wt_cursor->key_format #} >=> peekCString

cursorValueFormat :: Cursor -> IO String
cursorValueFormat = {# get __wt_cursor->value_format #} >=> peekCString

{# fun unsafe cursor_get_key as ^
  { `Cursor'
  , alloca- `ByteString' peekItem*
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_get_value as ^
  { `Cursor'
  , alloca- `ByteString' peekItem*
  } -> `CInt' errorCheck*- #}

{# fun unsafe cursor_set_key as ^
  { `Cursor'
  , withItem* `ByteString'
  } -> `()' #}

{# fun unsafe cursor_set_value as ^
  { `Cursor'
  , withItem* `ByteString'
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
-- AsyncOp
--

{# fun unsafe async_op_get_id as ^
  { `AsyncOp'
  } -> `Word' fromIntegral #}

{# fun unsafe async_op_get_type as ^
  { `AsyncOp'
  } -> `AsyncOpType' #}

--
-- Session
--

{# fun unsafe session_close as ^
  { `Session'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_reconfigure as ^
  { `Session'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_strerror as ^
  { `Session'
  , `Int'
  } -> `String' #}

{# fun unsafe session_open_cursor as ^
  { `Session'
  , `String'
  , nullablePtr `Maybe Cursor'
  , nullableCString* `Maybe String'
  , alloca- `Cursor' peek*
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_alter as ^
  { `Session'
  , `String'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_create as ^
  { `Session'
  , `String'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

#if WIREDTIGER_VERSION_MAJOR>3
  || (WIREDTIGER_VERSION_MAJOR==3 && WIREDTIGER_VERSION_MAJOR>2)
  || (WIREDTIGER_VERSION_MAJOR==3 && WIREDTIGER_VERSION_MAJOR==2 && WIREDTIGER_VERSION_MAJOR>=1)

{# fun unsafe session_import as ^
  { `Session'
  , `String'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

#endif

{# fun unsafe session_drop as ^
  { `Session'
  , `String'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_join as ^
  { `Session'
  , `Cursor'
  , `Cursor'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_rebalance as ^
  { `Session'
  , `String'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_rename as ^
  { `Session'
  , `String'
  , `String'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_reset as ^
  { `Session'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_salvage as ^
  { `Session'
  , `String'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_truncate as ^
  { `Session'
  , `String'
  , `Cursor'
  , `Cursor'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_upgrade as ^
  { `Session'
  , `String'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_verify as ^
  { `Session'
  , `String'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_begin_transaction as ^
  { `Session'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_commit_transaction as ^
  { `Session'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_prepare_transaction as ^
  { `Session'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_rollback_transaction as ^
  { `Session'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_timestamp_transaction as ^
  { `Session'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_query_timestamp as ^
  { `Session'
  , alloca- `String' peekCString*
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_checkpoint as ^
  { `Session'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_snapshot as ^
  { `Session'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_transaction_pinned_range as ^
  { `Session'
  , `Int'
  } -> `CInt' errorCheck*- #}

{# fun unsafe session_transaction_sync as ^
  { `Session'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

--
-- Connection
--

{# fun unsafe connection_async_flush as ^
  { `Connection'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_async_new_op as ^
  { `Connection'
  , `String'
  , `String'
  , `AsyncCallback'
  , alloca- `AsyncOp' peek*
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_close as ^
  { `Connection'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_debug_info as ^
  { `Connection'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_reconfigure as ^
  { `Connection'
  , nullableCString* `Maybe String'
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
  , nullablePtr `Maybe EventHandler'
  , nullableCString* `Maybe String'
  , alloca- `Session' peek*
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_query_timestamp as ^
  { `Connection'
  , `String'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_set_timestamp as ^
  { `Connection'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_rollback_to_stable as ^
  { `Connection'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_load_extension as ^
  { `Connection'
  , `String'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_add_data_source as ^
  { `Connection'
  , `String'
  , `DataSource'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_add_collator as ^
  { `Connection'
  , `String'
  , `Collator'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_add_compressor as ^
  { `Connection'
  , `String'
  , `Compressor'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_add_encryptor as ^
  { `Connection'
  , `String'
  , `Encryptor'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_add_extractor as ^
  { `Connection'
  , `String'
  , `Extractor'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe connection_set_file_system as ^
  { `Connection'
  , `FileSystem'
  , nullableCString* `Maybe String'
  } -> `CInt' errorCheck*- #}

--
--
--

{# fun unsafe wiredtiger_open as open
  { `String'
  , nullablePtr `Maybe EventHandler'
  , nullableCString* `Maybe String'
  , alloca- `Connection' peek*
  } -> `CInt' errorCheck*- #}

--
-- Streaming interface to packing
--

{# fun unsafe wiredtiger_pack_start as ^
  { `Session'
  , `String'
  , `Ptr ()'
  , `Int'
  , alloca- `PackStream' peek*
  } -> `CInt' errorCheck*- #}

{# fun unsafe wiredtiger_unpack_start as ^
  { `Session'
  , `String'
  , `Ptr ()'
  , `Int'
  , alloca- `PackStream' peek*
  } -> `CInt' errorCheck*- #}

{# fun unsafe wiredtiger_pack_close as ^
  { `PackStream'
  , alloca- `Int' peekInt*
  } -> `CInt' errorCheck*- #}

{# fun unsafe wiredtiger_pack_item as ^
  { `PackStream'
  , withItem* `ByteString'
  } -> `CInt' errorCheck*- #}

{# fun unsafe wiredtiger_pack_int as ^
  { `PackStream'
  , `Int'
  } -> `CInt' errorCheck*- #}

{# fun unsafe wiredtiger_pack_str as ^
  { `PackStream'
  , `String'
  } -> `CInt' errorCheck*- #}

{# fun unsafe wiredtiger_pack_uint as ^
  { `PackStream'
  , fromIntegral `Word'
  } -> `CInt' errorCheck*- #}

{# fun unsafe wiredtiger_unpack_item as ^
  { `PackStream'
  , alloca- `ByteString' peekItem*
  } -> `CInt' errorCheck*- #}

{# fun unsafe wiredtiger_unpack_int as ^
  { `PackStream'
  , alloca- `Int' peekInt*
  } -> `CInt' errorCheck*- #}

{# fun unsafe wiredtiger_unpack_str as ^
  { `PackStream'
  , alloca- `String' peekCString2*
  } -> `CInt' errorCheck*- #}

{# fun unsafe wiredtiger_unpack_uint as ^
  { `PackStream'
  , alloca- `Word' peekWord*
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
    e | e > 0 -> ioError (errnoToIOError "" (Errno e) Nothing Nothing)

nullableCString :: Maybe String -> (CString -> IO a) -> IO a
nullableCString = maybe ($ nullPtr) withCString

nullablePtr :: Maybe (Ptr a) -> Ptr a
nullablePtr = maybe nullPtr id

peekInt :: (Integral a, Storable a) => Ptr a -> IO Int
peekInt p = fromIntegral <$> peek p

peekWord :: (Integral a, Storable a) => Ptr a -> IO Word
peekWord p = fromIntegral <$> peek p

peekCString2 :: Ptr CString -> IO String
peekCString2 = peek >=> peekCString

peekItem :: Item -> IO ByteString
peekItem item = do
  data_ <- {# get WT_ITEM->data #} item
  size_ <- {# get WT_ITEM->size #} item
  packCStringLen (castPtr data_, fromIntegral size_)

withItem :: ByteString -> (Item -> IO a) -> IO a
withItem b k =
  alloca $ \item->
  useAsCStringLen b $ \(ptr, len) -> do
    {# set WT_ITEM->data #} item (castPtr ptr)
    {# set WT_ITEM->size #} item (fromIntegral len)
    k item

#c
/*
 * Cursor
 */

int cursor_get_key(WT_CURSOR *cursor, WT_ITEM *key) {
  return cursor->get_key(cursor, key);
}

int cursor_get_value(WT_CURSOR *cursor, WT_ITEM *value) {
  return cursor->get_value(cursor, value);
}

void cursor_set_key(WT_CURSOR *cursor, WT_ITEM *key) {
  cursor->set_key(cursor, key);
}

void cursor_set_value(WT_CURSOR *cursor, WT_ITEM *value) {
  cursor->set_value(cursor, value);
}

int cursor_compare(WT_CURSOR *cursor, WT_CURSOR *other, int *comparep) {
  return cursor->compare(cursor, other, comparep);
}

int cursor_equals(WT_CURSOR *cursor, WT_CURSOR *other, int *equalp) {
  return cursor->equals(cursor, other, equalp);
}

int cursor_next(WT_CURSOR *cursor) {
  return cursor->next(cursor);
}

int cursor_prev(WT_CURSOR *cursor) {
  return cursor->prev(cursor);
}

int cursor_reset(WT_CURSOR *cursor) {
  return cursor->reset(cursor);
}

int cursor_search(WT_CURSOR *cursor) {
  return cursor->search(cursor);
}

int cursor_search_near(WT_CURSOR *cursor, int *exactp) {
  return cursor->search_near(cursor, exactp);
}

int cursor_insert(WT_CURSOR *cursor) {
  return cursor->insert(cursor);
}

int cursor_modify(WT_CURSOR *cursor, WT_MODIFY *entries, int nentries) {
  return cursor->modify(cursor, entries, nentries);
}

int cursor_update(WT_CURSOR *cursor) {
  return cursor->update(cursor);
}

int cursor_remove(WT_CURSOR *cursor) {
  return cursor->remove(cursor);
}

int cursor_reserve(WT_CURSOR *cursor) {
  return cursor->reserve(cursor);
}

int cursor_close(WT_CURSOR *cursor) {
  return cursor->close(cursor);
}

int cursor_reconfigure(WT_CURSOR *cursor, const char *config) {
  return cursor->reconfigure(cursor, config);
}

int cursor_cache(WT_CURSOR *cursor) {
  return cursor->cache(cursor);
}

int cursor_reopen(WT_CURSOR *cursor, bool check_only) {
  return cursor->reopen(cursor, check_only);
}

/*
 * AsyncOp
 */

uint64_t async_op_get_id(WT_ASYNC_OP *op) {
  return op->get_id(op);
}

WT_ASYNC_OPTYPE async_op_get_type(WT_ASYNC_OP *op) {
  return op->get_type(op);
}

/*
 * Session
 */

int session_close(WT_SESSION *session, const char *config) {
  return session->close(session, config);
}

int session_reconfigure(WT_SESSION *session, const char *config) {
  return session->reconfigure(session, config);
}

const char* session_strerror(WT_SESSION *session, int error) {
  return session->strerror(session, error);
}

int session_open_cursor(WT_SESSION *session, const char *uri,
    WT_CURSOR *to_dup, const char *config, WT_CURSOR **cursorp) {
  int ret = session->open_cursor(session, uri, to_dup, config, cursorp);
  if (ret == 0) {
    (*cursorp)->flags |= WT_CURSTD_RAW;
  }
  return ret;
}

int session_alter(WT_SESSION *session, const char *name, const char *config) {
  return session->alter(session, name, config);
}

int session_create(WT_SESSION *session, const char *name, const char *config) {
  return session->create(session, name, config);
}

#if WIREDTIGER_VERSION_MAJOR>3
  || (WIREDTIGER_VERSION_MAJOR==3 && WIREDTIGER_VERSION_MAJOR>2)
  || (WIREDTIGER_VERSION_MAJOR==3 && WIREDTIGER_VERSION_MAJOR==2 && WIREDTIGER_VERSION_MAJOR>=1)
int session_import(WT_SESSION *session, const char *name, const char *config) {
  return session->import(session, name, config);
}
#endif

int session_drop(WT_SESSION *session, const char *name, const char *config) {
  return session->drop(session, name, config);
}

int session_join(WT_SESSION *session, WT_CURSOR *join_cursor, WT_CURSOR *ref_cursor, const char *config) {
  return session->join(session, join_cursor, ref_cursor, config);
}

/* int session_log_flush(WT_SESSION *session, const char *format, ...) */

int session_rebalance(WT_SESSION *session, const char *uri, const char *config) {
  return session->rebalance(session, uri, config);
}

int session_rename(WT_SESSION *session, const char *uri, const char *newuri, const char *config) {
  return session->rename(session, uri, newuri, config);
}

int session_reset(WT_SESSION *session) {
  return session->reset(session);
}

int session_salvage(WT_SESSION *session, const char *name, const char *config) {
  return session->salvage(session, name, config);
}

int session_truncate(WT_SESSION *session, const char *name, WT_CURSOR* start, WT_CURSOR *stop, const char *config) {
  return session->truncate(session, name, start, stop, config);
}

int session_upgrade(WT_SESSION *session, const char *name, const char *config) {
  return session->upgrade(session, name, config);
}

int session_verify(WT_SESSION *session, const char *name, const char *config) {
  return session->verify(session, name, config);
}

int session_begin_transaction(WT_SESSION *session, const char *config) {
  return session->begin_transaction(session, config);
}

int session_commit_transaction(WT_SESSION *session, const char *config) {
  return session->commit_transaction(session, config);
}

int session_prepare_transaction(WT_SESSION *session, const char *config) {
  return session->prepare_transaction(session, config);
}

int session_rollback_transaction(WT_SESSION *session, const char *config) {
  return session->rollback_transaction(session, config);
}

int session_timestamp_transaction(WT_SESSION *session, const char *config) {
  return session->timestamp_transaction(session, config);
}

int session_query_timestamp(WT_SESSION *session, char *hex_timestamp, const char *config) {
  return session->query_timestamp(session, hex_timestamp, config);
}

int session_checkpoint(WT_SESSION *session, const char *config) {
  return session->checkpoint(session, config);
}

int session_snapshot(WT_SESSION *session, const char *config) {
  return session->snapshot(session, config);
}

int session_transaction_pinned_range(WT_SESSION *session, uint64_t *range) {
  return session->transaction_pinned_range(session, range);
}

int session_transaction_sync(WT_SESSION *session, const char *config) {
  return session->transaction_sync(session, config);
}

/*
 * Connection
 */

int connection_async_flush(WT_CONNECTION *connection) {
  return connection->async_flush(connection);
}

int connection_async_new_op(WT_CONNECTION *connection, const char *uri,
const char *config, WT_ASYNC_CALLBACK *callback, WT_ASYNC_OP **asyncopp) {
  return connection->async_new_op(connection, uri, config, callback, asyncopp);
}

int connection_close(WT_CONNECTION *connection, const char *config) {
  return connection->close(connection, config);
}

int connection_debug_info(WT_CONNECTION *connection, const char *config) {
  return connection->debug_info(connection, config);
}

int connection_reconfigure(WT_CONNECTION *connection, const char *config) {
  return connection->reconfigure(connection, config);
}

const char *connection_get_home(WT_CONNECTION *connection) {
  return connection->get_home(connection);
}

int connection_configure_method(WT_CONNECTION *connection, const char *method, const char *uri, const char *config, const char *type, const char *check) {
  return connection->configure_method(connection, method, uri, config, type, check);
}

int connection_is_new(WT_CONNECTION *connection) {
  return connection->is_new(connection);
}

int connection_open_session(WT_CONNECTION *connection, WT_EVENT_HANDLER *event_handler, const char *config, WT_SESSION **sessionp) {
  return connection->open_session(connection, event_handler, config, sessionp);
}

int connection_query_timestamp(WT_CONNECTION *connection, char *hex_timestamp, const char *config) {
  return connection->query_timestamp(connection, hex_timestamp, config);
}

int connection_set_timestamp(WT_CONNECTION *connection, const char *config) {
  return connection->set_timestamp(connection, config);
}

int connection_rollback_to_stable(WT_CONNECTION *connection, const char *config) {
  return connection->rollback_to_stable(connection, config);
}

int connection_load_extension(WT_CONNECTION *connection, const char *path, const char *config) {
  return connection->load_extension(connection, path, config);
}

int connection_add_data_source(WT_CONNECTION *connection, const char *prefix, WT_DATA_SOURCE *data_source, const char *config) {
  return connection->add_data_source(connection, prefix, data_source, config);
}

int connection_add_collator(WT_CONNECTION *connection, const char *name, WT_COLLATOR *collator, const char *config) {
  return connection->add_collator(connection, name, collator, config);
}

int connection_add_compressor(WT_CONNECTION *connection, const char *name, WT_COMPRESSOR *compressor, const char *config) {
  return connection->add_compressor(connection, name, compressor, config);
}

int connection_add_encryptor(WT_CONNECTION *connection, const char *name, WT_ENCRYPTOR *compressor, const char *config) {
  return connection->add_encryptor(connection, name, compressor, config);
}

int connection_add_extractor(WT_CONNECTION *connection, const char *name, WT_EXTRACTOR *compressor, const char *config) {
  return connection->add_extractor(connection, name, compressor, config);
}

int connection_set_file_system(WT_CONNECTION *connection, WT_FILE_SYSTEM *fs, const char *config) {
  return connection->set_file_system(connection, fs, config);
}
#endc
