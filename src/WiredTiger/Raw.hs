{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module WiredTiger.Raw
  ( open,
    WiredTigerException (..),

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
import Data.IORef (newIORef, writeIORef, readIORef)
import Foreign
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import qualified Language.C.Inline as C
import WiredTiger.Raw.Context
import WiredTiger.Raw.Types

C.context (C.baseCtx <> C.bsCtx <> wiredtigerCtx)
C.include "wiredtiger.h"

type Config = String

type Uri = String

withNullableCString
  :: Maybe String
  -> (CString
  -> IO a)
  -> IO a
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

toWiredTigerException
  :: CInt
  -> WiredTigerException
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

errorCheck
  :: CInt
  -> IO ()
errorCheck res =
  case res of
    e | e < 0 -> throwIO (toWiredTigerException e)
    e | e > 0 -> ioError (errnoToIOError "" (Errno e) Nothing Nothing)
    _ -> pure () -- just 0 really but GHC doesn't understand that we cover all cases

withErrorCheck
  :: IO CInt
  -> IO ()
withErrorCheck x = x >>= errorCheck

-- | Open a connection to a database
open
  :: String
  -- ^ The path to the database home directory
  -> Maybe Config
  -- ^ The configuration string
  -> IO Connection
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

-- | Close a connection.
--
-- Any open sessions will be closed. This will release the resources
-- associated with the session handle, including rolling back any active
-- transactions and closing any cursors that remain open in the session.
connectionClose
  :: Connection
  -- ^ the connection handle
  -> Maybe Config
  -- ^ configuration string
  -> IO ()
connectionClose (Connection _cursorPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_CONNECTION* _cursorPtr)->close($(WT_CONNECTION* _cursorPtr), $(char* config)) } |]

connectionDebugInfo
  :: Connection
  -> Maybe Config
  -> IO ()
connectionDebugInfo (Connection _cursorPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_CONNECTION* _cursorPtr)->debug_info($(WT_CONNECTION* _cursorPtr), $(char* config)) } |]

-- | Reconfigure a connection handle.
connectionReconfigure
  :: Connection
  -- ^ the connection handle
  -> Maybe Config
  -- ^ configuration string
  -> IO ()
connectionReconfigure (Connection _cursorPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_CONNECTION* _cursorPtr)->reconfigure($(WT_CONNECTION *_cursorPtr), $(char* config)) } |]

-- | The home directory of the connection.
connectionGetHome
  :: Connection
  -- ^ the connection handle
  -> IO String
connectionGetHome (Connection _cursorPtr) =
  peekCString
    =<< [C.exp| const char* { $(WT_CONNECTION* _cursorPtr)->get_home($(WT_CONNECTION* _cursorPtr)) } |]

-- | Return if opening this handle created the database.
connectionIsNew
  :: Connection
  -- ^ the connection handle
  -> IO Bool
connectionIsNew (Connection _cursorPtr) =
  toBool
    <$> [C.exp| bool { $(WT_CONNECTION* _cursorPtr)->is_new($(WT_CONNECTION* _cursorPtr)) } |]

-- | Open a session.
connectionOpenSession
  :: Connection
  -- ^ the connection handle
  -> Maybe Config
  -- ^ configuration string
  -> IO Session
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

-- | Close the session handle.
--
-- This will release the resources associated with the session handle,
-- including rolling back any active transactions and closing any cursors
-- that remain open in the session.
sessionClose
  :: Session
  -- ^ the session handle
  -> Maybe Config
  -- ^ configuration string
  -> IO ()
sessionClose (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_SESSION* sPtr)->close($(WT_SESSION* sPtr), $(char* config)) } |]

-- | Reconfigure a session handle.
--
-- sessionReconfigure will fail if a transaction is in progress in the session.
-- All cursors are reset.
sessionReconfigure
  :: Session
  -- ^ the session handle
  -> Maybe Config
  -- ^ configuration string
  -> IO ()
sessionReconfigure (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_SESSION* sPtr)->reconfigure($(WT_SESSION* sPtr), $(char* config)) } |]

-- | Open a new cursor on a data source or duplicate an existing cursor.
--
-- > error_check(session->open_cursor(session, "table:mytable", NULL, NULL, &cursor));
--
-- An existing cursor can be duplicated by passing it as the to_dup
-- parameter and setting the uri parameter to NULL:
--
-- > error_check(session->open_cursor(session, "table:mytable", NULL, NULL, &cursor));
-- > cursor->set_key(cursor, key);
-- > error_check(cursor->search(cursor));
-- > /* Duplicate the cursor. */
-- > error_check(session->open_cursor(session, NULL, cursor, NULL, &duplicate));
--
-- Cursors being duplicated must have a key set, and successfully
-- duplicated cursors are positioned at the same place in the data source
-- as the original.
--
-- Cursor handles should be discarded by calling WT_CURSOR::close.
--
-- Cursors capable of supporting transactional operations operate in the
-- context of the current transaction, if any.
--
-- WT_SESSION::rollback_transaction implicitly resets all cursors.
--
-- Cursors are relatively light-weight objects but may hold references to
-- heavier-weight objects; applications should re-use cursors when
-- possible, but instantiating new cursors is not so expensive that
-- applications need to cache cursors at all cost.
sessionOpenCursor
  :: Session
  -- ^ the session handle
  -> Uri
  -- ^ the data source on which the cursor operates; cursors are usually
  -- opened on tables, however, cursors can be opened on any data source,
  -- regardless of whether it is ultimately stored in a table. Some cursor
  -- types may have limited functionality (for example, they may be
  -- read-only or not support transactional updates). See Data Sources for
  -- more information.
  -> Maybe Config
  -> IO Cursor
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
        Cursor <$> peek _cursorPtr <*> newIORef empty <*> newIORef empty

-- | Alter a table.
--
-- This will allow modification of some table settings after creation.
--
-- This method requires exclusive access to the specified data source(s).
-- If any cursors are open with the specified name(s) or a data source is
-- otherwise in use, the call will fail and return EBUSY.
-- sessionAlter
--
-- > error_check(session->alter(session, "table:mytable", "access_pattern_hint=random"));
--
sessionAlter
  :: Session
  -> String
  -> Maybe Config
  -> IO ()
sessionAlter (Session sPtr) name mConfig =
  withCString name $ \cName ->
    withNullableCString mConfig $ \config ->
      withErrorCheck
        [C.exp| int { $(WT_SESSION* sPtr)->alter($(WT_SESSION* sPtr), $(char* cName), $(char* config)) } |]

-- | Create a table, column group, index or file.
--
-- This method is not transactional, and will not guarantee ACID
-- properties, see Transactions for more details.
--
-- > error_check(session->create(session, "table:mytable", "key_format=S,value_format=S"));
--
sessionCreate
  :: Session
  -> String
  -> Maybe Config
  -> IO ()
sessionCreate (Session sPtr) name mConfig =
  withCString name $ \cName ->
    withNullableCString mConfig $ \config ->
      withErrorCheck
        [C.exp| int { $(WT_SESSION* sPtr)->create($(WT_SESSION* sPtr), $(char* cName), $(char* config)) } |]

-- | Drop (delete) an object.
--
-- This method requires exclusive access to the specified data source(s).
-- If any cursors are open with the specified name(s) or a data source is
-- otherwise in use, the call will fail and return EBUSY.
--
-- This method is not transactional, and will not guarantee ACID
-- properties, see Transactions for more details.
--
-- > error_check(session->drop(session, "table:mytable", NULL));
--
sessionDrop
  :: Session
  -> String
  -> Maybe Config
  -> IO ()
sessionDrop (Session sPtr) name mConfig =
  withCString name $ \cName ->
    withNullableCString mConfig $ \config ->
      withErrorCheck
        [C.exp| int { $(WT_SESSION* sPtr)->drop($(WT_SESSION* sPtr), $(char* cName), $(char* config)) } |]

-- | Rename an object.
--
-- This method is not transactional, and will not guarantee ACID
-- properties, see Transactions for more details.
--
-- > error_check(session->rename(session, "table:old", "table:new", NULL));
--
-- This method requires exclusive access to the specified data source(s).
-- If any cursors are open with the specified name(s) or a data source is
-- otherwise in use, the call will fail and return EBUSY.
sessionRename
  :: Session
  -> String
  -> String
  -> Maybe Config
  -> IO ()
sessionRename (Session sPtr) uri newUri mConfig =
  withCString uri $ \cUri ->
    withCString newUri $ \cNewUri ->
      withNullableCString mConfig $ \config ->
        withErrorCheck
          [C.exp| int { $(WT_SESSION* sPtr)->rename($(WT_SESSION* sPtr), $(char* cUri), $(char* cNewUri), $(char* config)) } |]

-- | Reset the session handle.
--
-- This method resets all cursors associated with this session, discards
-- cached resources and resets session statistics. The session can be
-- re-used immediately after this call returns. If a transaction is running
-- on this session, then this call takes no action and return an error.
--
-- > error_check(session->reset(session));
--
sessionReset
  :: Session
  -> IO ()
sessionReset (Session sPtr) =
  withErrorCheck
    [C.exp| int { $(WT_SESSION* sPtr)->reset($(WT_SESSION* sPtr)) } |]

-- | Upgrade a table or file.
--
-- Upgrade upgrades a table or file, if upgrade is required.
--
-- This method requires exclusive access to the specified data source(s).
-- If any cursors are open with the specified name(s) or a data source is
-- otherwise in use, the call will fail and return EBUSY.
--
-- > error_check(session->upgrade(session, "table:mytable", NULL));
--
sessionUpgrade
  :: Session
  -> String
  -> Maybe Config
  -> IO ()
sessionUpgrade (Session sPtr) name mConfig =
  withCString name $ \cName ->
    withNullableCString mConfig $ \config ->
      withErrorCheck
        [C.exp| int { $(WT_SESSION* sPtr)->upgrade($(WT_SESSION* sPtr), $(char* cName), $(char* config)) } |]

-- | Verify a table or file.
--
-- Verify reports if a file, or the files of which a table is comprised,
-- have been corrupted. The WT_SESSION::salvage method can be used to
-- repair a corrupted file,
--
-- > error_check(session->verify(session, "table:mytable", NULL));
--
-- This method requires exclusive access to the specified data source(s).
-- If any cursors are open with the specified name(s) or a data source is
-- otherwise in use, the call will fail and return EBUSY.
--
sessionVerify
  :: Session
  -> String
  -> Maybe Config
  -> IO ()
sessionVerify (Session sPtr) name mConfig =
  withCString name $ \cName ->
    withNullableCString mConfig $ \config ->
      withErrorCheck
        [C.exp| int { $(WT_SESSION* sPtr)->verify($(WT_SESSION* sPtr), $(char* cName), $(char* config)) } |]

-- | Start a transaction in this session.
--
-- The transaction remains active until ended by
-- WT_SESSION::commit_transaction or WT_SESSION::rollback_transaction.
-- Operations performed on cursors capable of supporting transactional
-- operations that are already open in this session, or which are opened
-- before the transaction ends, will operate in the context of the
-- transaction.
--
-- This method must not be called on a session with an active transaction.
--
sessionBeginTransaction
  :: Session
  -> Maybe Config
  -> IO ()
sessionBeginTransaction (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_SESSION* sPtr)->begin_transaction($(WT_SESSION* sPtr), $(char* config)) } |]

-- | Commit the current transaction.
--
-- A transaction must be in progress when this method is called.
--
-- If WT_SESSION::commit_transaction returns an error, the transaction was
-- rolled back, not committed.
--
-- This method must be called on a session with an active transaction.
--
sessionCommitTransaction
  :: Session
  -> Maybe Config
  -> IO ()
sessionCommitTransaction (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_SESSION* sPtr)->commit_transaction($(WT_SESSION* sPtr), $(char* config)) } |]

-- | Prepare the current transaction.
--
-- A transaction must be in progress when this method is called.
--
-- Preparing a transaction will guarantee a subsequent commit will succeed.
-- Only commit and rollback are allowed on a transaction after it has been
-- prepared. The transaction prepare API is designed to support MongoDB
-- exclusively, and guarantees update conflicts have been resolved, but
-- does not guarantee durability.
--
-- This method must be called on a session with an active transaction.
--
sessionPrepareTransaction
  :: Session
  -> Maybe Config
  -> IO ()
sessionPrepareTransaction (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_SESSION* sPtr)->prepare_transaction($(WT_SESSION* sPtr), $(char* config)) } |]

-- | Roll back the current transaction.
--
-- A transaction must be in progress when this method is called.
--
-- All cursors are reset.
--
-- This method must be called on a session with an active transaction.
--
sessionRollbackTransaction
  :: Session
  -> Maybe Config
  -> IO ()
sessionRollbackTransaction (Session sPtr) mConfig =
  withNullableCString mConfig $ \config ->
    withErrorCheck
      [C.exp| int { $(WT_SESSION* sPtr)->rollback_transaction($(WT_SESSION* sPtr), $(char* config)) } |]

-- | The name of the data source for the cursor, matches the uri parameter
-- to WT_SESSION::open_cursor used to open the cursor.
cursorUri
  :: Cursor
  -> IO String
cursorUri Cursor {_cursorPtr} =
  [C.exp| const char* { $(WT_CURSOR* _cursorPtr)->uri } |] >>= peekCString

-- | The format of the data packed into key items.
cursorKeyFormat
  :: Cursor
  -> IO String
cursorKeyFormat Cursor {_cursorPtr} =
  [C.exp| const char* { $(WT_CURSOR* _cursorPtr)->key_format } |] >>= peekCString

-- | The format of the data packed into value items.
cursorValueFormat
  :: Cursor
  -> IO String
cursorValueFormat Cursor {_cursorPtr} =
  [C.exp| const char* { $(WT_CURSOR* _cursorPtr)->value_format } |] >>= peekCString

-- | Get the key for the current record.
cursorGetKey
  :: Cursor
  -> IO ByteString
cursorGetKey Cursor {_cursorKey} = readIORef _cursorKey

-- | Get the value for the current record.
cursorGetValue
  :: Cursor
  -> IO ByteString
cursorGetValue Cursor {_cursorValue} = readIORef _cursorValue

-- | Set the key for the next operation.
cursorSetKey
  :: Cursor
  -> ByteString
  -> IO ()
cursorSetKey Cursor {_cursorPtr, _cursorKey} key = do
  writeIORef _cursorKey key
  [C.block|
    void {
      WT_ITEM item;
      item.data = $bs-ptr:key;
      item.size = $bs-len:key;
      $(WT_CURSOR* _cursorPtr)->set_key($(WT_CURSOR* _cursorPtr), &item);
    }
  |]

-- | Set the value for the next operation.
cursorSetValue
  :: Cursor
  -> ByteString
  -> IO ()
cursorSetValue Cursor {_cursorPtr, _cursorValue} value = do
  writeIORef _cursorValue value
  [C.block|
    void {
      WT_ITEM item;
      item.data = $bs-ptr:value;
      item.size = $bs-len:value;
      $(WT_CURSOR* _cursorPtr)->set_value($(WT_CURSOR* _cursorPtr), &item);
    }
  |]

-- | Return the next record.
cursorNext
  :: Cursor
  -> IO ()
cursorNext Cursor {_cursorPtr} =
  withErrorCheck
    [C.exp| int { $(WT_CURSOR* _cursorPtr)->next($(WT_CURSOR* _cursorPtr)) } |]

-- | Return the previous record.
cursorPrev
  :: Cursor
  -> IO ()
cursorPrev Cursor {_cursorPtr} =
  withErrorCheck
    [C.exp| int { $(WT_CURSOR* _cursorPtr)->prev($(WT_CURSOR* _cursorPtr)) } |]

-- | Reset the cursor.
--
-- Any resources held by the cursor are released, and the cursor's key and
-- position are no longer valid. Subsequent iterations with WT_CURSOR::next
-- will move to the first record, or with WT_CURSOR::prev will move to the
-- last record.
--
-- In the case of a statistics cursor, resetting the cursor refreshes the
-- statistics information returned. Resetting a session statistics cursor
-- resets all the session statistics values to zero.
--
cursorReset
  :: Cursor
  -> IO ()
cursorReset Cursor {_cursorPtr} =
  withErrorCheck
    [C.exp| int { $(WT_CURSOR* _cursorPtr)->reset($(WT_CURSOR* _cursorPtr)) } |]

-- | Return the record matching the key.
--
-- The key must first be set.
--
-- > const char *key = "some key";
-- > cursor->set_key(cursor, key);
-- > error_check(cursor->search(cursor));
--
-- On success, the cursor ends positioned at the returned record; to
-- minimize cursor resources, the WT_CURSOR::reset method should be called
-- as soon as the record has been retrieved and the cursor no longer needs
-- that position.
--
cursorSearch
  :: Cursor
  -> IO ()
cursorSearch Cursor {_cursorPtr} =
  withErrorCheck
    [C.exp| int { $(WT_CURSOR* _cursorPtr)->search($(WT_CURSOR* _cursorPtr)) } |]

-- | Insert a record and optionally update an existing record.
--
-- If the cursor was configured with "overwrite=true" (the default), both
-- the key and value must be set; if the record already exists, the key's
-- value will be updated, otherwise, the record will be inserted.
--
-- > /* Insert a new record or overwrite an existing record. */
-- > const char *key = "some key", *value = "some value";
-- > error_check(session->open_cursor(session, "table:mytable", NULL, NULL, &cursor));
-- > cursor->set_key(cursor, key);
-- > cursor->set_value(cursor, value);
-- > error_check(cursor->insert(cursor));
--
-- If the cursor was not configured with "overwrite=true", both the key and
-- value must be set and the record must not already exist; the record will
-- be inserted.
--
-- > /* Insert a new record and fail if the record exists. */
-- > const char *key = "new key", *value = "some value";
-- > error_check(
-- >   session->open_cursor(session, "table:mytable", NULL, "overwrite=false", &cursor));
-- > cursor->set_key(cursor, key);
-- > cursor->set_value(cursor, value);
-- > error_check(cursor->insert(cursor));
--
-- If a cursor with record number keys was configured with "append=true"
-- (not the default), the value must be set; a new record will be appended
-- and the record number set as the cursor key value.
--
-- > /* Insert a new record and assign a record number. */
-- > uint64_t recno;
-- > const char *value = "some value";
-- > cursor->set_value(cursor, value);
-- > error_check(cursor->insert(cursor));
-- > error_check(cursor->get_key(cursor, &recno));
--
-- The cursor ends with no position, and a subsequent call to the
-- WT_CURSOR::next (WT_CURSOR::prev) method will iterate from the beginning
-- (end) of the table.
--
-- If the cursor does not have record number keys or was not configured
-- with "append=true", the cursor ends with no key set and a subsequent
-- call to the WT_CURSOR::get_key method will fail. The cursor ends with no
-- value set and a subsequent call to the WT_CURSOR::get_value method will
-- fail.
--
-- Inserting a new record after the current maximum record in a
-- fixed-length bit field column-store (that is, a store with an 'r' type
-- key and 't' type value) may implicitly create the missing records as
-- records with a value of 0.
--
-- When loading a large amount of data into a new object, using a cursor
-- with the bulk configuration string enabled and loading the data in
-- sorted order will be much faster than doing out-of-order inserts. See
-- Bulk-load for more information.
--
-- The maximum length of a single column stored in a table is not fixed (as
-- it partially depends on the underlying file configuration), but is
-- always a small number of bytes less than 4GB.
--
cursorInsert
  :: Cursor
  -> IO ()
cursorInsert Cursor {_cursorPtr} =
  withErrorCheck
    [C.exp| int { $(WT_CURSOR* _cursorPtr)->insert($(WT_CURSOR* _cursorPtr)) } |]

-- | Update an existing record and optionally insert a record.
--
-- If the cursor was configured with "overwrite=true" (the default), both
-- the key and value must be set; if the record already exists, the key's
-- value will be updated, otherwise, the record will be inserted.
--
-- > const char *key = "some key", *value = "some value";
-- > error_check(session->open_cursor(session, "table:mytable", NULL, NULL, &cursor));
-- > cursor->set_key(cursor, key);
-- > cursor->set_value(cursor, value);
-- > error_check(cursor->update(cursor));
--
-- If the cursor was not configured with "overwrite=true", both the key and
-- value must be set and the record must already exist; the record will be
-- updated.
--
-- > const char *key = "some key", *value = "some value";
-- > error_check(
-- >   session->open_cursor(session, "table:mytable", NULL, "overwrite=false", &cursor));
-- > cursor->set_key(cursor, key);
-- > cursor->set_value(cursor, value);
-- > error_check(cursor->update(cursor));
--
-- On success, the cursor ends positioned at the modified record; to
-- minimize cursor resources, the WT_CURSOR::reset method should be called
-- as soon as the cursor no longer needs that position. (The
-- WT_CURSOR::insert method never keeps a cursor position and may be more
-- efficient for that reason.)
--
-- The maximum length of a single column stored in a table is not fixed (as
-- it partially depends on the underlying file configuration), but is
-- always a small number of bytes less than 4GB.
--
cursorUpdate
  :: Cursor
  -> IO ()
cursorUpdate Cursor {_cursorPtr} =
  withErrorCheck
    [C.exp| int { $(WT_CURSOR* _cursorPtr)->update($(WT_CURSOR* _cursorPtr)) } |]

-- | Remove a record.
--
-- The key must be set; the key's record will be removed if it exists, no
-- error will be returned if the record does not exist.
--
-- > const char *key = "some key";
-- > error_check(session->open_cursor(session, "table:mytable", NULL, NULL, &cursor));
-- > cursor->set_key(cursor, key);
-- > error_check(cursor->remove(cursor));
--
-- Any cursor position does not change: if the cursor was positioned before
-- the WT_CURSOR::remove call, the cursor remains positioned at the removed
-- record; to minimize cursor resources, the WT_CURSOR::reset method should
-- be called as soon as the cursor no longer needs that position. If the
-- cursor was not positioned before the WT_CURSOR::remove call, the cursor
-- ends with no position, and a subsequent call to the WT_CURSOR::next
-- (WT_CURSOR::prev) method will iterate from the beginning (end) of the
-- table.
--
-- > const char *key = "some key";
-- > error_check(
-- >   session->open_cursor(session, "table:mytable", NULL, "overwrite=false", &cursor));
-- > cursor->set_key(cursor, key);
-- > error_check(cursor->remove(cursor));
--
-- Removing a record in a fixed-length bit field column-store (that is, a
-- store with an 'r' type key and 't' type value) is identical to setting
-- the record's value to 0.
--
cursorRemove
  :: Cursor
  -> IO ()
cursorRemove Cursor {_cursorPtr} =
  withErrorCheck
    [C.exp| int { $(WT_CURSOR* _cursorPtr)->remove($(WT_CURSOR* _cursorPtr)) } |]

-- | Close the cursor.
--
-- This releases the resources associated with the cursor handle. Cursors
-- are closed implicitly by ending the enclosing connection or closing the
-- session in which they were opened.
--
-- > error_check(cursor->close(cursor));
--
cursorClose
  :: Cursor
  -> IO ()
cursorClose Cursor {_cursorPtr} =
  withErrorCheck
    [C.exp| int { $(WT_CURSOR* _cursorPtr)->close($(WT_CURSOR* _cursorPtr)) } |]
