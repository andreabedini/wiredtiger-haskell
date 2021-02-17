#include <wiredtiger.h>

/*
 * Cursor
 */

int cursor_get_key1(WT_CURSOR *cursor, char **key) {
  return cursor->get_key(cursor, key);
}

int cursor_get_value1(WT_CURSOR *cursor, char **value) {
  return cursor->get_value(cursor, value);
}

void cursor_set_key1(WT_CURSOR *cursor, char *key) {
  cursor->set_key(cursor, key);
}

void cursor_set_value1(WT_CURSOR *cursor, char *value) {
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
  return session->open_cursor(session, uri, to_dup, config, cursorp);
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

// connection_async_new_op

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
