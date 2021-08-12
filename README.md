# WiredTiger Haskell binding (unofficial)

## Why

There are not many key-value store libraries available for Haskell. This is
an attempt to make one. Also, for learning.

### Note

What I implemented is not going to work. I am grabbing a pointer from a
bytestring and giving it to the C side to hold on to.

```haskell
cursorSetKey :: Cursor -> ByteString -> IO ()
cursorSetKey (Cursor cPtr) key =
  [C.block|
    void {
      WT_ITEM item;
      item.data = $bs-ptr:key;
      item.size = $bs-len:key;
      $(WT_CURSOR* cPtr)->set_key($(WT_CURSOR* cPtr), &item);
    }
  |]
```

From the [docs](http://source.wiredtiger.com/3.2.1/cursor_ops.html#cursor_memory_scoping).

```
Cursor key/value memory scoping

When applications pass a pointer (either to a WT_ITEM or a string), to WT_CURSOR::set_key or WT_CURSOR::set_value, WiredTiger does not copy the memory referenced by the pointer. For this reason, the application must keep the referenced memory unchanged and valid until the next operation that successfully positions the cursor, modifies the underlying data, or the cursor is reset or closed (discarding its resources). The operations that position the cursor are WT_CURSOR::next, WT_CURSOR::prev, WT_CURSOR::search and WT_CURSOR::search_near; the operations that modify the underlying data are WT_CURSOR::insert, WT_CURSOR::update and WT_CURSOR::remove.

If a cursor operation fails (for example, due to a WT_ROLLBACK error), it may be retried without calling WT_CURSOR::set_key or WT_CURSOR::set_value again. That is, the cursor may still reference the application-supplied memory until the cursor is successfully positioned, underlying data is modified, or the cursor is closed or reset.

Any pointers returned by WT_CURSOR::get_key or WT_CURSOR::get_value are only valid until a subsequent cursor call that successfully positions the cursor, modifies the underlying data, or the cursor is reset or closed. These pointers may reference private WiredTiger data structures that may not be modified or freed by the application. If a longer scope is required, the application must make a copy of the memory before the cursor is re-used, closed or reset.
```

What should I do instead?

Thanks

