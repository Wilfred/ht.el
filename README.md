# ht.el

The missing hashmap utility library for Emacs.

## Functions

* `ht-create` `()`
* `ht-get` `(table key default?)`
* `ht-set` `(table key value)`
* `ht-remove` `(table key)`
* `ht-clear` `(table)`
* `ht-keys` `(table)`
* `ht-values` `(table)`
* `ht-items` `(table)`

## Why?

Common operations with hash tables (e.g. enumerate the keys) is too
difficult in Emacs lisp. ht.el provides a consistent naming scheme
(contrast `make-hash-table` with `puthash`) and (IMO) a more natural
argument ordering.

ht.el also deliberately returns `nil` from functions that modify the
hash table.
