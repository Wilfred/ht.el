# ht.el

The missing hashmap utility library for Emacs.

## Functions

* `ht-create`
* `ht-get`
* `ht-set`
* `ht-remove`
* `ht-clear`
* `ht-keys`
* `ht-values`
* `ht-items`

## Why?

Common operations with hash tables (e.g. enumerate the keys) is too
difficult in Emacs lisp. ht.el provides a consistent naming scheme
(contrast `make-hash-table` with `puthash`) and (IMO) a more natural
argument ordering.

ht.el also deliberately returns `nil` from functions that modify the
hash table.
