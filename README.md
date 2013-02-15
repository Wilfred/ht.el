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
* `ht-copy` `(table)`

## Why?

Libraries like [s.el](https://github.com/magnars/s.el) (strings) and
[dash.el](https://github.com/magnars/dash.el) (lists) have shown how
much nicer Emacs lisp programming can be with good libraries. ht.el
aims to similarly simplify working with hash tables.

Common operations with hash tables (e.g. enumerate the keys) are too
difficult in Emacs lisp.

ht.el offers:

* A consistent naming scheme (contrast `make-hash-table` with `puthash`)
* A more natural argument ordering
* Mutation functions always return `nil`
* A more comprehensive range of hash table operations

### Similar libraries

* [kv.el](https://github.com/nicferrier/emacs-kv) (focuses more on alists)
