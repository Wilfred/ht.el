# ht.el

The missing hash table utility library for Emacs.

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
* `ht-from-alist` `(alist)`
* `ht-from-plist` `(plist)`

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

## Running tests

`M-x ht-run-tests`

## What's an alist/plist?

An alist is an association list, which is a list of pairs. It looks like this:

    ((key1 . value1)
     (key2 . value2)
     (key3 . value3))
     
A plist is a property list, which is a flat list with an even number
of items. It looks like this:

    (key1 value1
     key2 value2
     key3 value3)

Both of these are slow. ht.el provides `ht-from-alist` and
`ht-from-plist` to help you convert to hash tables.
