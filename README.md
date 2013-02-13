# hm.el

The missing hashmap utility library for Emacs.

## Functions

* `hm-create`
* `hm-get`
* `hm-set`
* `hm-remove`
* `hm-clear`
* `hm-keys`
* `hm-values`
* `hm-items`

## Why?

Common operations with hash tables (e.g. enumerate the keys) is too
difficult in Emacs lisp. hm.el provides a consistent naming scheme
(contrast `make-hash-table` with `puthash`) and (IMO) a more natural
argument ordering.
