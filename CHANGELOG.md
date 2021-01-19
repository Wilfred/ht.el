## v2.4 (not yet tagged)

No changes yet.

## v2.3

### Features

* Added `ht-empty-p` as an alias of `ht-empty?`.

* Many functions have been marked as `side-effect-free`, improving
  performance and byte-compiler warnings.

* `ht-get` and `ht-get*` can now be used with `setf`, e.g.

```
(setf (ht-get my-table my-key) new-value)
```

### Bug Fixes

* `ht<-plist` now keeps the first item in the list, if there are
duplicates. This makes it consistent with `ht<-alist` and
`plist-get`.

* `ht-get*` now handles an empty list of keys correctly, and will no
  longer stack overflow on large lists of keys.

* `ht-contains-p` now even supports hash tables that contain the key
`ht--not-found`.

## v2.2

* Added `ht-select-keys`
* Added `ht-get*`

ht.el now uses `defsubst` for many of its functions, which may improve
performance in byte-compiled code.

## v2.1

* `ht<-alist` and `ht<-plist` gained an optional argument `test` to
specify the equality predicate.
* Added `ht-equal?`.

## v2.0 -- API Change

Functions names have been changed to be more explicit and consistent.

Note that ht.el includes aliases, so v2.0 is fully backwards
compatible.

Mutation functions now always end with `!`, and `ht-delete-if` has
been renamed for consistency with its non-mutating equivalent
`ht-reject`.

* `ht-set` -> `ht-set!`
* `ht-update` -> `ht-update!`
* `ht-remove` -> `ht-remove!`
* `ht-clear` -> `ht-clear!`
* `ht-delete-if` -> `ht-reject!`

Predicates now always end with `?`.

* `ht-p` -> `ht?`
* `ht-contains-p` -> `ht-contains?`

Conversion functions now use `<-` and `->`.

* `ht-to-alist` -> `ht->alist`
* `ht-to-plist` -> `ht->plist`
* `ht-from-alist` -> `ht<-alist`
* `ht-from-plist` -> `ht<-plist`

## v1.6

* Added `ht-reject` and `ht-select`
* Added `ht-delete-if`
* Added `ht-find`
* Added `ht-empty?` and `ht-size`

Also added Travis configuration.

## v1.5

* `ht-aeach` now evaluates to `nil` as it should (use `ht-amap` if you
  want the resulting hash table).

## v1.4

* Added `ht-merge`.

## v1.3

* Removed runtime dependency on `cl`.

## v1.2

* Fixed various `void-variable` crashes due to `ht-amap` only being
  declared after its usage.

## v1.1

* Added `ht-contains-p`.

## v1.0 -- API Change

* `ht-map` now returns a list, as you'd expect a map function to do.
* Added `ht-each` for when you're only interested in side-effects.
* Added an anaphoric version of `ht-each`, `ht-aeach`.

## v0.11

* Added `ht-map` and an anaphoric version `ht-amap`.

## v0.10

* Added `ht-p`, an alias of `hash-table-p`, (mainly for completeness).

## v0.9

* Added `ht-update`.

## v0.8

* Added the `ht` macro to make hash table literals easy

## v0.7

* Added `ht-to-alist` and `ht-to-plist`

## v0.6

* Fixed a bug where `ht-from-alist` would overwrite the latest key-value
  association with older ones

## v0.5

* Added `ht-from-plist`

## v0.4

* Added `ht-from-alist`

## v0.3

* Added ht-copy

## v0.2

* Changed functions from hm-* to ht-* (Emacs doesn't use the term hash map)

## v0.1

* Initial release
