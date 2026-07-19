<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Maps.hs (generator not yet
     built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.maps

Ordered maps: finite collections of key-value bindings, with at most one value per key.
Hydra maps are ordered collections: iteration — `elems`, `keys`, `toList` — is in ascending key
order, under the total order defined in [ordering-and-equality.md](../ordering-and-equality.md).
Because key ordering is part of the map type's contract, every map-typed variable in this
module's signatures carries the `ordering` constraint on its key type, including in primitives
that perform no comparison themselves.

#### alter — **Draft**

`∀k,v. (ordering k) ⇒ (optional<v> → optional<v>) → k → map<k, v> → map<k, v>`

Usage: `alter f k m`

Transform the binding at a key using a function which sees the optional current value.
Applies `f` to `given v` when `m` contains a binding of `k` to `v`, and to `none` when `k` is
absent from `m`.
If `f` returns `given v'`, the result contains the binding of `k` to `v'` (added or updated);
if `f` returns `none`, the result contains no binding for `k` (removed if it was present).
All other bindings of `m` are unchanged.
A single primitive that subsumes `insert`, `delete`, and adjustment of an existing value.

Since: 0.15

#### bimap — **Draft**

`∀k1,k2,v1,v2. (ordering k1, ordering k2) ⇒ (k1 → k2) → (v1 → v2) → map<k1, v1> → map<k2, v2>`

Usage: `bimap fk fv m`

Map functions over both the keys and the values of a map.
Returns a map with a binding of `fk k` to `fv v` for each binding of `k` to `v` in `m`.
If `fk` maps distinct keys to the same image, the colliding bindings collapse to one: the
binding from the greatest colliding original key (the last in ascending original-key order)
supplies the value.
When `fk` is injective, the result has exactly as many bindings as `m`.
Compare `mapKeys`, which transforms keys only, and `map`, which transforms values only.

Since: 0.15

#### delete — **Draft**

`∀k,v. (ordering k) ⇒ k → map<k, v> → map<k, v>`

Usage: `delete k m`

Remove a key from a map.
Returns `m` with the binding for `k` removed; if `k` is not present, `m` is returned unchanged.

Since: 0.15

#### difference — **Draft**

`∀k,v. (ordering k) ⇒ map<k, v> → map<k, v> → map<k, v>`

Usage: `difference m1 m2`

Compute the difference of two maps by key.
Returns the map containing exactly the bindings of `m1` whose keys do not appear in `m2`.
Only the key set of `m2` matters; its values are ignored.
The key-set analogue is `hydra.lib.sets.difference`.

Since: 0.18

#### elems — **Draft**

`∀k,v. (ordering k) ⇒ map<k, v> → list<v>`

Usage: `elems m`

Return the values of a map.
Returns the values of `m` as a list, in ascending order of their keys.
Values are not deduplicated: the result has exactly one element per binding of `m`, so its
length is `size m`.

Since: 0.15

#### empty — **Draft**

`∀k,v. (ordering k) ⇒ map<k, v>`

Usage: `empty`

The map with no bindings.
`null empty` is `true`, and `size empty` is `0`.

Since: 0.15

#### filter — **Draft**

`∀k,v. (ordering k) ⇒ (v → boolean) → map<k, v> → map<k, v>`

Usage: `filter p m`

Filter a map by value.
Returns the submap of `m` containing exactly the bindings of `k` to `v` for which `p v` is
`true`.
Use `filterWithKey` when the predicate also needs to see the key.

Since: 0.15

#### filterWithKey — **Draft**

`∀k,v. (ordering k) ⇒ (k → v → boolean) → map<k, v> → map<k, v>`

Usage: `filterWithKey p m`

Filter a map by key and value.
Returns the submap of `m` containing exactly the bindings of `k` to `v` for which `p k v` is
`true`.

Since: 0.15

#### findWithDefault — **Draft**

`∀k,v. (ordering k) ⇒ v → k → map<k, v> → v`

Usage: `findWithDefault default k m`

Look up a value by key, with a default if the key is absent.
Returns the value bound to `k` in `m` if `k` is present, and `default` otherwise.
`findWithDefault default k m` is `hydra.lib.optionals.withDefault default (lookup k m)`; this
defining equation is the specification, and the default implementation.
The default comes first, an exception to the parameter-order conventions, matching
`hydra.lib.optionals.withDefault`.

Lazy: `default` — evaluated only when `k` is absent from `m`.

Since: 0.15

#### fromList — **Draft**

`∀k,v. (ordering k) ⇒ list<pair<k, v>> → map<k, v>`

Usage: `fromList xs`

Build a map from a list of key-value pairs.
Returns the map containing exactly the bindings in `xs`.
If `xs` contains multiple entries for the same key, the binding from the last such entry in
list order wins.
`fromList` and `toList` are inverses up to this normalization: `fromList (toList m)` is `m`.

Since: 0.15

#### insert — **Draft**

`∀k,v. (ordering k) ⇒ k → v → map<k, v> → map<k, v>`

Usage: `insert k v m`

Insert a key-value pair into a map.
Returns `m` with the binding of `k` to `v` added; if `k` is already present, its value is
overwritten with `v`.

Since: 0.15

#### intersection — **Draft**

`∀k,v. (ordering k) ⇒ map<k, v> → map<k, v> → map<k, v>`

Usage: `intersection m1 m2`

Compute the intersection of two maps by key.
Returns the map containing exactly the bindings of `m1` whose keys also appear in `m2`.
On each common key the value is taken from `m1`; only the key set of `m2` matters, and its
values are ignored.
The key-set analogue is `hydra.lib.sets.intersection`.

Since: 0.18

#### keys — **Draft**

`∀k,v. (ordering k) ⇒ map<k, v> → list<k>`

Usage: `keys m`

Return the keys of a map.
Returns the keys of `m` as a list, in ascending order.
The result contains no duplicates, and its length is `size m`.

Since: 0.15

#### lookup — **Draft**

`∀k,v. (ordering k) ⇒ k → map<k, v> → optional<v>`

Usage: `lookup k m`

Look up a value in a map by key.
Returns `given v` where `v` is the value bound to `k` in `m`, or `none` if `k` is not present.
Use `member` for a bare presence test, and `findWithDefault` to supply a fallback value.

Since: 0.15

#### map — **Draft**

`∀k,v1,v2. (ordering k) ⇒ (v1 → v2) → map<k, v1> → map<k, v2>`

Usage: `map f m`

Map a function over the values of a map.
Returns the map with the same keys as `m` and the value `f v` for each binding of `k` to `v`.
Keys are untouched, so the result always has exactly as many bindings as `m`.

Since: 0.15

#### mapKeys — **Draft**

`∀k1,k2,v. (ordering k1, ordering k2) ⇒ (k1 → k2) → map<k1, v> → map<k2, v>`

Usage: `mapKeys f m`

Map a function over the keys of a map.
Returns a map in which each binding of `k` to `v` in `m` becomes a binding of `f k` to `v`.
If `f` maps distinct keys to the same image, the colliding bindings collapse to one: the
binding whose original key is greatest (in the ascending order of `k1`) wins.
When `f` is injective, the result has exactly as many bindings as `m`.

Since: 0.15

#### member — **Draft**

`∀k,v. (ordering k) ⇒ k → map<k, v> → boolean`

Usage: `member k m`

Test whether a key is present in a map.
Returns `true` exactly when `k` is a key of `m`.

Since: 0.15

#### null — **Draft**

`∀k,v. (ordering k) ⇒ map<k, v> → boolean`

Usage: `null m`

Test whether a map is empty.
Returns `true` exactly when `m` has no bindings.

Since: 0.15

#### singleton — **Draft**

`∀k,v. (ordering k) ⇒ k → v → map<k, v>`

Usage: `singleton k v`

Construct a map with a single binding.
Returns the map containing exactly the binding of `k` to `v`.

Since: 0.15

#### size — **Draft**

`∀k,v. (ordering k) ⇒ map<k, v> → int32`

Usage: `size m`

Return the number of bindings in a map.
Returns the number of key-value bindings in `m` as an `int32`.

Since: 0.15

#### toList — **Draft**

`∀k,v. (ordering k) ⇒ map<k, v> → list<pair<k, v>>`

Usage: `toList m`

Convert a map to a list of key-value pairs.
Returns the bindings of `m` as a list of pairs, in ascending key order.
`fromList (toList m)` is `m`.

Since: 0.15

#### union — **Draft**

`∀k,v. (ordering k) ⇒ map<k, v> → map<k, v> → map<k, v>`

Usage: `union m1 m2`

Compute the left-biased union of two maps.
Returns the map containing all bindings of `m1`, plus the bindings of `m2` whose keys do not
appear in `m1`.
On key collision the binding from `m1` (the first map) wins.
`unions` generalizes this to a list of maps, with the same bias.

Since: 0.15

#### unions — **Draft**

`∀k,v. (ordering k) ⇒ list<map<k, v>> → map<k, v>`

Usage: `unions ms`

Compute the left-biased union of a list of maps.
Returns the map containing every binding of every map in `ms`; when a key occurs in more than
one map, the binding from the earliest such map in the list wins.
`unions ms` is equivalent to folding `union` over `ms` from the left, starting from `empty`.
`unions []` is `empty`, and the bias matches `union` and `hydra.lib.sets.unions`.

Since: 0.18
