<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Sets.hs (generator not yet
     built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.sets

Ordered sets: finite collections of distinct elements.
Hydra sets are ordered collections: iteration (`toList`) is in ascending element order, under
the total order defined in [ordering-and-equality.md](../ordering-and-equality.md).
Because element ordering is part of the set type's contract, every set-typed variable in this
module's signatures carries the `ordering` constraint on its element type, including in
primitives that perform no comparison themselves.

#### delete — **Draft**

`∀t. (ordering t) ⇒ t → set<t> → set<t>`

Usage: `delete x s`

Remove an element from a set.
Returns `s` with `x` removed; if `x` is not in `s`, `s` is returned unchanged.

Since: 0.15

#### difference — **Draft**

`∀t. (ordering t) ⇒ set<t> → set<t> → set<t>`

Usage: `difference s1 s2`

Compute the difference of two sets.
Returns the set of elements that are in `s1` but not in `s2`.
The map analogue (by key) is `hydra.lib.maps.difference`.

Since: 0.15

#### empty — **Draft**

`∀t. (ordering t) ⇒ set<t>`

Usage: `empty`

The set with no elements.
`null empty` is `true`, and `size empty` is `0`.

Since: 0.15

#### filter — **Draft**

`∀t. (ordering t) ⇒ (t → boolean) → set<t> → set<t>`

Usage: `filter p s`

Filter a set by a predicate.
Returns the subset of `s` containing exactly the elements `x` for which `p x` is `true`.

Since: 0.18

#### fromList — **Draft**

`∀t. (ordering t) ⇒ list<t> → set<t>`

Usage: `fromList xs`

Construct a set from a list of elements.
Returns the set containing exactly the distinct elements of `xs`; duplicates are discarded.
`toList (fromList xs)` is `xs` with duplicates removed and the remaining elements in ascending
order.

Since: 0.15

#### insert — **Draft**

`∀t. (ordering t) ⇒ t → set<t> → set<t>`

Usage: `insert x s`

Add an element to a set.
Returns `s` with `x` added; if `x` is already in `s`, `s` is returned unchanged.

Since: 0.15

#### intersection — **Draft**

`∀t. (ordering t) ⇒ set<t> → set<t> → set<t>`

Usage: `intersection s1 s2`

Compute the intersection of two sets.
Returns the set of elements that are present in both `s1` and `s2`.
The map analogue (by key) is `hydra.lib.maps.intersection`.

Since: 0.15

#### map — **Draft**

`∀t1,t2. (ordering t1, ordering t2) ⇒ (t1 → t2) → set<t1> → set<t2>`

Usage: `map f s`

Map a function over a set.
Returns the set of `f x` for each element `x` of `s`.
Elements that `f` maps to the same image are deduplicated, so the result may have fewer
elements than `s`; when `f` is injective, `size (map f s)` is `size s`.
The `ordering` constraint on the output element type `t2` is essential: the result set must
order and deduplicate its elements; the constraint on `t1` follows the module-wide contract.

Since: 0.15

#### member — **Draft**

`∀t. (ordering t) ⇒ t → set<t> → boolean`

Usage: `member x s`

Test whether an element is in a set.
Returns `true` exactly when `x` is an element of `s`.

Since: 0.15

#### null — **Draft**

`∀t. (ordering t) ⇒ set<t> → boolean`

Usage: `null s`

Test whether a set is empty.
Returns `true` exactly when `s` has no elements.

Since: 0.15

#### singleton — **Draft**

`∀t. (ordering t) ⇒ t → set<t>`

Usage: `singleton x`

Construct a set containing a single element.
Returns the set whose only element is `x`.

Since: 0.15

#### size — **Draft**

`∀t. (ordering t) ⇒ set<t> → int32`

Usage: `size s`

Return the number of elements in a set.
Returns the number of distinct elements of `s` as an `int32`.

Since: 0.15

#### toList — **Draft**

`∀t. (ordering t) ⇒ set<t> → list<t>`

Usage: `toList s`

Convert a set to a list.
Returns the elements of `s` as a list, in ascending order under the element type's total order.
The result contains no duplicates, and its length is `size s`.

Since: 0.15

#### union — **Draft**

`∀t. (ordering t) ⇒ set<t> → set<t> → set<t>`

Usage: `union s1 s2`

Compute the union of two sets.
Returns the set of elements that are in `s1` or in `s2` (or both).
`unions` generalizes this to a list of sets.

Since: 0.15

#### unions — **Draft**

`∀t. (ordering t) ⇒ list<set<t>> → set<t>`

Usage: `unions ss`

Compute the union of a list of sets.
Returns the set of elements that are in at least one set in `ss`.
`unions ss` is equivalent to folding `union` over `ss` from the left, starting from `empty`;
`unions []` is `empty`.

Since: 0.15
