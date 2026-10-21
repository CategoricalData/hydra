<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.sets

<!-- Note: this is an automatically generated file. Do not edit. -->

Ordered sets: finite collections of distinct elements.

Hydra sets are ordered collections: iteration (toList) is in ascending element order, under the type's total order.

Because element ordering is part of the set type's contract, every set-typed variable in this module's signatures carries the ordering constraint on its element type, including in primitives that perform no comparison themselves.

## hydra.core.lib.sets.delete

Remove an element from a set.

`∀x.(ordering x) ⇒ x → set<x> → set<x>`

*Since: 0.15.*

delete(x, s) returns s with x removed; if x is not in s, s is returned unchanged.

Requires an 'ordering' constraint on the element type.

Total. Corresponds to Haskell's Data.Set.delete :: Ord a => a -> Set a -> Set a.

**[HYDRA-CORE-LIB-SETS-DELETE-REMOVES-ELEMENT] (requirement) **delete removes the given element from the set if present, leaving every other element unchanged.

**[HYDRA-CORE-LIB-SETS-DELETE-NO-OP-WHEN-ABSENT-OR-EMPTY] (requirement) **delete returns the set unchanged when the element is not present, including when the set is empty.

## hydra.core.lib.sets.difference

Compute the difference of two sets: elements in the first that are not in the second.

`∀x.(ordering x) ⇒ set<x> → set<x> → set<x>`

*Since: 0.15.*

difference(s1, s2) returns the set of elements that are in s1 but not in s2.

Requires an 'ordering' constraint on the element type.

Total. Corresponds to Haskell's Data.Set.difference :: Ord a => Set a -> Set a -> Set a.

**[HYDRA-CORE-LIB-SETS-DIFFERENCE-REMOVES-ELEMENTS-OF-SECOND-SET] (requirement) **difference returns the elements of the first set that do not occur in the second set.

**[HYDRA-CORE-LIB-SETS-DIFFERENCE-IDENTITY-WHEN-SECOND-EMPTY] (requirement) **difference with an empty second set returns the first set unchanged.

## hydra.core.lib.sets.empty

The empty set.

`∀x.(ordering x) ⇒ set<x>`

*Since: 0.15.*

empty is the set with no elements.

Requires an 'ordering' constraint on the element type.

Total. Corresponds to Haskell's Data.Set.empty :: Set a.

**[HYDRA-CORE-LIB-SETS-EMPTY-HAS-NO-ELEMENTS] (requirement) **empty is the set containing no elements, for any element type.

## hydra.core.lib.sets.filter

Filter a set by a predicate.

`∀x.(ordering x) ⇒ (x → boolean) → set<x> → set<x>`

*Since: 0.17.6.*

filter(p, s) returns the subset of s containing exactly the elements x for which p(x) is true.

Requires an 'ordering' constraint on the element type.

Total. Corresponds to Haskell's Data.Set.filter :: (a -> Bool) -> Set a -> Set a.

## hydra.core.lib.sets.fromList

Construct a set from a list of elements (duplicates removed).

`∀x.(ordering x) ⇒ list<x> → set<x>`

*Since: 0.15.*

fromList(xs) returns the set containing exactly the distinct elements of xs; duplicates are discarded.

Requires an 'ordering' constraint on the element type.

Total. Corresponds to Haskell's Data.Set.fromList :: Ord a => [a] -> Set a.

**[HYDRA-CORE-LIB-SETS-FROM-LIST-COLLECTS-DISTINCT-ELEMENTS] (requirement) **fromList returns the set of the distinct elements of the given list, discarding duplicates and order.

**[HYDRA-CORE-LIB-SETS-FROM-LIST-EMPTY-LIST-YIELDS-EMPTY-SET] (requirement) **fromList of the empty list is the empty set.

## hydra.core.lib.sets.insert

Add an element to a set.

`∀x.(ordering x) ⇒ x → set<x> → set<x>`

*Since: 0.15.*

insert(x, s) returns s with x added; if x is already in s, s is returned unchanged.

Requires an 'ordering' constraint on the element type.

Total. Corresponds to Haskell's Data.Set.insert :: Ord a => a -> Set a -> Set a.

**[HYDRA-CORE-LIB-SETS-INSERT-ADDS-ELEMENT] (requirement) **insert adds the given element to the set, leaving every other element unchanged.

**[HYDRA-CORE-LIB-SETS-INSERT-NO-OP-WHEN-ALREADY-PRESENT] (requirement) **insert returns the set unchanged when the element is already present.

## hydra.core.lib.sets.intersection

Compute the intersection of two sets: elements present in both.

`∀x.(ordering x) ⇒ set<x> → set<x> → set<x>`

*Since: 0.15.*

intersection(s1, s2) returns the set of elements present in both s1 and s2.

Requires an 'ordering' constraint on the element type.

Total. Corresponds to Haskell's Data.Set.intersection :: Ord a => Set a -> Set a -> Set a.

**[HYDRA-CORE-LIB-SETS-INTERSECTION-KEEPS-ELEMENTS-IN-BOTH] (requirement) **intersection returns exactly the elements present in both sets.

**[HYDRA-CORE-LIB-SETS-INTERSECTION-EMPTY-WHEN-NO-OVERLAP-OR-EITHER-EMPTY] (requirement) **intersection is the empty set when the two sets share no elements, or when either set is empty.

## hydra.core.lib.sets.isEmpty

Test whether a set is empty.

`∀x.(ordering x) ⇒ set<x> → boolean`

*Since: 0.15.*

isEmpty(s) returns true iff s has no elements.

Requires an 'ordering' constraint on the element type.

Total. Corresponds to Haskell's Data.Set.null :: Set a -> Bool.

**[HYDRA-CORE-LIB-SETS-IS-EMPTY-TESTS-ZERO-SIZE] (requirement) **isEmpty returns true exactly when the set has no elements.

## hydra.core.lib.sets.map

Map a function over a set.

`∀x,y.(ordering x, ordering y) ⇒ (x → y) → set<x> → set<y>`

*Since: 0.15.*

map(f, s) returns the set of f(x) for each x in s. Elements that f maps to the same image are deduplicated by the result type's ordering.

Requires 'ordering' constraints on both the input and output element types.

Total. Corresponds to Haskell's Data.Set.map :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b.

**[HYDRA-CORE-LIB-SETS-MAP-APPLIES-FUNCTION-TO-EACH-ELEMENT] (requirement) **map returns the set of applying the given function to each element, deduplicating results that collide under the output type's ordering.

**[HYDRA-CORE-LIB-SETS-MAP-EMPTY-INPUT] (requirement) **map over the empty set is the empty set, regardless of the function.

## hydra.core.lib.sets.member

Test whether an element is in a set.

`∀x.(ordering x) ⇒ x → set<x> → boolean`

*Since: 0.15.*

member(x, s) returns true iff x is an element of s.

Requires an 'ordering' constraint on the element type.

Total. Corresponds to Haskell's Data.Set.member :: Ord a => a -> Set a -> Bool.

**[HYDRA-CORE-LIB-SETS-MEMBER-TESTS-SET-MEMBERSHIP] (requirement) **member returns true exactly when the given value is an element of the set.

**[HYDRA-CORE-LIB-SETS-MEMBER-FALSE-WHEN-ABSENT-OR-EMPTY] (requirement) **member returns false when the value is not an element of the set, including when the set is empty.

## hydra.core.lib.sets.singleton

Construct a set containing a single element.

`∀x.(ordering x) ⇒ x → set<x>`

*Since: 0.15.*

singleton(x) returns the set containing exactly the element x.

Requires an 'ordering' constraint on the element type.

Total. Corresponds to Haskell's Data.Set.singleton :: a -> Set a.

**[HYDRA-CORE-LIB-SETS-SINGLETON-WRAPS-ONE-ELEMENT] (requirement) **singleton x returns the set containing exactly the element x.

## hydra.core.lib.sets.size

Return the number of elements in a set.

`∀x.(ordering x) ⇒ set<x> → int32`

*Since: 0.15.*

size(s) returns the number of elements in s as an int32.

Requires an 'ordering' constraint on the element type.

Total. Corresponds to Haskell's Data.Set.size :: Set a -> Int (with narrowing to int32).

**[HYDRA-CORE-LIB-SETS-SIZE-COUNTS-ELEMENTS] (requirement) **size returns the number of elements in the set, including zero for the empty set.

## hydra.core.lib.sets.toList

Convert a set to a list (in ascending order).

`∀x.(ordering x) ⇒ set<x> → list<x>`

*Since: 0.15.*

toList(s) returns the elements of s as a list, in ascending order under the element type's ordering.

Requires an 'ordering' constraint on the element type.

Total. Corresponds to Haskell's Data.Set.toAscList :: Set a -> [a].

**[HYDRA-CORE-LIB-SETS-TO-LIST-ORDERS-ASCENDING] (requirement) **toList returns the elements of the set as a list in ascending order under the element type's ordering, regardless of insertion order.

**[HYDRA-CORE-LIB-SETS-TO-LIST-EMPTY-SET-YIELDS-EMPTY-LIST] (requirement) **toList of the empty set is the empty list.

## hydra.core.lib.sets.union

Compute the union of two sets: elements in either.

`∀x.(ordering x) ⇒ set<x> → set<x> → set<x>`

*Since: 0.15.*

union(s1, s2) returns the set of elements that are in s1 or in s2 (or both).

Requires an 'ordering' constraint on the element type.

Total. Corresponds to Haskell's Data.Set.union :: Ord a => Set a -> Set a -> Set a.

**[HYDRA-CORE-LIB-SETS-UNION-KEEPS-ELEMENTS-IN-EITHER] (requirement) **union returns the elements that are in either set (or both), with no duplicates.

**[HYDRA-CORE-LIB-SETS-UNION-IDENTITY-WHEN-EITHER-EMPTY] (requirement) **union with either set empty returns the other set unchanged.

## hydra.core.lib.sets.unions

Compute the union of a list of sets.

`∀x.(ordering x) ⇒ list<set<x>> → set<x>`

*Since: 0.15.*

unions(ss) returns the union of every set in ss. Equivalent to folding union over ss starting from empty.

Requires an 'ordering' constraint on the element type.

Total. Corresponds to Haskell's Data.Set.unions :: Ord a => [Set a] -> Set a.

**[HYDRA-CORE-LIB-SETS-UNIONS-KEEPS-ELEMENTS-IN-ANY] (requirement) **unions returns the elements that occur in at least one of the given sets, equivalent to folding union over the list starting from empty.

**[HYDRA-CORE-LIB-SETS-UNIONS-EMPTY-LIST-OF-SETS-YIELDS-EMPTY-SET] (requirement) **unions of an empty list of sets is the empty set.
