<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.maps

Ordered maps: finite collections of key-value bindings, with at most one value per key.

Hydra maps are ordered collections: iteration -- elems, keys, toList -- is in ascending key order, under the total order defined in the ordering-and-equality specification.

Because key ordering is part of the map type's contract, every map-typed variable in this module's signatures carries the ordering constraint on its key type, including in primitives that perform no comparison themselves.

## alter

Alter a value at a key using a function which sees the optional current value.

`∀v, k.(ordering k) ⇒ alter(f: optional<v> → optional<v>, k: k, m: map<k, v>) → map<k, v>`

**Parameters**

- `f` (`optional<v> → optional<v>`): the update function, applied to the optional current value at the key
- `k` (`k`): the key whose binding to alter
- `m` (`map<k, v>`): the map to update

**Returns** `map<k, v>`

*Since: 0.15.*

alter(f, k, m) applies f to Just(v) when m contains key k with value v, or to Nothing when k is absent. If f returns Just(v'), the binding (k, v') is set in the result; if f returns Nothing, k is removed from the result.

A single primitive that subsumes insert, delete, and adjust.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.alter :: Ord k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v.

**[HYDRA-CORE-LIB-MAPS-ALTER-INSERTS-UPDATES-OR-DELETES] (requirement)** alter applies the given function to the optional current value at the key: if the function returns given(v'), that binding is set (inserting if absent, overwriting if present); if it returns none, the key is removed.

## bimap

Map functions over both the keys and values of a map.

`∀k1, k2, v1, v2.(ordering k1, ordering k2) ⇒ bimap(fk: k1 → k2, fv: v1 → v2, m: map<k1, v1>) → map<k2, v2>`

**Parameters**

- `fk` (`k1 → k2`): the function to apply to each key
- `fv` (`v1 → v2`): the function to apply to each value
- `m` (`map<k1, v1>`): the map to transform

**Returns** `map<k2, v2>`

*Since: 0.15.*

bimap(fk, fv, m) returns a new map with key fk(k) and value fv(v) for each binding (k, v) in m.

Key collisions after applying fk are resolved by keeping the last binding encountered (host may differ on collision policy if fk is not injective).

Requires 'ordering' constraints on both the input and output key types.

Total. Corresponds to a key-and-value lift of Haskell's Data.Map.fromList . map (\(k,v) -> (fk k, fv v)) . toList.

**[HYDRA-CORE-LIB-MAPS-BIMAP-TRANSFORMS-KEYS-AND-VALUES] (requirement)** bimap returns a new map where each binding (k, v) becomes (fk(k), fv(v)), applying the key function and value function independently to every binding.

**[HYDRA-CORE-LIB-MAPS-BIMAP-EMPTY-INPUT] (requirement)** bimap of the empty map is the empty map, regardless of the key and value functions.

## delete

Remove a key from a map.

`∀k, v.(ordering k) ⇒ delete(k: k, m: map<k, v>) → map<k, v>`

**Parameters**

- `k` (`k`): the key to remove
- `m` (`map<k, v>`): the map to remove the key from

**Returns** `map<k, v>`

*Since: 0.15.*

delete(k, m) returns m with the binding for k removed; if k is not present, m is returned unchanged.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.delete :: Ord k => k -> Map k v -> Map k v.

**[HYDRA-CORE-LIB-MAPS-DELETE-REMOVES-KEY-IF-PRESENT] (requirement)** delete removes the binding for the given key if present, leaving the rest of the map unchanged.

**[HYDRA-CORE-LIB-MAPS-DELETE-NO-OP-WHEN-ABSENT-OR-EMPTY] (requirement)** delete leaves the map unchanged when the key is absent, including when the map is empty.

## difference

Compute the difference of two maps by key.

`∀k, v.(ordering k) ⇒ difference(m1: map<k, v>, m2: map<k, v>) → map<k, v>`

**Parameters**

- `m1` (`map<k, v>`): the map whose bindings are retained
- `m2` (`map<k, v>`): the map whose keys are subtracted

**Returns** `map<k, v>`

*Since: 0.17.6.*

difference(m1, m2) returns the map containing exactly the bindings of m1 whose keys do not appear in m2. Only the key set of m2 matters; its values are ignored.

The key-set analogue is hydra.core.lib.sets.difference.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.difference :: Ord k => Map k v -> Map k v -> Map k v.

**[HYDRA-CORE-LIB-MAPS-DIFFERENCE-REMOVES-BINDINGS-WITH-KEYS-IN-SECOND-MAP] (requirement)** difference returns the bindings of the first map whose keys do not occur in the second map.

**[HYDRA-CORE-LIB-MAPS-DIFFERENCE-IDENTITY-WHEN-SECOND-EMPTY] (requirement)** difference with an empty second map returns the first map unchanged.

## elems

Return the values of a map (in key order).

`∀k, v.(ordering k) ⇒ elems(m: map<k, v>) → list<v>`

**Parameters**

- `m` (`map<k, v>`): the map whose values to return

**Returns** `list<v>`

*Since: 0.15.*

elems(m) returns the values of m as a list, ordered by their keys' ascending order.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.elems :: Map k v -> [v].

**[HYDRA-CORE-LIB-MAPS-ELEMS-RETURNS-VALUES-IN-KEY-ORDER] (requirement)** elems returns the map's values as a list ordered by ascending key, including when keys were inserted out of order.

**[HYDRA-CORE-LIB-MAPS-ELEMS-EMPTY-INPUT] (requirement)** elems of the empty map is the empty list.

## empty

The empty map.

`∀k, v.(ordering k) ⇒ empty : map<k, v>`

*Since: 0.15.*

empty is the map with no bindings.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.empty :: Map k v.

**[HYDRA-CORE-LIB-MAPS-EMPTY-HAS-NO-BINDINGS] (requirement)** empty is the map containing no bindings.

## filter

Filter a map by value.

`∀v, k.(ordering k) ⇒ filter(p: v → boolean, m: map<k, v>) → map<k, v>`

**Parameters**

- `p` (`v → boolean`): the predicate to test each value
- `m` (`map<k, v>`): the map to filter

**Returns** `map<k, v>`

*Since: 0.15.*

filter(p, m) returns the submap of m containing exactly the bindings (k, v) for which p(v) is true.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.filter :: (v -> Bool) -> Map k v -> Map k v.

**[HYDRA-CORE-LIB-MAPS-FILTER-KEEPS-MATCHING-VALUES] (requirement)** filter returns the submap containing exactly the bindings whose value satisfies the predicate, preserving all matching keys.

**[HYDRA-CORE-LIB-MAPS-FILTER-BOUNDARY-CASES] (requirement)** filter degrades correctly at the boundaries: a predicate matching no value yields the empty map; filtering the empty map yields the empty map.

## filterWithKey

Filter a map by key and value.

`∀k, v.(ordering k) ⇒ filterWithKey(p: k → v → boolean, m: map<k, v>) → map<k, v>`

**Parameters**

- `p` (`k → v → boolean`): the predicate to test each key and value
- `m` (`map<k, v>`): the map to filter

**Returns** `map<k, v>`

*Since: 0.15.*

filterWithKey(p, m) returns the submap of m containing exactly the bindings (k, v) for which p(k, v) is true.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.filterWithKey :: (k -> v -> Bool) -> Map k v -> Map k v.

**[HYDRA-CORE-LIB-MAPS-FILTER-WITH-KEY-KEEPS-MATCHING-BINDINGS] (requirement)** filterWithKey returns the submap containing exactly the bindings for which the predicate, given both key and value, is true.

**[HYDRA-CORE-LIB-MAPS-FILTER-WITH-KEY-BOUNDARY-CASES] (requirement)** filterWithKey degrades correctly at the boundaries: a predicate matching no binding yields the empty map; filtering the empty map yields the empty map.

## findWithDefault

Look up a value with a default if the key is absent.

`∀v, k.(ordering k) ⇒ findWithDefault(def: lazy v, k: k, m: map<k, v>) → v`

**Parameters**

- `def` (`v`): the default value, returned when the key is absent
- `k` (`k`): the key to look up
- `m` (`map<k, v>`): the map to search

**Returns** `v`

*Since: 0.15.*

findWithDefault(def, k, m) returns the value bound to k in m if k is present, or def otherwise. Equivalent to maybe(def, identity, lookup(k, m)).

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.findWithDefault :: Ord k => v -> k -> Map k v -> v.

**[HYDRA-CORE-LIB-MAPS-FIND-WITH-DEFAULT-RETURNS-BOUND-VALUE-OR-DEFAULT] (requirement)** findWithDefault returns the value bound to the key when present, or the supplied default value when the key is absent.

## fromList

Build a map from a list of key-value pairs.

`∀k, v.(ordering k) ⇒ fromList(xs: list<(k, v)>) → map<k, v>`

**Parameters**

- `xs` (`list<(k, v)>`): the list of key-value pairs to build the map from

**Returns** `map<k, v>`

*Since: 0.15.*

fromList(xs) returns the map containing exactly the bindings in xs. If xs contains multiple entries for the same key, the last one wins (matching Haskell's fromList behavior).

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.fromList :: Ord k => [(k, v)] -> Map k v.

**[HYDRA-CORE-LIB-MAPS-FROM-LIST-BUILDS-MAP-FROM-PAIRS] (requirement)** fromList returns the map containing exactly the bindings in the input list of key-value pairs.

**[HYDRA-CORE-LIB-MAPS-FROM-LIST-LAST-DUPLICATE-WINS] (requirement)** fromList resolves multiple entries for the same key by keeping the value from the last occurrence in the input list.

**[HYDRA-CORE-LIB-MAPS-FROM-LIST-EMPTY-INPUT] (requirement)** fromList of the empty list of pairs is the empty map.

## insert

Insert a key-value pair into a map.

`∀k, v.(ordering k) ⇒ insert(k: k, v: v, m: map<k, v>) → map<k, v>`

**Parameters**

- `k` (`k`): the key to insert
- `v` (`v`): the value to bind to the key
- `m` (`map<k, v>`): the map to insert into

**Returns** `map<k, v>`

*Since: 0.15.*

insert(k, v, m) returns m with the binding (k, v) added or updated. If k is already present, its value is overwritten.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.insert :: Ord k => k -> v -> Map k v -> Map k v.

**[HYDRA-CORE-LIB-MAPS-INSERT-ADDS-OR-OVERWRITES-BINDING] (requirement)** insert adds the given key-value binding to the map, overwriting any existing value at that key, including when the map was empty.

## intersection

Compute the intersection of two maps by key.

`∀k, v.(ordering k) ⇒ intersection(m1: map<k, v>, m2: map<k, v>) → map<k, v>`

**Parameters**

- `m1` (`map<k, v>`): the map whose bindings are retained
- `m2` (`map<k, v>`): the map whose key set is intersected

**Returns** `map<k, v>`

*Since: 0.17.6.*

intersection(m1, m2) returns the map containing exactly the bindings of m1 whose keys also appear in m2. On each common key the value is taken from m1; only the key set of m2 matters, and its values are ignored.

The key-set analogue is hydra.core.lib.sets.intersection.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.intersection :: Ord k => Map k v -> Map k v -> Map k v.

**[HYDRA-CORE-LIB-MAPS-INTERSECTION-KEEPS-BINDINGS-WITH-KEYS-IN-BOTH] (requirement)** intersection returns the bindings of the first map whose keys also occur in the second map, taking the value from the first map on each common key.

**[HYDRA-CORE-LIB-MAPS-INTERSECTION-EMPTY-WHEN-NO-OVERLAP-OR-EITHER-EMPTY] (requirement)** intersection is the empty map when the two maps share no keys, or when either map is empty.

## isEmpty

Test whether a map is empty.

`∀k, v.(ordering k) ⇒ isEmpty(m: map<k, v>) → boolean`

**Parameters**

- `m` (`map<k, v>`): the map to test

**Returns** `boolean`

*Since: 0.15.*

isEmpty(m) returns true iff m has no bindings.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.null :: Map k v -> Bool.

**[HYDRA-CORE-LIB-MAPS-IS-EMPTY-TESTS-ZERO-BINDINGS] (requirement)** isEmpty returns true exactly when the map has no bindings.

## keys

Return the keys of a map (in key order).

`∀k, v.(ordering k) ⇒ keys(m: map<k, v>) → list<k>`

**Parameters**

- `m` (`map<k, v>`): the map whose keys to return

**Returns** `list<k>`

*Since: 0.15.*

keys(m) returns the keys of m as a list, in ascending order.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.keys :: Map k v -> [k].

**[HYDRA-CORE-LIB-MAPS-KEYS-RETURNS-KEYS-IN-ORDER] (requirement)** keys returns the map's keys as a list in ascending order, including when keys were inserted out of order.

**[HYDRA-CORE-LIB-MAPS-KEYS-EMPTY-INPUT] (requirement)** keys of the empty map is the empty list.

## lookup

Look up a value in a map by key, returning Nothing if absent.

`∀k, v.(ordering k) ⇒ lookup(k: k, m: map<k, v>) → optional<v>`

**Parameters**

- `k` (`k`): the key to look up
- `m` (`map<k, v>`): the map to search

**Returns** `optional<v>`

*Since: 0.15.*

lookup(k, m) returns Just(v) where v is the value bound to k in m, or Nothing if k is not present.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.lookup :: Ord k => k -> Map k v -> Maybe v.

**[HYDRA-CORE-LIB-MAPS-LOOKUP-RETURNS-BOUND-VALUE] (requirement)** lookup returns the value bound to the given key, wrapped, when the key is present.

**[HYDRA-CORE-LIB-MAPS-LOOKUP-NONE-WHEN-ABSENT-OR-EMPTY] (requirement)** lookup returns none when the key is absent, including when the map is empty.

## map

Map a function over the values of a map.

`∀v1, v2, k.(ordering k) ⇒ map(f: v1 → v2, m: map<k, v1>) → map<k, v2>`

**Parameters**

- `f` (`v1 → v2`): the function to apply to each value
- `m` (`map<k, v1>`): the map to transform

**Returns** `map<k, v2>`

*Since: 0.15.*

map(f, m) returns a map with the same keys as m and value f(v) for each binding (k, v).

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.map :: (v -> w) -> Map k v -> Map k w / fmap on Map.

**[HYDRA-CORE-LIB-MAPS-MAP-APPLIES-FUNCTION-TO-VALUES] (requirement)** map applies the given function to every value in the map, leaving keys unchanged.

**[HYDRA-CORE-LIB-MAPS-MAP-EMPTY-INPUT] (requirement)** map over the empty map is the empty map, regardless of the function.

## mapKeys

Map a function over the keys of a map.

`∀k1, k2, v.(ordering k1, ordering k2) ⇒ mapKeys(f: k1 → k2, m: map<k1, v>) → map<k2, v>`

**Parameters**

- `f` (`k1 → k2`): the function to apply to each key
- `m` (`map<k1, v>`): the map to transform

**Returns** `map<k2, v>`

*Since: 0.15.*

mapKeys(f, m) returns a map where each binding (k, v) becomes (f(k), v). If f maps multiple keys to the same image, key collisions are resolved by keeping the binding with the greater original key (matching Haskell's mapKeys behavior).

Requires 'ordering' constraints on both the input and output key types.

Total. Corresponds to Haskell's Data.Map.mapKeys :: (Ord k1, Ord k2) => (k1 -> k2) -> Map k1 v -> Map k2 v.

**[HYDRA-CORE-LIB-MAPS-MAP-KEYS-APPLIES-FUNCTION-TO-KEYS] (requirement)** mapKeys applies the given function to every key in the map, leaving the bound values unchanged.

**[HYDRA-CORE-LIB-MAPS-MAP-KEYS-EMPTY-INPUT] (requirement)** mapKeys over the empty map is the empty map, regardless of the function.

## member

Test whether a key is present in a map.

`∀k, v.(ordering k) ⇒ member(k: k, m: map<k, v>) → boolean`

**Parameters**

- `k` (`k`): the key to test for
- `m` (`map<k, v>`): the map to test

**Returns** `boolean`

*Since: 0.15.*

member(k, m) returns true iff k is a key in m.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.member :: Ord k => k -> Map k v -> Bool.

**[HYDRA-CORE-LIB-MAPS-MEMBER-TESTS-KEY-PRESENCE] (requirement)** member returns true exactly when the given key is present in the map.

**[HYDRA-CORE-LIB-MAPS-MEMBER-FALSE-WHEN-ABSENT-OR-EMPTY] (requirement)** member returns false when the key is absent, including when the map is empty.

## singleton

Construct a map with a single key-value pair.

`∀k, v.(ordering k) ⇒ singleton(k: k, v: v) → map<k, v>`

**Parameters**

- `k` (`k`): the key of the single binding
- `v` (`v`): the value of the single binding

**Returns** `map<k, v>`

*Since: 0.15.*

singleton(k, v) returns the map containing exactly the binding (k, v).

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.singleton :: k -> v -> Map k v.

**[HYDRA-CORE-LIB-MAPS-SINGLETON-WRAPS-ONE-BINDING] (requirement)** singleton returns the map containing exactly the one given key-value binding.

## size

Return the number of key-value pairs in a map.

`∀k, v.(ordering k) ⇒ size(m: map<k, v>) → int32`

**Parameters**

- `m` (`map<k, v>`): the map whose bindings to count

**Returns** `int32`

*Since: 0.15.*

size(m) returns the number of bindings in m as an int32.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.size :: Map k v -> Int (with narrowing to int32).

**[HYDRA-CORE-LIB-MAPS-SIZE-COUNTS-BINDINGS] (requirement)** size returns the number of bindings in the map, including zero for the empty map.

## toList

Convert a map to a list of key-value pairs (in key order).

`∀k, v.(ordering k) ⇒ toList(m: map<k, v>) → list<(k, v)>`

**Parameters**

- `m` (`map<k, v>`): the map to convert

**Returns** `list<(k, v)>`

*Since: 0.15.*

toList(m) returns the bindings of m as a list of (key, value) pairs, in ascending key order.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.toList :: Map k v -> [(k, v)].

**[HYDRA-CORE-LIB-MAPS-TO-LIST-RETURNS-PAIRS-IN-KEY-ORDER] (requirement)** toList returns the map's bindings as a list of key-value pairs ordered by ascending key, including when keys were inserted out of order.

**[HYDRA-CORE-LIB-MAPS-TO-LIST-EMPTY-INPUT] (requirement)** toList of the empty map is the empty list.

## union

Compute the union of two maps; the first map's bindings take precedence on key collision.

`∀k, v.(ordering k) ⇒ union(m1: map<k, v>, m2: map<k, v>) → map<k, v>`

**Parameters**

- `m1` (`map<k, v>`): the map whose bindings take precedence
- `m2` (`map<k, v>`): the map whose remaining bindings are added

**Returns** `map<k, v>`

*Since: 0.15.*

union(m1, m2) returns the map containing all bindings from m1 plus the bindings of m2 whose keys are not in m1. On key collision, the binding from m1 is preferred.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.union :: Ord k => Map k v -> Map k v -> Map k v.

**[HYDRA-CORE-LIB-MAPS-UNION-FIRST-MAP-TAKES-PRECEDENCE] (requirement)** union returns the map containing all bindings from the first map plus the bindings of the second map whose keys are not in the first; on key collision the first map's binding is kept.

**[HYDRA-CORE-LIB-MAPS-UNION-IDENTITY-WITH-EMPTY] (requirement)** union with either map empty returns the other map unchanged.

## unions

Compute the left-biased union of a list of maps.

`∀k, v.(ordering k) ⇒ unions(ms: list<map<k, v>>) → map<k, v>`

**Parameters**

- `ms` (`list<map<k, v>>`): the list of maps to combine

**Returns** `map<k, v>`

*Since: 0.17.6.*

unions(ms) returns the map containing every binding of every map in ms; when a key occurs in more than one map, the binding from the earliest such map in the list wins.

unions(ms) is equivalent to folding union over ms from the left, starting from empty; unions([]) is empty, and the bias matches union and hydra.core.lib.sets.unions.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.Map.unions :: Ord k => [Map k v] -> Map k v.

**[HYDRA-CORE-LIB-MAPS-UNIONS-EARLIEST-MAP-TAKES-PRECEDENCE] (requirement)** unions returns the bindings of every map in the list; when a key occurs in more than one map, the binding from the earliest such map in the list is kept.

**[HYDRA-CORE-LIB-MAPS-UNIONS-EMPTY-LIST-OF-MAPS-YIELDS-EMPTY-MAP] (requirement)** unions of an empty list of maps is the empty map.
