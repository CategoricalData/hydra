<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.lists

<!-- Note: this is an automatically generated file. Do not edit. -->

Lists form a monad: singleton is its unit and bind its bind, with map, apply, and compose completing the functor, applicative, and Kleisli structure.

The traversal family mapList, mapOptional, and mapSet traverses a container in the list monad, and foldList is the monadic fold.

The bare foldl and foldr are the plain, non-monadic folds.

## hydra.core.lib.lists.apply

Apply a list of functions to a list of values (applicative style).

`∀x,y.list<x → y> → list<x> → list<y>`

*Since: 0.15.*

apply(fs, xs) returns the list of all f x for f in fs and x in xs, in order: for each f in fs (outer loop), for each x in xs (inner loop).

Equivalent to the applicative list instance.

Total. Corresponds to Haskell's (<*>) :: [a -> b] -> [a] -> [b].

**[HYDRA-CORE-LIB-LISTS-APPLY-SEQUENTIAL-COMPOSITION] (requirement) **apply threads the input list through every function in the function list in order, concatenating each function's output.

**[HYDRA-CORE-LIB-LISTS-APPLY-IDENTITY-CASES] (requirement) **apply with an empty function list, or an empty input list, or a single function/input, degrades correctly: zero functions or zero inputs yields an empty result; one of either acts as a direct map.

## hydra.core.lib.lists.at

Return the element at the given index, or Nothing if out of bounds.

`∀x.int32 → list<x> → optional<x>`

*Since: 0.17.6.*

at(i, xs) returns Just(xs[i]) if 0 <= i < length(xs), or Nothing otherwise.

Total. Wraps the Haskell (!!) operator, which is partial, in maybe to make out-of-bounds total.

Renamed from hydra.lib.lists.maybeAt.

**[HYDRA-CORE-LIB-LISTS-AT-RETURNS-ELEMENT-AT-INDEX] (requirement) **at returns the element at the given zero-based index, wrapped, for any valid index including the first and last.

**[HYDRA-CORE-LIB-LISTS-AT-NONE-WHEN-OUT-OF-RANGE] (requirement) **at returns none when the index is negative, at or beyond the list's length, or when the list is empty.

## hydra.core.lib.lists.bind

Apply a function that returns lists to each element and flatten the results.

`∀x,y.list<x> → (x → list<y>) → list<y>`

*Since: 0.15.*

bind(xs, f) applies f to each element of xs and concatenates the resulting lists in order. Equivalent to concatMap.

Total. Corresponds to Haskell's (>>=) :: [a] -> (a -> [b]) -> [b].

**[HYDRA-CORE-LIB-LISTS-BIND-FLAT-MAPS-OVER-ELEMENTS] (requirement) **bind applies a list-producing function to each element and concatenates the results, in order, including when the function returns duplicate or negative values.

**[HYDRA-CORE-LIB-LISTS-BIND-EMPTY-INPUT] (requirement) **bind over an empty list is the empty list, regardless of the function supplied.

## hydra.core.lib.lists.compose

Compose two functions that return lists (Kleisli composition in the list monad).

`∀x,y,z.(x → list<y>) → (y → list<z>) → x → list<z>`

*Since: 0.17.6.*

compose(f, g, x) is bind(f(x), g); this defining equation is the specification. The results of applying g to each element of f(x) are concatenated in order.

Total. Corresponds to Kleisli composition (>=>) in the list monad.

## hydra.core.lib.lists.concat

Concatenate a list of lists.

`∀x.list<list<x>> → list<x>`

*Since: 0.15.*

concat(xss) returns the list obtained by appending all the lists in xss in order.

Total. Corresponds to Haskell's concat :: [[a]] -> [a].

**[HYDRA-CORE-LIB-LISTS-CONCAT-FLATTENS-PRESERVING-ORDER] (requirement) **concat flattens a list of lists into a single list, preserving element order within and across the constituent lists.

**[HYDRA-CORE-LIB-LISTS-CONCAT-EMPTY-INPUTS] (requirement) **concat of a list of lists in which every list is empty, or of an empty list of lists, is the empty list.

## hydra.core.lib.lists.concat2

Concatenate two lists.

`∀x.list<x> → list<x> → list<x>`

*Since: 0.15.*

concat2(xs, ys) returns xs with ys appended.

Total. Corresponds to Haskell's (++) :: [a] -> [a] -> [a].

**[HYDRA-CORE-LIB-LISTS-CONCAT2-APPENDS-PRESERVING-ORDER] (requirement) **concat2 appends its second list argument to its first, preserving the order of both, for both numeric and string element types.

**[HYDRA-CORE-LIB-LISTS-CONCAT2-IDENTITY-ON-EMPTY] (requirement) **concat2 with either or both arguments empty returns the other argument unchanged (the empty list is the identity element).

## hydra.core.lib.lists.cons

Prepend a value to a list.

`∀x.x → list<x> → list<x>`

*Since: 0.15.*

cons(x, xs) returns a list whose head is x and whose tail is xs.

Total. Corresponds to Haskell's (:) :: a -> [a] -> [a].

**[HYDRA-CORE-LIB-LISTS-CONS-PREPENDS-ELEMENT] (requirement) **cons prepends its first argument to the front of its second argument (the list), for empty and non-empty lists, any element type, and any value including negative numbers.

## hydra.core.lib.lists.distinct

Remove duplicate elements from a list.

`∀x.(equality x) ⇒ list<x> → list<x>`

*Since: 0.17.6.*

distinct(xs) returns the list of distinct elements of xs, in the order of their first occurrence.

Requires an 'equality' constraint on the element type.

Total. Corresponds to Haskell's Data.List.nub :: Eq a => [a] -> [a].

Renamed from hydra.lib.lists.nub.

**[HYDRA-CORE-LIB-LISTS-DISTINCT-REMOVES-DUPLICATES-PRESERVING-FIRST-OCCURRENCE] (requirement) **distinct removes repeated elements, keeping only the first occurrence of each distinct value and preserving the relative order of the survivors, for numeric and string element types.

**[HYDRA-CORE-LIB-LISTS-DISTINCT-EMPTY-INPUT] (requirement) **distinct of an empty list is the empty list.

## hydra.core.lib.lists.drop

Drop the first n elements from a list.

`∀x.int32 → list<x> → list<x>`

*Since: 0.15.*

drop(n, xs) returns the suffix of xs after dropping the first n elements; if n is greater than or equal to length(xs) the result is the empty list; if n is non-positive the result is xs unchanged.

Total. Corresponds to Haskell's drop :: Int -> [a] -> [a].

**[HYDRA-CORE-LIB-LISTS-DROP-DROPS-FROM-FRONT] (requirement) **drop n removes the first n elements from the front of the list, or all elements if n is greater than or equal to the list's length.

**[HYDRA-CORE-LIB-LISTS-DROP-NON-POSITIVE-IS-IDENTITY] (requirement) **drop with n equal to zero or negative returns the list unchanged.

## hydra.core.lib.lists.dropWhile

Drop elements from the beginning of a list while the predicate is true.

`∀x.(x → boolean) → list<x> → list<x>`

*Since: 0.15.*

dropWhile(p, xs) returns the suffix of xs starting at the first element for which p returns false. If p is true for every element, the result is the empty list.

Total. Corresponds to Haskell's dropWhile :: (a -> Bool) -> [a] -> [a].

**[HYDRA-CORE-LIB-LISTS-DROP-WHILE-DROPS-LEADING-MATCHES] (requirement) **dropWhile removes elements from the front of the list for as long as the predicate holds, stopping at (and keeping) the first element for which it does not; an empty list, a predicate matching no elements, or a predicate matching every element are each handled correctly as boundary cases.

## hydra.core.lib.lists.filter

Filter a list by a predicate.

`∀x.(x → boolean) → list<x> → list<x>`

*Since: 0.15.*

filter(p, xs) returns the list of elements x in xs for which p(x) is true, in original order.

Total. Corresponds to Haskell's filter :: (a -> Bool) -> [a] -> [a].

**[HYDRA-CORE-LIB-LISTS-FILTER-KEEPS-MATCHING-ELEMENTS] (requirement) **filter returns the sublist of elements satisfying the predicate, preserving their relative order, whether the predicate matches some, all, or none of the elements.

**[HYDRA-CORE-LIB-LISTS-FILTER-EMPTY-INPUT] (requirement) **filter over an empty list is the empty list, regardless of the predicate.

## hydra.core.lib.lists.find

Find the first element matching a predicate.

`∀x.(x → boolean) → list<x> → optional<x>`

*Since: 0.15.*

find(p, xs) returns Just(x) where x is the first element of xs for which p(x) is true, or Nothing if no such element exists.

Total. Corresponds to Haskell's find :: (a -> Bool) -> [a] -> Maybe a.

**[HYDRA-CORE-LIB-LISTS-FIND-RETURNS-FIRST-MATCH] (requirement) **find returns the first element (wrapped) satisfying the predicate, scanning in list order.

**[HYDRA-CORE-LIB-LISTS-FIND-NONE-WHEN-NO-MATCH-OR-EMPTY] (requirement) **find returns none when no element satisfies the predicate, including when the list is empty.

## hydra.core.lib.lists.foldList

Left-fold a list in the list monad (a nondeterministic fold).

`∀x,y.(x → y → list<x>) → x → list<y> → list<x>`

*Since: 0.17.6.*

foldList(f, acc0, xs) folds xs from the left, branching the accumulator over every result of the step function at each element: after each element the set of accumulators is replaced by all results of applying f to each current accumulator and that element.

foldList(f, acc0, xs) is foldl(\macc el -> bind(macc, \acc -> f(acc, el)), singleton(acc0), xs); this defining equation is the specification. For the empty list the result is singleton(acc0).

Total on finite inputs. The list-monad instance of the monadic left fold.

For example, if f(acc, x) returns the two-element list containing acc and the sum of acc and x, then foldList(f, 0, xs) computes the sums of all subsets of xs.

## hydra.core.lib.lists.foldl

Left-fold a list with an accumulator.

`∀y,x.(y → x → y) → y → list<x> → y`

*Since: 0.15.*

foldl(f, acc0, xs) reduces xs left-associatively: foldl(f, acc0, [x1, x2, ..., xn]) = f(f(f(acc0, x1), x2), ..., xn). For the empty list the result is acc0.

Strict in the accumulator on hosts where laziness would otherwise leak space.

Total on finite inputs. Corresponds to Haskell's foldl' :: (b -> a -> b) -> b -> [a] -> b.

**[HYDRA-CORE-LIB-LISTS-FOLDL-APPLIES-LEFT-ASSOCIATIVELY] (requirement) **foldl combines the accumulator with each element left-to-right using the given binary operation, so order-sensitive operations (e.g. subtraction) reflect strict left associativity.

**[HYDRA-CORE-LIB-LISTS-FOLDL-EMPTY-LIST-RETURNS-ACCUMULATOR] (requirement) **foldl over an empty list returns the initial accumulator unchanged.

## hydra.core.lib.lists.foldr

Right-fold a list with an accumulator.

`∀x,y.(x → y → y) → y → list<x> → y`

*Since: 0.15.*

foldr(f, acc0, xs) reduces xs right-associatively: foldr(f, acc0, [x1, ..., xn]) = f(x1, f(x2, ..., f(xn, acc0))). For the empty list the result is acc0.

Total on finite inputs. Corresponds to Haskell's foldr :: (a -> b -> b) -> b -> [a] -> b.

**[HYDRA-CORE-LIB-LISTS-FOLDR-APPLIES-RIGHT-ASSOCIATIVELY] (requirement) **foldr combines each element with the accumulator right-to-left using the given binary operation, so order-sensitive operations (e.g. subtraction) reflect strict right associativity, and differ from the equivalent foldl where the operation is non-commutative.

**[HYDRA-CORE-LIB-LISTS-FOLDR-EMPTY-LIST-RETURNS-ACCUMULATOR] (requirement) **foldr over an empty list returns the initial accumulator unchanged.

## hydra.core.lib.lists.group

Group consecutive equal elements.

`∀x.(equality x) ⇒ list<x> → list<list<x>>`

*Since: 0.15.*

group(xs) returns a list of lists obtained by grouping consecutive equal elements of xs together. Each inner list is non-empty and contains equal elements; the concatenation of the result equals xs.

Equality is determined by the element type's 'equality' constraint.

Total. Corresponds to Haskell's Data.List.group :: Eq a => [a] -> [[a]].

**[HYDRA-CORE-LIB-LISTS-GROUP-GROUPS-CONSECUTIVE-DUPLICATES] (requirement) **group partitions the list into maximal runs of consecutive equal elements, preserving overall order; non-adjacent duplicates (e.g. [1,1,2,2,2,3,1]) are not merged into the same run.

**[HYDRA-CORE-LIB-LISTS-GROUP-EMPTY-INPUT] (requirement) **group of an empty list is the empty list.

## hydra.core.lib.lists.head

Return the first element, or Nothing if the list is empty.

`∀x.list<x> → optional<x>`

*Since: 0.17.6.*

head(xs) returns Just(x) where x is the first element of xs, or Nothing if xs is empty.

Total. Wraps Haskell's partial head in maybe.

Renamed from hydra.lib.lists.maybeHead.

**[HYDRA-CORE-LIB-LISTS-HEAD-RETURNS-FIRST-ELEMENT] (requirement) **head returns the first element of a non-empty list, wrapped, for any element type.

**[HYDRA-CORE-LIB-LISTS-HEAD-NONE-WHEN-EMPTY] (requirement) **head of an empty list is none, for any element type.

## hydra.core.lib.lists.init

Return all elements except the last, or Nothing if the list is empty.

`∀x.list<x> → optional<list<x>>`

*Since: 0.17.6.*

init(xs) returns Just(ys) where ys is xs with its last element removed, or Nothing if xs is empty.

Total. Wraps Haskell's partial init in maybe.

Renamed from hydra.lib.lists.maybeInit.

**[HYDRA-CORE-LIB-LISTS-INIT-RETURNS-ALL-BUT-LAST] (requirement) **init returns all elements of a non-empty list except the last, wrapped, including the empty list when the input has exactly one element.

**[HYDRA-CORE-LIB-LISTS-INIT-NONE-WHEN-EMPTY] (requirement) **init of an empty list is none.

## hydra.core.lib.lists.intersperse

Intersperse a value between consecutive elements of a list.

`∀x.x → list<x> → list<x>`

*Since: 0.15.*

intersperse(sep, xs) returns xs with sep inserted between each pair of adjacent elements; for lists of length 0 or 1 the input is returned unchanged.

Total. Corresponds to Haskell's intersperse :: a -> [a] -> [a].

**[HYDRA-CORE-LIB-LISTS-INTERSPERSE-INSERTS-ELEMENT-BETWEEN-ITEMS] (requirement) **intersperse inserts a copy of the given element between each pair of adjacent elements of the list (not before the first or after the last), for string and numeric element types.

**[HYDRA-CORE-LIB-LISTS-INTERSPERSE-NO-INSERTION-BELOW-TWO-ELEMENTS] (requirement) **intersperse on a list of zero or one elements returns the list unchanged -- there are no adjacent pairs to separate.

## hydra.core.lib.lists.isEmpty

Test whether a list is empty.

`∀x.list<x> → boolean`

*Since: 0.15.*

isEmpty(xs) returns true iff xs is the empty list.

Total. Corresponds to Haskell's null :: [a] -> Bool.

**[HYDRA-CORE-LIB-LISTS-IS-EMPTY-TESTS-ZERO-LENGTH] (requirement) **isEmpty returns true exactly when the list has no elements, for any element type.

## hydra.core.lib.lists.join

Intercalate a list of lists with a separator list between each.

`∀x.list<x> → list<list<x>> → list<x>`

*Since: 0.17.6.*

join(sep, xss) returns the concatenation of xss with sep inserted between consecutive lists. Equivalent to concat(intersperse(sep, xss)).

Total. Corresponds to Haskell's intercalate :: [a] -> [[a]] -> [a].

Renamed from hydra.lib.lists.intercalate.

**[HYDRA-CORE-LIB-LISTS-JOIN-INSERTS-SEPARATOR-BETWEEN-LISTS] (requirement) **join concatenates a list of lists with a copy of the separator list inserted between each pair of adjacent lists (not before the first or after the last), for separators of any length including empty, and skips the separator insertion where it has no visible effect (a single list, or lists containing empty lists).

**[HYDRA-CORE-LIB-LISTS-JOIN-EMPTY-LIST-OF-LISTS] (requirement) **join of an empty list of lists is the empty list, regardless of the separator.

## hydra.core.lib.lists.last

Return the last element, or Nothing if the list is empty.

`∀x.list<x> → optional<x>`

*Since: 0.17.6.*

last(xs) returns Just(x) where x is the last element of xs, or Nothing if xs is empty.

Total. Wraps Haskell's partial last in maybe.

Renamed from hydra.lib.lists.maybeLast.

**[HYDRA-CORE-LIB-LISTS-LAST-RETURNS-LAST-ELEMENT] (requirement) **last returns the final element of a non-empty list, wrapped, for any element type.

**[HYDRA-CORE-LIB-LISTS-LAST-NONE-WHEN-EMPTY] (requirement) **last of an empty list is none.

## hydra.core.lib.lists.length

Return the length of a list.

`∀x.list<x> → int32`

*Since: 0.15.*

length(xs) returns the number of elements in xs as an int32. Returns 0 for the empty list.

Total on finite inputs; the int32 result overflows for lists longer than 2^31-1 elements.

Corresponds to Haskell's length :: [a] -> Int (with narrowing to Int32).

**[HYDRA-CORE-LIB-LISTS-LENGTH-COUNTS-ELEMENTS] (requirement) **length returns the number of elements in the list, for any element type, including zero for the empty list.

## hydra.core.lib.lists.map

Map a function over a list.

`∀x,y.(x → y) → list<x> → list<y>`

*Since: 0.15.*

map(f, xs) returns the list of f(x) for each x in xs, in original order.

Total. Corresponds to Haskell's map :: (a -> b) -> [a] -> [b] / fmap on lists.

**[HYDRA-CORE-LIB-LISTS-MAP-APPLIES-FUNCTION-ELEMENTWISE] (requirement) **map applies the given function to each element of the list in place, preserving list order and length, for any element and function.

**[HYDRA-CORE-LIB-LISTS-MAP-IDENTITY-AND-EMPTY-CASES] (requirement) **map with the identity function returns the list unchanged; map over an empty list is the empty list.

## hydra.core.lib.lists.mapGivens

Map a partial function over a list, keeping only the present results.

`∀x,y.(x → optional<y>) → list<x> → list<y>`

mapGivens(f, xs) applies f to each element of xs and returns the list of contained values from given results in original order; none results are discarded.

Total. Corresponds to Haskell's Data.Maybe.mapMaybe :: (a -> Maybe b) -> [a] -> [b].

## hydra.core.lib.lists.mapList

Traverse a list in the list monad.

`∀x,y.(x → list<y>) → list<x> → list<list<y>>`

*Since: 0.17.6.*

mapList(f, xs) returns all combinations obtainable by choosing one element from f(x) for each x in xs: each result list has the same length as xs, with its element at each position drawn from the corresponding f(x).

Results appear in the lexicographic order of the choices, with the choice for the first element varying slowest. The number of results is the product of the lengths of the lists f(x); if f(x) is empty for any element, the result is the empty list; mapList(f, []) is singleton([]).

Total on finite inputs. The list-monad instance of the traversal family.

## hydra.core.lib.lists.mapOptional

Traverse an optional value in the list monad.

`∀x,y.(x → list<y>) → optional<x> → list<optional<y>>`

*Since: 0.17.6.*

mapOptional(f, m) returns the single-element list containing none when m is none; for given(x) it returns given(y) for each element y of f(x), in order.

Total on finite inputs. The list-monad instance of the traversal family, applied to the optional container.

## hydra.core.lib.lists.mapSet

Traverse a set in the list monad.

`∀x,y.(ordering x, ordering y) ⇒ (x → list<y>) → set<x> → list<set<y>>`

*Since: 0.17.6.*

mapSet(f, s) returns all sets obtainable by choosing one element from f(x) for each element x of s. Elements of s are traversed in ascending order, and results appear in the lexicographic order of the choices, with the choice for the least element varying slowest.

A result set may have fewer elements than s when distinct choices coincide. If f(x) is empty for any element, the result is the empty list; for the empty set the result is the single-element list containing the empty set.

Requires 'ordering' constraints on both element types (the set type contract).

## hydra.core.lib.lists.member

Test whether an element is in a list.

`∀x.(equality x) ⇒ x → list<x> → boolean`

*Since: 0.17.6.*

member(x, xs) returns true iff some element of xs is equal to x.

Requires an 'equality' constraint on the element type.

Total. Corresponds to Haskell's elem :: Eq a => a -> [a] -> Bool.

Renamed from hydra.lib.lists.elem.

**[HYDRA-CORE-LIB-LISTS-MEMBER-TESTS-SET-MEMBERSHIP] (requirement) **member returns true exactly when the given value occurs at least once in the list, for both numeric and string elements, including lists with duplicates.

**[HYDRA-CORE-LIB-LISTS-MEMBER-FALSE-WHEN-ABSENT-OR-EMPTY] (requirement) **member returns false when the value does not occur in the list, including when the list is empty.

## hydra.core.lib.lists.partition

Partition a list into elements that satisfy a predicate and those that do not.

`∀x.(x → boolean) → list<x> → (list<x>, list<x>)`

*Since: 0.15.*

partition(p, xs) returns a pair (yes, no) where yes is the list of elements of xs for which p is true and no is the list of elements for which p is false, each preserving original order.

Total. Corresponds to Haskell's partition :: (a -> Bool) -> [a] -> ([a], [a]).

**[HYDRA-CORE-LIB-LISTS-PARTITION-SPLITS-BY-SATISFACTION] (requirement) **partition returns a pair of (elements satisfying the predicate, elements not satisfying it), each sublist preserving the original relative order of its members.

**[HYDRA-CORE-LIB-LISTS-PARTITION-BOUNDARY-CASES] (requirement) **partition degrades correctly at the boundaries: a predicate matching every element yields (whole list, empty); matching none yields (empty, whole list); an empty input list yields (empty, empty).

## hydra.core.lib.lists.replicate

Build a list of n copies of a value.

`∀x.int32 → x → list<x>`

*Since: 0.15.*

replicate(n, x) returns a list of n copies of x; for n <= 0 the result is the empty list.

Total. Corresponds to Haskell's replicate :: Int -> a -> [a].

**[HYDRA-CORE-LIB-LISTS-REPLICATE-REPEATS-VALUE-N-TIMES] (requirement) **replicate n x returns a list containing n copies of x, including the empty list when n is zero, for numeric and string element types.

## hydra.core.lib.lists.reverse

Reverse a list.

`∀x.list<x> → list<x>`

*Since: 0.15.*

reverse(xs) returns the elements of xs in reverse order.

Total. Corresponds to Haskell's reverse :: [a] -> [a].

**[HYDRA-CORE-LIB-LISTS-REVERSE-REVERSES-ELEMENT-ORDER] (requirement) **reverse returns the list with its elements in the opposite order, for any element type, including lists of zero or one elements (which are unaffected).

## hydra.core.lib.lists.singleton

Construct a single-element list.

`∀x.x → list<x>`

*Since: 0.15.*

singleton(x) = [x].

The constructor for a single-element list; useful point-free (e.g. map singleton xs).

Total. Corresponds to Haskell's singleton :: a -> [a].

**[HYDRA-CORE-LIB-LISTS-SINGLETON-WRAPS-ONE-ELEMENT] (requirement) **singleton x returns a one-element list containing exactly x, for any value including negative numbers, zero, and strings.

## hydra.core.lib.lists.sort

Sort a list.

`∀x.(ordering x) ⇒ list<x> → list<x>`

*Since: 0.15.*

sort(xs) returns xs sorted in ascending order under the element type's ordering. Sort is stable: equal elements preserve their original relative order.

Requires an 'ordering' constraint on the element type.

Total. Corresponds to Haskell's Data.List.sort :: Ord a => [a] -> [a].

**[HYDRA-CORE-LIB-LISTS-SORT-ORDERS-ASCENDING] (requirement) **sort returns the list's elements in ascending order under the type's natural ordering, preserving multiplicity (duplicates are kept), and leaves already-sorted or single-element lists unchanged.

## hydra.core.lib.lists.sortBy

Sort a list using a key-extraction function.

`∀x,y.(ordering y) ⇒ (x → y) → list<x> → list<x>`

*Since: 0.17.6.*

sortBy(f, xs) returns xs sorted in ascending order by f(x) for each element x. Sort is stable: elements with equal keys preserve their original relative order.

Requires an 'ordering' constraint on the key type.

Total. Corresponds to Haskell's Data.List.sortOn :: Ord b => (a -> b) -> [a] -> [a].

Renamed from hydra.lib.lists.sortOn.

**[HYDRA-CORE-LIB-LISTS-SORT-BY-ORDERS-BY-KEY-FUNCTION] (requirement) **sortBy returns the list ordered ascending by the natural ordering of the given key function's output, not the elements' own ordering, so the result changes with the key function (e.g. sorting strings by length, or numbers by absolute value, differs from sorting by natural value).

**[HYDRA-CORE-LIB-LISTS-SORT-BY-BOUNDARY-INPUTS] (requirement) **sortBy on an empty or single-element list returns it unchanged, regardless of the key function.

## hydra.core.lib.lists.span

Split a list at the first element where the predicate fails.

`∀x.(x → boolean) → list<x> → (list<x>, list<x>)`

*Since: 0.15.*

span(p, xs) returns a pair (ys, zs) where ys is the longest prefix of xs whose elements all satisfy p, and zs is the remainder of xs starting at the first element that fails p (or zs is empty if all elements satisfy p).

Total. Corresponds to Haskell's span :: (a -> Bool) -> [a] -> ([a], [a]).

**[HYDRA-CORE-LIB-LISTS-SPAN-SPLITS-AT-FIRST-NON-MATCH] (requirement) **span returns a pair of (longest prefix for which the predicate holds, the remaining suffix), degrading correctly when the predicate matches every element (whole list, empty), no elements (empty, whole list), or the input is empty (empty, empty).

## hydra.core.lib.lists.tail

Return all elements except the first, or Nothing if the list is empty.

`∀x.list<x> → optional<list<x>>`

*Since: 0.17.6.*

tail(xs) returns Just(ys) where ys is xs with its first element removed, or Nothing if xs is empty.

Total. Wraps Haskell's partial tail in maybe.

Renamed from hydra.lib.lists.maybeTail.

**[HYDRA-CORE-LIB-LISTS-TAIL-RETURNS-ALL-BUT-FIRST] (requirement) **tail returns all elements of a non-empty list except the first, wrapped, including the empty list when the input has exactly one element.

**[HYDRA-CORE-LIB-LISTS-TAIL-NONE-WHEN-EMPTY] (requirement) **tail of an empty list is none.

## hydra.core.lib.lists.take

Take the first n elements of a list.

`∀x.int32 → list<x> → list<x>`

*Since: 0.15.*

take(n, xs) returns the prefix of xs of length min(n, length(xs)); if n is non-positive the result is the empty list.

Total. Corresponds to Haskell's take :: Int -> [a] -> [a].

**[HYDRA-CORE-LIB-LISTS-TAKE-TAKES-FROM-FRONT] (requirement) **take n returns the first n elements of the list, or the whole list if n is greater than or equal to its length.

**[HYDRA-CORE-LIB-LISTS-TAKE-NON-POSITIVE-IS-EMPTY] (requirement) **take with n equal to zero or negative returns the empty list.

## hydra.core.lib.lists.takeWhile

Take elements from the beginning of a list while a predicate holds.

`∀x.(x → boolean) → list<x> → list<x>`

*Since: 0.17.6.*

takeWhile(p, xs) returns the longest prefix of xs whose elements all satisfy p. If p fails for the first element the result is the empty list; if p holds for every element the result is xs unchanged.

dropWhile returns the complementary suffix, and span(p, xs) returns the pair of takeWhile(p, xs) and dropWhile(p, xs).

Total. Corresponds to Haskell's takeWhile :: (a -> Bool) -> [a] -> [a].

## hydra.core.lib.lists.transpose

Transpose a list of lists.

`∀x.list<list<x>> → list<list<x>>`

*Since: 0.15.*

transpose(xss) returns a list of lists where the i-th inner list contains the i-th element of every inner list of xss that has at least i+1 elements. Inner lists of differing lengths produce a ragged result rather than an error.

Total. Corresponds to Haskell's Data.List.transpose :: [[a]] -> [[a]].

**[HYDRA-CORE-LIB-LISTS-TRANSPOSE-SWAPS-ROWS-AND-COLUMNS] (requirement) **transpose converts a list of rows into a list of columns, where column i holds the i-th element of every row that has one; rows need not be the same length (a ragged matrix is handled by omitting absent elements, not by padding or erroring), and an empty list of rows yields the empty list.

## hydra.core.lib.lists.uncons

Decompose a list into its head and tail, or Nothing if empty.

`∀x.list<x> → optional<(x, list<x>)>`

*Since: 0.15.*

uncons(xs) returns Just(head, tail) where head is the first element of xs and tail is the remaining list, or Nothing if xs is empty.

Total. Corresponds to Haskell's Data.List.uncons :: [a] -> Maybe (a, [a]).

**[HYDRA-CORE-LIB-LISTS-UNCONS-SPLITS-HEAD-AND-TAIL] (requirement) **uncons on a non-empty list returns (wrapped) the pair of its first element and the remaining list, including when the remaining list is empty.

**[HYDRA-CORE-LIB-LISTS-UNCONS-NONE-WHEN-EMPTY] (requirement) **uncons of an empty list is none.

## hydra.core.lib.lists.zip

Zip two lists element-wise into pairs.

`∀x,y.list<x> → list<y> → list<(x, y)>`

*Since: 0.15.*

zip(xs, ys) returns the list of pairs (xs[i], ys[i]) for i from 0 to min(length(xs), length(ys))-1. The result has length equal to the shorter of the two inputs.

Total. Corresponds to Haskell's zip :: [a] -> [b] -> [(a, b)].

**[HYDRA-CORE-LIB-LISTS-ZIP-PAIRS-ELEMENTWISE] (requirement) **zip combines two lists into a list of pairs by position, truncating to the length of the shorter list when the lengths differ.

**[HYDRA-CORE-LIB-LISTS-ZIP-EMPTY-WHEN-EITHER-EMPTY] (requirement) **zip is the empty list whenever either input list is empty.

## hydra.core.lib.lists.zipWith

Zip two lists with a combining function.

`∀x,y,z.(x → y → z) → list<x> → list<y> → list<z>`

*Since: 0.15.*

zipWith(f, xs, ys) returns the list of f(xs[i], ys[i]) for i from 0 to min(length(xs), length(ys))-1. The result has length equal to the shorter of the two inputs.

Total. Corresponds to Haskell's zipWith :: (a -> b -> c) -> [a] -> [b] -> [c].

**[HYDRA-CORE-LIB-LISTS-ZIP-WITH-COMBINES-ELEMENTWISE-BY-FUNCTION] (requirement) **zipWith combines two lists position-by-position using the given binary function, truncating to the length of the shorter list when the lengths differ, for numeric and string element/result types.

**[HYDRA-CORE-LIB-LISTS-ZIP-WITH-EMPTY-WHEN-EITHER-EMPTY] (requirement) **zipWith is the empty list whenever either input list is empty, regardless of the function.
