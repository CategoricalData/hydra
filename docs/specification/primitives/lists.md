<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Lists.hs (generator not yet
     built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.lists

List construction, transformation, and folding.
Lists form a monad: `pure` is its unit and `bind` its bind, with `map`, `apply`, and `compose`
completing the functor, applicative, and Kleisli structure.
The traversal family `mapList`, `mapOptional`, and `mapSet` traverses a container in the list
monad, and `foldList` is the monadic fold.
The bare `foldl` and `foldr` are the plain, non-monadic folds.

#### apply — **Draft**

`∀t1,t2. list<t1 → t2> → list<t1> → list<t2>`

Usage: `apply fs xs`

Apply a list of functions to a list of values (applicative style).
Returns the list of `f x` for each `f` in `fs` and each `x` in `xs`, ordered with `fs` as the
outer loop and `xs` as the inner loop.
The result has length equal to the product of the lengths of `fs` and `xs`.
`apply fs xs` is equivalent to `bind fs (λf → map f xs)`; this is the applicative structure
induced by the list monad.

Since: 0.15

#### at — **Draft**

`∀t. int32 → list<t> → optional<t>`

Usage: `at i xs`

Return the element of a list at a given index, if any.
Indexing is zero-based: returns `given x` where `x` is the element of `xs` at position `i`
when `0 <= i < length xs`, and `none` otherwise.
Out-of-range indices, including negative indices, are expressed in the codomain as `none`
rather than as a failure.

Since: 0.18 (renamed from `hydra.lib.lists.maybeAt`)

#### bind — **Draft**

`∀t1,t2. list<t1> → (t1 → list<t2>) → list<t2>`

Usage: `bind xs f`

Apply a function that returns lists to each element of a list, and flatten the results.
Applies `f` to each element of `xs` and concatenates the resulting lists in order
(a "flat map").
`bind xs f` is equivalent to `concat (map f xs)`.
This is the monadic bind for lists; `pure` is the corresponding unit.

Since: 0.15

#### compose — **Draft**

`∀t1,t2,t3. (t1 → list<t2>) → (t2 → list<t3>) → t1 → list<t3>`

Usage: `compose f g x`

Compose two functions that return lists (Kleisli composition in the list monad).
`compose f g x` is `bind (f x) g`; this defining equation is the specification.
The results of applying `g` to each element of `f x` are concatenated in order.

Since: 0.18

#### concat — **Draft**

`∀t. list<list<t>> → list<t>`

Usage: `concat xss`

Concatenate a list of lists.
Returns the list obtained by appending all the lists in `xss` in order.
For the empty outer list the result is the empty list.

Since: 0.15

#### concat2 — **Draft**

`∀t. list<t> → list<t> → list<t>`

Usage: `concat2 xs ys`

Concatenate two lists; the binary form of `concat`.
Returns `xs` with `ys` appended.

Since: 0.15

#### cons — **Draft**

`∀t. t → list<t> → list<t>`

Usage: `cons x xs`

Prepend a value to a list.
Returns a list whose head is `x` and whose tail is `xs`.
`uncons` is the corresponding decomposition.

Since: 0.15

#### distinct — **Draft**

`∀t. (equality t) ⇒ list<t> → list<t>`

Usage: `distinct xs`

Remove duplicate elements from a list.
Returns the list of distinct elements of `xs`, in the order of their first occurrence.
Requires an `equality` constraint on the element type.
Quadratic in the length of `xs` in the worst case.

Since: 0.18 (renamed from `hydra.lib.lists.nub`)

#### drop — **Draft**

`∀t. int32 → list<t> → list<t>`

Usage: `drop n xs`

Drop the first `n` elements from a list.
Returns the suffix of `xs` after removing the first `n` elements.
If `n` is greater than or equal to `length xs` the result is the empty list; if `n` is
non-positive the result is `xs` unchanged.
`take` returns the complementary prefix.

Since: 0.15

#### dropWhile — **Draft**

`∀t. (t → boolean) → list<t> → list<t>`

Usage: `dropWhile p xs`

Drop elements from the beginning of a list while a predicate holds.
Returns the suffix of `xs` starting at the first element for which `p` returns `false`.
If `p` is `true` for every element, the result is the empty list.
`dropWhile p xs` is `hydra.lib.pairs.second (span p xs)`; this defining equation is the
specification, and the default implementation.
`takeWhile` returns the complementary prefix.

Since: 0.15

#### filter — **Draft**

`∀t. (t → boolean) → list<t> → list<t>`

Usage: `filter p xs`

Filter a list by a predicate.
Returns the list of elements `x` in `xs` for which `p x` is `true`, in original order.
`partition` returns both the satisfying and the non-satisfying elements at once.

Since: 0.15

#### find — **Draft**

`∀t. (t → boolean) → list<t> → optional<t>`

Usage: `find p xs`

Find the first element of a list matching a predicate.
Returns `given x` where `x` is the first element of `xs` for which `p x` is `true`, or `none`
if no such element exists.

Since: 0.15

#### foldl — **Draft**

`∀t1,t2. (t1 → t2 → t1) → t1 → list<t2> → t1`

Usage: `foldl f acc0 xs`

Left-fold a list with an accumulator.
Reduces `xs` left-associatively:
`foldl f acc0 [x1, x2, ..., xn]` is `f (... (f (f acc0 x1) x2) ...) xn`.
For the empty list the result is `acc0`.
This is the strict left fold: the accumulator is evaluated at each step, so no deferred
computation accumulates over the length of the list.
`foldList` is the monadic counterpart in the list monad.

Since: 0.15

#### foldList — **Draft**

`∀t1,t2. (t1 → t2 → list<t1>) → t1 → list<t2> → list<t1>`

Usage: `foldList f acc0 xs`

Left-fold a list in the list monad (a nondeterministic fold).
Folds `xs` from the left, branching the accumulator over every result of the step function at
each element: after each element the set of accumulators is replaced by all results of applying
`f` to each current accumulator and that element.
`foldList f acc0 xs` is `foldl (λmacc el → bind macc (λacc → f acc el)) (pure acc0) xs`;
this defining equation is the specification.
For the empty list the result is `pure acc0`.
For example, if `f acc x` returns the two-element list containing `acc` and the sum of `acc`
and `x`, then `foldList f 0 xs` computes the sums of all subsets of `xs`.

Since: 0.18

#### foldr — **Draft**

`∀t1,t2. (t1 → t2 → t2) → t2 → list<t1> → t2`

Usage: `foldr f acc0 xs`

Right-fold a list with an accumulator.
Reduces `xs` right-associatively:
`foldr f acc0 [x1, ..., xn]` is `f x1 (f x2 (... (f xn acc0)))`.
For the empty list the result is `acc0`.

Since: 0.15

#### group — **Draft**

`∀t. (equality t) ⇒ list<t> → list<list<t>>`

Usage: `group xs`

Group consecutive equal elements of a list.
Returns a list of lists obtained by grouping consecutive equal elements of `xs` together.
Each inner list is non-empty and contains equal elements, and the concatenation of the result
equals `xs`.
Equality is determined by the element type's `equality` constraint.

Since: 0.15

#### head — **Draft**

`∀t. list<t> → optional<t>`

Usage: `head xs`

Return the first element of a list, if any.
Returns `given x` where `x` is the first element of `xs`, or `none` if `xs` is empty.
Emptiness is expressed in the codomain as `none` rather than as a failure.
`uncons` returns the head and tail together.

Since: 0.18 (renamed from `hydra.lib.lists.maybeHead`)

#### init — **Draft**

`∀t. list<t> → optional<list<t>>`

Usage: `init xs`

Return all elements of a list except the last, if any.
Returns `given ys` where `ys` is `xs` with its last element removed, or `none` if `xs` is
empty.
Emptiness is expressed in the codomain as `none` rather than as a failure.

Since: 0.18 (renamed from `hydra.lib.lists.maybeInit`)

#### intersperse — **Draft**

`∀t. t → list<t> → list<t>`

Usage: `intersperse sep xs`

Intersperse a value between consecutive elements of a list.
Returns `xs` with `sep` inserted between each pair of adjacent elements.
For lists of length 0 or 1 the input is returned unchanged.
`join` is the analogous operation on lists of lists, with flattening.

Since: 0.15

#### join — **Draft**

`∀t. list<t> → list<list<t>> → list<t>`

Usage: `join sep xss`

Concatenate a list of lists with a separator list inserted between each.
Returns the concatenation of the lists in `xss`, in order, with `sep` inserted between
consecutive lists.
`join sep xss` is `concat (intersperse sep xss)`; this defining equation is the specification.

Since: 0.18 (renamed from `hydra.lib.lists.intercalate`)

#### last — **Draft**

`∀t. list<t> → optional<t>`

Usage: `last xs`

Return the last element of a list, if any.
Returns `given x` where `x` is the last element of `xs`, or `none` if `xs` is empty.
Emptiness is expressed in the codomain as `none` rather than as a failure.

Since: 0.18 (renamed from `hydra.lib.lists.maybeLast`)

#### length — **Draft**

`∀t. list<t> → int32`

Usage: `length xs`

Return the length of a list.
Returns the number of elements in `xs` as an `int32`; returns 0 for the empty list.
Lists longer than 2^31 - 1 elements are outside the specified domain (see
[Sizes and indices](index.md#sizes-and-indices)).

Since: 0.15

#### map — **Draft**

`∀t1,t2. (t1 → t2) → list<t1> → list<t2>`

Usage: `map f xs`

Map a function over a list.
Returns the list of `f x` for each `x` in `xs`, in original order.
The result has the same length as `xs`.
This is the functor map for lists.

Since: 0.15

#### mapGivens — **Draft**

`∀t1,t2. (t1 → optional<t2>) → list<t1> → list<t2>`

Usage: `mapGivens f xs`

Map an optional-returning function over a list, keeping only the present results.
Applies `f` to each element of `xs` in order; each `given y` result contributes `y` to the
output, and each `none` result is discarded.
The order of the kept results follows the order of `xs`.
`mapGivens f xs` is `hydra.lib.optionals.givens (map f xs)`; this defining equation is the
specification.

Since: 0.18 (moved from `hydra.lib.optionals.mapOptional`)

#### mapList — **Draft**

`∀t1,t2. (t1 → list<t2>) → list<t1> → list<list<t2>>`

Usage: `mapList f xs`

Traverse a list in the list monad.
Returns all combinations obtainable by choosing one element from `f x` for each `x` in `xs`:
each result list has the same length as `xs`, with its element at each position drawn from the
corresponding `f x`.
Results appear in the lexicographic order of the choices, with the choice for the first element
varying slowest.
The number of results is the product of the lengths of the lists `f x`; if `f x` is empty for
any element, the result is the empty list.
`mapList f []` is `pure []`, the single-element list containing the empty list.
This is the list-monad instance of the traversal family (`mapList`, `mapOptional`, `mapSet`).

Since: 0.18

#### mapOptional — **Draft**

`∀t1,t2. (t1 → list<t2>) → optional<t1> → list<optional<t2>>`

Usage: `mapOptional f m`

Traverse an optional value in the list monad.
For `none` the result is the single-element list containing `none`.
For `given x` the result contains `given y` for each element `y` of `f x`, in order.
This is the list-monad instance of the traversal family, applied to the optional container.

Since: 0.18

#### mapSet — **Draft**

`∀t1,t2. (ordering t1, ordering t2) ⇒ (t1 → list<t2>) → set<t1> → list<set<t2>>`

Usage: `mapSet f s`

Traverse a set in the list monad.
Returns all sets obtainable by choosing one element from `f x` for each element `x` of `s`.
Elements of `s` are traversed in ascending order, and results appear in the lexicographic order
of the choices, with the choice for the least element varying slowest.
A result set may have fewer elements than `s` when distinct choices coincide.
If `f x` is empty for any element, the result is the empty list; for the empty set the result
is the single-element list containing the empty set.
The `ordering` constraints follow from the set type contract (see
[index.md](index.md#type-classes-and-constraints)).

Since: 0.18

#### member — **Draft**

`∀t. (equality t) ⇒ t → list<t> → boolean`

Usage: `member x xs`

Test whether a value is an element of a list.
Returns `true` if and only if some element of `xs` is equal to `x`.
Requires an `equality` constraint on the element type.

Since: 0.18 (renamed from `hydra.lib.lists.elem`)

#### null — **Draft**

`∀t. list<t> → boolean`

Usage: `null xs`

Test whether a list is empty.
Returns `true` if and only if `xs` is the empty list.

Since: 0.15

#### partition — **Draft**

`∀t. (t → boolean) → list<t> → pair<list<t>, list<t>>`

Usage: `partition p xs`

Partition a list into the elements that satisfy a predicate and those that do not.
Returns a pair whose first component is the list of elements of `xs` for which `p` is `true`
and whose second component is the list of elements for which `p` is `false`, each preserving
original order.
The first component equals `filter p xs`.

Since: 0.15

#### pure — **Draft**

`∀t. t → list<t>`

Usage: `pure x`

Wrap a value in a single-element list.
Returns the list whose only element is `x`.
This is the unit of the list monad, and the standard way to construct a single-element list
(the former `singleton` is deprecated in its favor).

Since: 0.15

#### replicate — **Draft**

`∀t. int32 → t → list<t>`

Usage: `replicate n x`

Build a list of `n` copies of a value.
Returns a list of length `n` in which every element is `x`; for non-positive `n` the result is
the empty list.

Since: 0.15

#### reverse — **Draft**

`∀t. list<t> → list<t>`

Usage: `reverse xs`

Reverse a list.
Returns the elements of `xs` in reverse order.

Since: 0.15

#### sort — **Draft**

`∀t. (ordering t) ⇒ list<t> → list<t>`

Usage: `sort xs`

Sort a list.
Returns `xs` sorted in ascending order under the element type's total order, as defined in
[ordering-and-equality.md](../ordering-and-equality.md).
The sort is stable: equal elements preserve their original relative order.
Requires an `ordering` constraint on the element type.
`sortBy` sorts by a projected key instead of by the elements themselves.

Since: 0.15

#### sortBy — **Draft**

`∀t1,t2. (ordering t2) ⇒ (t1 → t2) → list<t1> → list<t1>`

Usage: `sortBy f xs`

Sort a list by a key-extraction function (a projection sort).
Returns `xs` sorted in ascending order of the key `f x` computed for each element `x`, under
the key type's total order.
The sort is stable: elements with equal keys preserve their original relative order.
Requires an `ordering` constraint on the key type only; the element type is unconstrained.

Since: 0.18 (renamed from `hydra.lib.lists.sortOn`)

#### span — **Draft**

`∀t. (t → boolean) → list<t> → pair<list<t>, list<t>>`

Usage: `span p xs`

Split a list at the first element where a predicate fails.
Returns a pair whose first component is the longest prefix of `xs` whose elements all satisfy
`p`, and whose second component is the remainder of `xs` starting at the first element that
fails `p` (empty if every element satisfies `p`).
The first component equals `takeWhile p xs` and the second equals `dropWhile p xs`; the
concatenation of the two components equals `xs`.

Since: 0.15

#### tail — **Draft**

`∀t. list<t> → optional<list<t>>`

Usage: `tail xs`

Return all elements of a list except the first, if any.
Returns `given ys` where `ys` is `xs` with its first element removed, or `none` if `xs` is
empty.
Emptiness is expressed in the codomain as `none` rather than as a failure.
`uncons` returns the head and tail together.

Since: 0.18 (renamed from `hydra.lib.lists.maybeTail`)

#### take — **Draft**

`∀t. int32 → list<t> → list<t>`

Usage: `take n xs`

Take the first `n` elements of a list.
Returns the prefix of `xs` of length `min n (length xs)`; if `n` is non-positive the result is
the empty list.
`drop` returns the complementary suffix.

Since: 0.15

#### takeWhile — **Draft**

`∀t. (t → boolean) → list<t> → list<t>`

Usage: `takeWhile p xs`

Take elements from the beginning of a list while a predicate holds.
Returns the longest prefix of `xs` whose elements all satisfy `p`.
If `p` fails for the first element the result is the empty list; if `p` holds for every
element the result is `xs` unchanged.
`dropWhile` returns the complementary suffix, and `span p xs` returns the pair of
`takeWhile p xs` and `dropWhile p xs`.

Since: 0.18

#### transpose — **Draft**

`∀t. list<list<t>> → list<list<t>>`

Usage: `transpose xss`

Transpose a list of lists.
Returns a list of lists in which the i-th inner list contains the i-th element of every inner
list of `xss` that has at least i + 1 elements.
For example, `transpose [[1, 2, 3], [4, 5, 6]]` is `[[1, 4], [2, 5], [3, 6]]`.
Inner lists of differing lengths produce a ragged result rather than an error: shorter inner
lists simply stop contributing elements.

Since: 0.15

#### uncons — **Draft**

`∀t. list<t> → optional<pair<t, list<t>>>`

Usage: `uncons xs`

Decompose a list into its head and tail, if any.
Returns `given` of the pair of the first element of `xs` and the remaining list, or `none` if
`xs` is empty.
`cons` is the corresponding construction; `head` and `tail` return the components separately.

Since: 0.15

#### zip — **Draft**

`∀t1,t2. list<t1> → list<t2> → list<pair<t1, t2>>`

Usage: `zip xs ys`

Zip two lists element-wise into pairs.
Returns the list of pairs of corresponding elements of `xs` and `ys`, in order.
The result has length equal to the shorter of the two inputs; excess elements of the longer
list are discarded.

Since: 0.15

#### zipWith — **Draft**

`∀t1,t2,t3. (t1 → t2 → t3) → list<t1> → list<t2> → list<t3>`

Usage: `zipWith f xs ys`

Zip two lists with a combining function.
Returns the list of `f x y` for corresponding elements `x` of `xs` and `y` of `ys`, in order.
The result has length equal to the shorter of the two inputs; excess elements of the longer
list are discarded.

Since: 0.15

#### elem — **Deprecated**

`∀t. (equality t) ⇒ t → list<t> → boolean`

Deprecated since: 0.18. Use: `hydra.lib.lists.member`.

#### intercalate — **Deprecated**

`∀t. list<t> → list<list<t>> → list<t>`

Deprecated since: 0.18. Use: `hydra.lib.lists.join`.

#### maybeAt — **Deprecated**

`∀t. int32 → list<t> → optional<t>`

Deprecated since: 0.18. Use: `hydra.lib.lists.at`.

#### maybeHead — **Deprecated**

`∀t. list<t> → optional<t>`

Deprecated since: 0.18. Use: `hydra.lib.lists.head`.

#### maybeInit — **Deprecated**

`∀t. list<t> → optional<list<t>>`

Deprecated since: 0.18. Use: `hydra.lib.lists.init`.

#### maybeLast — **Deprecated**

`∀t. list<t> → optional<t>`

Deprecated since: 0.18. Use: `hydra.lib.lists.last`.

#### maybeTail — **Deprecated**

`∀t. list<t> → optional<list<t>>`

Deprecated since: 0.18. Use: `hydra.lib.lists.tail`.

#### nub — **Deprecated**

`∀t. (equality t) ⇒ list<t> → list<t>`

Deprecated since: 0.18. Use: `hydra.lib.lists.distinct`.

#### singleton — **Deprecated**

`∀t. t → list<t>`

Deprecated since: 0.18. Use: `hydra.lib.lists.pure`.
Lists standardize on the monadic unit `pure`; sets and maps retain `singleton`.

#### sortOn — **Deprecated**

`∀t1,t2. (ordering t2) ⇒ (t1 → t2) → list<t1> → list<t1>`

Deprecated since: 0.18. Use: `hydra.lib.lists.sortBy`.
