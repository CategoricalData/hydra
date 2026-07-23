-- | Primitive declarations for the hydra.lib.lists namespace.
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Lib.Lists where

import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms hiding (apply, compose, map)
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), concat, drop, dropWhile, elem, filter, foldl, foldr,
                               head, init, last, length, map, null, pure, replicate, reverse,
                               span, tail, take, takeWhile, zip, zipWith)


ns :: ModuleName
ns = ModuleName "hydra.lib.lists"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.lists module.")}
  where
    definitions = [apply, at, bind, compose, concat, concat2, cons, distinct, drop, dropWhile,
                   filter, find, foldList, foldl, foldr, group, head, init, intersperse,
                   join, last, length, map, mapList, mapOptional, mapSet, member, null, partition,
                   pure, replicate, reverse, singleton, sort, sortBy, span, tail, take, takeWhile,
                   transpose, uncons, zip, zipWith]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

defineWithDefault :: String -> String -> TermSignature -> [String] -> TypedTerm a -> PrimitiveDefinition
defineWithDefault = primitiveWithDefaultInModule module_

-- Shared type-variable shortcuts.
l :: Type -> Type
l = Types.list

tx, ty, tz :: Type
tx = Types.var "x"
ty = Types.var "y"
tz = Types.var "z"

apply :: PrimitiveDefinition
apply = define "apply" "Apply a list of functions to a list of values (applicative style)."
  (sigWithParams [("fs", "the list of functions to apply"), ("xs", "the list of values to apply them to")] $ TypeScheme [Name "x", Name "y"]
    (l (tx Types.~> ty) Types.~> l tx Types.~> l ty) Nothing)
  ["apply(fs, xs) returns the list of all f x for f in fs and x in xs, in order: for each f in fs (outer\
  \ loop), for each x in xs (inner loop).",
   "Equivalent to the applicative list instance.",
   "Total. Corresponds to Haskell's (<*>) :: [a -> b] -> [a] -> [b]."]

at :: PrimitiveDefinition
at = define "at" "Return the element at the given index, or Nothing if out of bounds."
  (sigWithParams [("i", "the index to look up"), ("xs", "the list to index into")] $ TypeScheme [Name "x"]
    (Types.int32 Types.~> l tx Types.~> Types.optional tx) Nothing)
  ["at(i, xs) returns Just(xs[i]) if 0 <= i < length(xs), or Nothing otherwise.",
   "Total. Wraps the Haskell (!!) operator, which is partial, in maybe to make out-of-bounds total."]

bind :: PrimitiveDefinition
bind = defineWithDefault "bind" "Apply a function that returns lists to each element and flatten the results."
  (sigWithParams [("xs", "the list to bind over"), ("f", "the function returning a list for each element")] $ TypeScheme [Name "x", Name "y"]
    (l tx Types.~> (tx Types.~> l ty) Types.~> l ty) Nothing)
  ["bind(xs, f) applies f to each element of xs and concatenates the resulting lists in order.\
  \ Equivalent to concatMap.",
   "Total. Corresponds to Haskell's (>>=) :: [a] -> (a -> [b]) -> [b]."]
  ("xs" ~> "f" ~>
    Lists.foldr
      ("x" ~> "acc" ~> Lists.concat2 (var "f" @@ var "x") (var "acc"))
      (list ([] :: [TypedTerm b]))
      (var "xs"))

compose :: PrimitiveDefinition
compose = define "compose" "Compose two functions that return lists (Kleisli composition in the list monad)."
  (sigWithParams [("f", "the first list-returning function"), ("g", "the second list-returning function"), ("x", "the input value")] $ TypeScheme [Name "x", Name "y", Name "z"]
    ((tx Types.~> l ty) Types.~> (ty Types.~> l tz) Types.~> tx Types.~> l tz) Nothing)
  ["compose(f, g, x) is bind(f(x), g); this defining equation is the specification. The results of\
  \ applying g to each element of f(x) are concatenated in order.",
   "Total. Corresponds to Kleisli composition (>=>) in the list monad."]

concat :: PrimitiveDefinition
concat = define "concat" "Concatenate a list of lists."
  (sigWithParams [("xss", "the list of lists to concatenate")] $ TypeScheme [Name "x"] (l (l tx) Types.~> l tx) Nothing)
  ["concat(xss) returns the list obtained by appending all the lists in xss in order.",
   "Total. Corresponds to Haskell's concat :: [[a]] -> [a]."]

concat2 :: PrimitiveDefinition
concat2 = define "concat2" "Concatenate two lists."
  (sigWithParams [("xs", "the first list"), ("ys", "the list to append")] $ TypeScheme [Name "x"] (l tx Types.~> l tx Types.~> l tx) Nothing)
  ["concat2(xs, ys) returns xs with ys appended.",
   "Total. Corresponds to Haskell's (++) :: [a] -> [a] -> [a]."]

cons :: PrimitiveDefinition
cons = define "cons" "Prepend a value to a list."
  (sigWithParams [("x", "the value to prepend"), ("xs", "the list to prepend to")] $ TypeScheme [Name "x"] (tx Types.~> l tx Types.~> l tx) Nothing)
  ["cons(x, xs) returns a list whose head is x and whose tail is xs.",
   "Total. Corresponds to Haskell's (:) :: a -> [a] -> [a]."]

distinct :: PrimitiveDefinition
distinct = define "distinct" "Remove duplicate elements from a list."
  (sigWithParams [("xs", "the list to deduplicate")] $ Types.polyConstrained [("x", [Name "equality"])] (l tx Types.~> l tx))
  ["distinct(xs) returns the list of distinct elements of xs, in the order of their first occurrence.",
   "Requires an 'equality' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.List.nub :: Eq a => [a] -> [a]."]

drop :: PrimitiveDefinition
drop = define "drop" "Drop the first n elements from a list."
  (sigWithParams [("n", "the number of elements to drop"), ("xs", "the list to drop from")] $ TypeScheme [Name "x"] (Types.int32 Types.~> l tx Types.~> l tx) Nothing)
  ["drop(n, xs) returns the suffix of xs after dropping the first n elements; if n is greater than or\
  \ equal to length(xs) the result is the empty list; if n is non-positive the result is xs\
  \ unchanged.",
   "Total. Corresponds to Haskell's drop :: Int -> [a] -> [a]."]

dropWhile :: PrimitiveDefinition
dropWhile = defineWithDefault "dropWhile" "Drop elements from the beginning of a list while the predicate is true."
  (sigWithParams [("p", "the predicate to test each element"), ("xs", "the list to drop from")] $ TypeScheme [Name "x"]
    ((tx Types.~> Types.boolean) Types.~> l tx Types.~> l tx) Nothing)
  ["dropWhile(p, xs) returns the suffix of xs starting at the first element for which p returns false.\
  \ If p is true for every element, the result is the empty list.",
   "Total. Corresponds to Haskell's dropWhile :: (a -> Bool) -> [a] -> [a]."]
  ("p" ~> "xs" ~> Pairs.second (Lists.span (var "p") (var "xs")))

filter :: PrimitiveDefinition
filter = defineWithDefault "filter" "Filter a list by a predicate."
  (sigWithParams [("p", "the predicate to test each element"), ("xs", "the list to filter")] $ TypeScheme [Name "x"]
    ((tx Types.~> Types.boolean) Types.~> l tx Types.~> l tx) Nothing)
  ["filter(p, xs) returns the list of elements x in xs for which p(x) is true, in original order.",
   "Total. Corresponds to Haskell's filter :: (a -> Bool) -> [a] -> [a]."]
  ("p" ~> "xs" ~>
    Lists.foldr
      ("x" ~> "acc" ~> Logic.ifElse (var "p" @@ var "x")
        (Lists.cons (var "x") (var "acc"))
        (var "acc"))
      (list ([] :: [TypedTerm a]))
      (var "xs"))

find :: PrimitiveDefinition
find = defineWithDefault "find" "Find the first element matching a predicate."
  (sigWithParams [("p", "the predicate to test each element"), ("xs", "the list to search")] $ TypeScheme [Name "x"]
    ((tx Types.~> Types.boolean) Types.~> l tx Types.~> Types.optional tx) Nothing)
  ["find(p, xs) returns Just(x) where x is the first element of xs for which p(x) is true, or Nothing\
  \ if no such element exists.",
   "Total. Corresponds to Haskell's find :: (a -> Bool) -> [a] -> Maybe a."]
  ("p" ~> "xs" ~>
    Lists.foldr
      ("x" ~> "acc" ~> Logic.ifElse (var "p" @@ var "x")
        (just (var "x"))
        (var "acc"))
      (nothing :: TypedTerm (Maybe a))
      (var "xs"))

foldList :: PrimitiveDefinition
foldList = define "foldList" "Left-fold a list in the list monad (a nondeterministic fold)."
  (sigWithParams [("f", "the branching step function"), ("acc0", "the initial accumulator"), ("xs", "the list to fold")] $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> ty Types.~> l tx) Types.~> tx Types.~> l ty Types.~> l tx) Nothing)
  ["foldList(f, acc0, xs) folds xs from the left, branching the accumulator over every result of the\
  \ step function at each element: after each element the set of accumulators is replaced by all\
  \ results of applying f to each current accumulator and that element.",
   "foldList(f, acc0, xs) is foldl(\\macc el -> bind(macc, \\acc -> f(acc, el)), pure(acc0), xs); this\
  \ defining equation is the specification. For the empty list the result is pure(acc0).",
   "Total on finite inputs. The list-monad instance of the monadic left fold."]

foldl :: PrimitiveDefinition
foldl = define "foldl" "Left-fold a list with an accumulator."
  (sigWithParams [("f", "the step function combining accumulator and element"), ("acc0", "the initial accumulator"), ("xs", "the list to fold")] $ TypeScheme [Name "y", Name "x"]
    ((ty Types.~> tx Types.~> ty) Types.~> ty Types.~> l tx Types.~> ty) Nothing)
  ["foldl(f, acc0, xs) reduces xs left-associatively: foldl(f, acc0, [x1, x2, ..., xn]) =\
  \ f(f(f(acc0, x1), x2), ..., xn). For the empty list the result is acc0.",
   "Strict in the accumulator on hosts where laziness would otherwise leak space.",
   "Total on finite inputs. Corresponds to Haskell's foldl' :: (b -> a -> b) -> b -> [a] -> b."]

foldr :: PrimitiveDefinition
foldr = define "foldr" "Right-fold a list with an accumulator."
  (sigWithParams [("f", "the step function combining element and accumulator"), ("acc0", "the initial accumulator"), ("xs", "the list to fold")] $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> ty Types.~> ty) Types.~> ty Types.~> l tx Types.~> ty) Nothing)
  ["foldr(f, acc0, xs) reduces xs right-associatively: foldr(f, acc0, [x1, ..., xn]) =\
  \ f(x1, f(x2, ..., f(xn, acc0))). For the empty list the result is acc0.",
   "Total on finite inputs. Corresponds to Haskell's foldr :: (a -> b -> b) -> b -> [a] -> b."]

group :: PrimitiveDefinition
group = define "group" "Group consecutive equal elements."
  (sigWithParams [("xs", "the list to group")] $ Types.polyConstrained [("x", [Name "equality"])]
    (l tx Types.~> l (l tx)))
  ["group(xs) returns a list of lists obtained by grouping consecutive equal elements of xs together.\
  \ Each inner list is non-empty and contains equal elements; the concatenation of the result equals\
  \ xs.",
   "Equality is determined by the element type's 'equality' constraint.",
   "Total. Corresponds to Haskell's Data.List.group :: Eq a => [a] -> [[a]]."]

head :: PrimitiveDefinition
head = define "head" "Return the first element, or Nothing if the list is empty."
  (sigWithParams [("xs", "the list to take the head of")] $ TypeScheme [Name "x"] (l tx Types.~> Types.optional tx) Nothing)
  ["head(xs) returns Just(x) where x is the first element of xs, or Nothing if xs is empty.",
   "Total. Wraps Haskell's partial head in maybe."]

init :: PrimitiveDefinition
init = define "init" "Return all elements except the last, or Nothing if the list is empty."
  (sigWithParams [("xs", "the list to take the initial segment of")] $ TypeScheme [Name "x"] (l tx Types.~> Types.optional (l tx)) Nothing)
  ["init(xs) returns Just(ys) where ys is xs with its last element removed, or Nothing if xs is empty.",
   "Total. Wraps Haskell's partial init in maybe."]

intersperse :: PrimitiveDefinition
intersperse = define "intersperse" "Intersperse a value between consecutive elements of a list."
  (sigWithParams [("sep", "the separator value to insert"), ("xs", "the list to intersperse into")] $ TypeScheme [Name "x"]
    (tx Types.~> l tx Types.~> l tx) Nothing)
  ["intersperse(sep, xs) returns xs with sep inserted between each pair of adjacent elements; for\
  \ lists of length 0 or 1 the input is returned unchanged.",
   "Total. Corresponds to Haskell's intersperse :: a -> [a] -> [a]."]

join :: PrimitiveDefinition
join = define "join" "Intercalate a list of lists with a separator list between each."
  (sigWithParams [("sep", "the separator list to insert"), ("xss", "the list of lists to join")] $ TypeScheme [Name "x"]
    (l tx Types.~> l (l tx) Types.~> l tx) Nothing)
  ["join(sep, xss) returns the concatenation of xss with sep inserted between consecutive lists.\
  \ Equivalent to concat(intersperse(sep, xss)).",
   "Total. Corresponds to Haskell's intercalate :: [a] -> [[a]] -> [a]."]

last :: PrimitiveDefinition
last = define "last" "Return the last element, or Nothing if the list is empty."
  (sigWithParams [("xs", "the list to take the last element of")] $ TypeScheme [Name "x"] (l tx Types.~> Types.optional tx) Nothing)
  ["last(xs) returns Just(x) where x is the last element of xs, or Nothing if xs is empty.",
   "Total. Wraps Haskell's partial last in maybe."]

length :: PrimitiveDefinition
length = define "length" "Return the length of a list."
  (sigWithParams [("xs", "the list to measure")] $ TypeScheme [Name "x"] (l tx Types.~> Types.int32) Nothing)
  ["length(xs) returns the number of elements in xs as an int32. Returns 0 for the empty list.",
   "Total on finite inputs; the int32 result overflows for lists longer than 2^31-1 elements.",
   "Corresponds to Haskell's length :: [a] -> Int (with narrowing to Int32)."]

map :: PrimitiveDefinition
map = define "map" "Map a function over a list."
  (sigWithParams [("f", "the function to apply to each element"), ("xs", "the list to map over")] $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> ty) Types.~> l tx Types.~> l ty) Nothing)
  ["map(f, xs) returns the list of f(x) for each x in xs, in original order.",
   "Total. Corresponds to Haskell's map :: (a -> b) -> [a] -> [b] / fmap on lists."]

mapList :: PrimitiveDefinition
mapList = define "mapList" "Traverse a list in the list monad."
  (sigWithParams [("f", "the list-returning function to apply to each element"), ("xs", "the list to traverse")] $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> l ty) Types.~> l tx Types.~> l (l ty)) Nothing)
  ["mapList(f, xs) returns all combinations obtainable by choosing one element from f(x) for each x in\
  \ xs: each result list has the same length as xs, with its element at each position drawn from the\
  \ corresponding f(x).",
   "Results appear in the lexicographic order of the choices, with the choice for the first element\
  \ varying slowest. The number of results is the product of the lengths of the lists f(x); if f(x) is\
  \ empty for any element, the result is the empty list; mapList(f, []) is pure([]).",
   "Total on finite inputs. The list-monad instance of the traversal family."]

mapOptional :: PrimitiveDefinition
mapOptional = define "mapOptional" "Traverse an optional value in the list monad."
  (sigWithParams [("f", "the list-returning function to apply"), ("m", "the optional value to traverse")] $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> l ty) Types.~> Types.optional tx Types.~> l (Types.optional ty)) Nothing)
  ["mapOptional(f, m) returns the single-element list containing none when m is none; for given(x) it\
  \ returns given(y) for each element y of f(x), in order.",
   "Total on finite inputs. The list-monad instance of the traversal family, applied to the optional\
  \ container."]

mapSet :: PrimitiveDefinition
mapSet = define "mapSet" "Traverse a set in the list monad."
  (sigWithParams [("f", "the list-returning function to apply to each element"), ("s", "the set to traverse")] $ Types.polyConstrained [("x", [Name "ordering"]), ("y", [Name "ordering"])]
    ((tx Types.~> l ty) Types.~> Types.set tx Types.~> l (Types.set ty)))
  ["mapSet(f, s) returns all sets obtainable by choosing one element from f(x) for each element x of s.\
  \ Elements of s are traversed in ascending order, and results appear in the lexicographic order of\
  \ the choices, with the choice for the least element varying slowest.",
   "A result set may have fewer elements than s when distinct choices coincide. If f(x) is empty for\
  \ any element, the result is the empty list; for the empty set the result is the single-element list\
  \ containing the empty set.",
   "Requires 'ordering' constraints on both element types (the set type contract)."]

member :: PrimitiveDefinition
member = define "member" "Test whether an element is in a list."
  (sigWithParams [("x", "the value to look for"), ("xs", "the list to search")] $ Types.polyConstrained [("x", [Name "equality"])]
    (tx Types.~> l tx Types.~> Types.boolean))
  ["member(x, xs) returns true iff some element of xs is equal to x.",
   "Requires an 'equality' constraint on the element type.",
   "Total. Corresponds to Haskell's elem :: Eq a => a -> [a] -> Bool."]

null :: PrimitiveDefinition
null = define "null" "Test whether a list is empty."
  (sigWithParams [("xs", "the list to test")] $ TypeScheme [Name "x"] (l tx Types.~> Types.boolean) Nothing)
  ["null(xs) returns true iff xs is the empty list.",
   "Total. Corresponds to Haskell's null :: [a] -> Bool."]

partition :: PrimitiveDefinition
partition = defineWithDefault "partition" "Partition a list into elements that satisfy a predicate and those that do not."
  (sigWithParams [("p", "the predicate to test each element"), ("xs", "the list to partition")] $ TypeScheme [Name "x"]
    ((tx Types.~> Types.boolean) Types.~> l tx Types.~> Types.pair (l tx) (l tx)) Nothing)
  ["partition(p, xs) returns a pair (yes, no) where yes is the list of elements of xs for which p is\
  \ true and no is the list of elements for which p is false, each preserving original order.",
   "Total. Corresponds to Haskell's partition :: (a -> Bool) -> [a] -> ([a], [a])."]
  ("p" ~> "xs" ~>
    Lists.foldr
      ("x" ~> "acc" ~> Logic.ifElse (var "p" @@ var "x")
        (pair (Lists.cons (var "x") (Pairs.first $ var "acc")) (Pairs.second $ var "acc"))
        (pair (Pairs.first $ var "acc") (Lists.cons (var "x") (Pairs.second $ var "acc"))))
      (pair (list ([] :: [TypedTerm a])) (list ([] :: [TypedTerm a])))
      (var "xs"))

pure :: PrimitiveDefinition
pure = define "pure" "Wrap a value in a single-element list."
  (sigWithParams [("x", "the value to wrap")] $ TypeScheme [Name "x"] (tx Types.~> l tx) Nothing)
  ["pure(x) = [x]. The applicative pure for lists.",
   "Total. Corresponds to Haskell's pure :: a -> [a]."]

replicate :: PrimitiveDefinition
replicate = define "replicate" "Build a list of n copies of a value."
  (sigWithParams [("n", "the number of copies"), ("x", "the value to replicate")] $ TypeScheme [Name "x"] (Types.int32 Types.~> tx Types.~> l tx) Nothing)
  ["replicate(n, x) returns a list of n copies of x; for n <= 0 the result is the empty list.",
   "Total. Corresponds to Haskell's replicate :: Int -> a -> [a]."]

reverse :: PrimitiveDefinition
reverse = define "reverse" "Reverse a list."
  (sigWithParams [("xs", "the list to reverse")] $ TypeScheme [Name "x"] (l tx Types.~> l tx) Nothing)
  ["reverse(xs) returns the elements of xs in reverse order.",
   "Total. Corresponds to Haskell's reverse :: [a] -> [a]."]

singleton :: PrimitiveDefinition
singleton = define "singleton" "Construct a single-element list."
  (sigWithParams [("x", "the value to wrap in a single-element list")] $ TypeScheme [Name "x"] (tx Types.~> l tx) Nothing)
  ["singleton(x) = [x]. Identical to pure for lists.",
   "Total. Corresponds to Haskell's singleton :: a -> [a]."]

sort :: PrimitiveDefinition
sort = define "sort" "Sort a list."
  (sigWithParams [("xs", "the list to sort")] $ Types.polyConstrained [("x", [Name "ordering"])] (l tx Types.~> l tx))
  ["sort(xs) returns xs sorted in ascending order under the element type's ordering. Sort is stable:\
  \ equal elements preserve their original relative order.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.List.sort :: Ord a => [a] -> [a]."]

sortBy :: PrimitiveDefinition
sortBy = define "sortBy" "Sort a list using a key-extraction function."
  (sigWithParams [("f", "the key-extraction function"), ("xs", "the list to sort")] $ Types.polyConstrained [("x", []), ("y", [Name "ordering"])]
    ((tx Types.~> ty) Types.~> l tx Types.~> l tx))
  ["sortBy(f, xs) returns xs sorted in ascending order by f(x) for each element x. Sort is stable:\
  \ elements with equal keys preserve their original relative order.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.List.sortOn :: Ord b => (a -> b) -> [a] -> [a]."]

span :: PrimitiveDefinition
span = defineWithDefault "span" "Split a list at the first element where the predicate fails."
  (sigWithParams [("p", "the predicate to test each element"), ("xs", "the list to split")] $ TypeScheme [Name "x"]
    ((tx Types.~> Types.boolean) Types.~> l tx Types.~> Types.pair (l tx) (l tx)) Nothing)
  ["span(p, xs) returns a pair (ys, zs) where ys is the longest prefix of xs whose elements all\
  \ satisfy p, and zs is the remainder of xs starting at the first element that fails p (or zs is\
  \ empty if all elements satisfy p).",
   "Total. Corresponds to Haskell's span :: (a -> Bool) -> [a] -> ([a], [a])."]
  ("p" ~> "xs" ~>
    Lists.foldl
      ("acc" ~> "x" ~> Logic.ifElse
        (Logic.and (Lists.null (Pairs.second $ var "acc")) (var "p" @@ var "x"))
        (pair (Lists.concat2 (Pairs.first $ var "acc") (list [var "x"])) (Pairs.second $ var "acc"))
        (pair (Pairs.first $ var "acc") (Lists.concat2 (Pairs.second $ var "acc") (list [var "x"]))))
      (pair (list ([] :: [TypedTerm a])) (list ([] :: [TypedTerm a])))
      (var "xs"))

tail :: PrimitiveDefinition
tail = define "tail" "Return all elements except the first, or Nothing if the list is empty."
  (sigWithParams [("xs", "the list to take the tail of")] $ TypeScheme [Name "x"] (l tx Types.~> Types.optional (l tx)) Nothing)
  ["tail(xs) returns Just(ys) where ys is xs with its first element removed, or Nothing if xs is empty.",
   "Total. Wraps Haskell's partial tail in maybe."]

take :: PrimitiveDefinition
take = define "take" "Take the first n elements of a list."
  (sigWithParams [("n", "the number of elements to take"), ("xs", "the list to take from")] $ TypeScheme [Name "x"] (Types.int32 Types.~> l tx Types.~> l tx) Nothing)
  ["take(n, xs) returns the prefix of xs of length min(n, length(xs)); if n is non-positive the result\
  \ is the empty list.",
   "Total. Corresponds to Haskell's take :: Int -> [a] -> [a]."]

takeWhile :: PrimitiveDefinition
takeWhile = defineWithDefault "takeWhile" "Take elements from the beginning of a list while a predicate holds."
  (sigWithParams [("p", "the predicate to test each element"), ("xs", "the list to take from")] $ TypeScheme [Name "x"]
    ((tx Types.~> Types.boolean) Types.~> l tx Types.~> l tx) Nothing)
  ["takeWhile(p, xs) returns the longest prefix of xs whose elements all satisfy p. If p fails for the\
  \ first element the result is the empty list; if p holds for every element the result is xs\
  \ unchanged.",
   "dropWhile returns the complementary suffix, and span(p, xs) returns the pair of takeWhile(p, xs)\
  \ and dropWhile(p, xs).",
   "Total. Corresponds to Haskell's takeWhile :: (a -> Bool) -> [a] -> [a]."]
  ("p" ~> "xs" ~> Pairs.first (Lists.span (var "p") (var "xs")))

transpose :: PrimitiveDefinition
transpose = define "transpose" "Transpose a list of lists."
  (sigWithParams [("xss", "the list of lists to transpose")] $ TypeScheme [Name "x"] (l (l tx) Types.~> l (l tx)) Nothing)
  ["transpose(xss) returns a list of lists where the i-th inner list contains the i-th element of\
  \ every inner list of xss that has at least i+1 elements. Inner lists of differing lengths produce\
  \ a ragged result rather than an error.",
   "Total. Corresponds to Haskell's Data.List.transpose :: [[a]] -> [[a]]."]

uncons :: PrimitiveDefinition
uncons = define "uncons" "Decompose a list into its head and tail, or Nothing if empty."
  (sigWithParams [("xs", "the list to decompose")] $ TypeScheme [Name "x"]
    (l tx Types.~> Types.optional (Types.pair tx (l tx))) Nothing)
  ["uncons(xs) returns Just(head, tail) where head is the first element of xs and tail is the\
  \ remaining list, or Nothing if xs is empty.",
   "Total. Corresponds to Haskell's Data.List.uncons :: [a] -> Maybe (a, [a])."]

zip :: PrimitiveDefinition
zip = define "zip" "Zip two lists element-wise into pairs."
  (sigWithParams [("xs", "the first list"), ("ys", "the second list")] $ TypeScheme [Name "x", Name "y"]
    (l tx Types.~> l ty Types.~> l (Types.pair tx ty)) Nothing)
  ["zip(xs, ys) returns the list of pairs (xs[i], ys[i]) for i from 0 to min(length(xs),\
  \ length(ys))-1. The result has length equal to the shorter of the two inputs.",
   "Total. Corresponds to Haskell's zip :: [a] -> [b] -> [(a, b)]."]

zipWith :: PrimitiveDefinition
zipWith = define "zipWith" "Zip two lists with a combining function."
  (sigWithParams [("f", "the combining function"), ("xs", "the first list"), ("ys", "the second list")] $ TypeScheme [Name "x", Name "y", Name "z"]
    ((tx Types.~> ty Types.~> tz) Types.~> l tx Types.~> l ty Types.~> l tz) Nothing)
  ["zipWith(f, xs, ys) returns the list of f(xs[i], ys[i]) for i from 0 to min(length(xs),\
  \ length(ys))-1. The result has length equal to the shorter of the two inputs.",
   "Total. Corresponds to Haskell's zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]."]
