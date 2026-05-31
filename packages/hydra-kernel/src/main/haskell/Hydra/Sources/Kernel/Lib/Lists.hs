-- | Primitive declarations for the hydra.lib.lists namespace.

module Hydra.Sources.Kernel.Lib.Lists where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.lists"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.lists module."}
  where
    definitions = [
      primNoDef "apply"       "Apply a list of functions to a list of values (applicative style)." applySig (Just
        "apply(fs, xs) returns the list of all f x for f in fs and x in xs, in order: for each f in fs (outer\
        \ loop), for each x in xs (inner loop). Total. Equivalent to the applicative list instance.\
        \ Corresponds to Haskell's (<*>) :: [a -> b] -> [a] -> [b]."),
      primNoDef "bind"        "Apply a function that returns lists to each element and flatten the results." bindSig (Just
        "bind(xs, f) applies f to each element of xs and concatenates the resulting lists in order. Total.\
        \ Equivalent to concatMap. Corresponds to Haskell's (>>=) :: [a] -> (a -> [b]) -> [b]."),
      primNoDef "concat"      "Concatenate a list of lists." concatSig (Just
        "concat(xss) returns the list obtained by appending all the lists in xss in order. Total.\
        \ Corresponds to Haskell's concat :: [[a]] -> [a]."),
      primNoDef "concat2"     "Concatenate two lists." concat2Sig (Just
        "concat2(xs, ys) returns xs with ys appended. Total. Corresponds to Haskell's\
        \ (++) :: [a] -> [a] -> [a]."),
      primNoDef "cons"        "Prepend a value to a list." consSig (Just
        "cons(x, xs) returns a list whose head is x and whose tail is xs. Total. Corresponds to Haskell's\
        \ (:) :: a -> [a] -> [a]."),
      primNoDef "drop"        "Drop the first n elements from a list." dropSig (Just
        "drop(n, xs) returns the suffix of xs after dropping the first n elements; if n is greater than or\
        \ equal to length(xs) the result is the empty list; if n is non-positive the result is xs unchanged.\
        \ Total. Corresponds to Haskell's drop :: Int -> [a] -> [a]."),
      primNoDef "dropWhile"   "Drop elements from the beginning of a list while the predicate is true." dropWhileSig (Just
        "dropWhile(p, xs) returns the suffix of xs starting at the first element for which p returns false.\
        \ If p is true for every element, the result is the empty list. Total. Corresponds to Haskell's\
        \ dropWhile :: (a -> Bool) -> [a] -> [a]."),
      primNoDef "elem"        "Test whether an element is in a list." elemSig (Just
        "elem(x, xs) returns true iff some element of xs is equal to x. Requires an 'equality' constraint\
        \ on the element type. Total. Corresponds to Haskell's elem :: Eq a => a -> [a] -> Bool."),
      primNoDef "filter"      "Filter a list by a predicate." filterSig (Just
        "filter(p, xs) returns the list of elements x in xs for which p(x) is true, in original order.\
        \ Total. Corresponds to Haskell's filter :: (a -> Bool) -> [a] -> [a]."),
      primNoDef "find"        "Find the first element matching a predicate." findSig (Just
        "find(p, xs) returns Just(x) where x is the first element of xs for which p(x) is true, or Nothing\
        \ if no such element exists. Total. Corresponds to Haskell's\
        \ find :: (a -> Bool) -> [a] -> Maybe a."),
      primNoDef "foldl"       "Left-fold a list with an accumulator." foldlSig (Just
        "foldl(f, acc0, xs) reduces xs left-associatively: foldl(f, acc0, [x1, x2, ..., xn]) =\
        \ f(f(f(acc0, x1), x2), ..., xn). For the empty list the result is acc0. Strict in the\
        \ accumulator on hosts where laziness would otherwise leak space. Total on finite inputs.\
        \ Corresponds to Haskell's foldl' :: (b -> a -> b) -> b -> [a] -> b."),
      primNoDef "foldr"       "Right-fold a list with an accumulator." foldrSig (Just
        "foldr(f, acc0, xs) reduces xs right-associatively: foldr(f, acc0, [x1, ..., xn]) =\
        \ f(x1, f(x2, ..., f(xn, acc0))). For the empty list the result is acc0. Total on finite inputs.\
        \ Corresponds to Haskell's foldr :: (a -> b -> b) -> b -> [a] -> b."),
      primNoDef "group"       "Group consecutive equal elements." groupSig (Just
        "group(xs) returns a list of lists obtained by grouping consecutive equal elements of xs together.\
        \ Each inner list is non-empty and contains equal elements; the concatenation of the result equals\
        \ xs. Equality is determined by the element type's 'equality' constraint. Total. Corresponds to\
        \ Haskell's Data.List.group :: Eq a => [a] -> [[a]]."),
      primNoDef "intercalate" "Intercalate a list of lists with a separator list between each." intercalateSig (Just
        "intercalate(sep, xss) returns the concatenation of xss with sep inserted between consecutive\
        \ lists. Equivalent to concat(intersperse(sep, xss)). Total. Corresponds to Haskell's\
        \ intercalate :: [a] -> [[a]] -> [a]."),
      primNoDef "intersperse" "Intersperse a value between consecutive elements of a list." intersperseSig (Just
        "intersperse(sep, xs) returns xs with sep inserted between each pair of adjacent elements; for\
        \ lists of length 0 or 1 the input is returned unchanged. Total. Corresponds to Haskell's\
        \ intersperse :: a -> [a] -> [a]."),
      primNoDef "length"      "Return the length of a list." lengthSig (Just
        "length(xs) returns the number of elements in xs as an int32. Returns 0 for the empty list. Total\
        \ on finite inputs; the int32 result overflows for lists longer than 2^31-1 elements. Corresponds\
        \ to Haskell's length :: [a] -> Int (with narrowing to Int32)."),
      primNoDef "map"         "Map a function over a list." mapSig (Just
        "map(f, xs) returns the list of f(x) for each x in xs, in original order. Total. Corresponds to\
        \ Haskell's map :: (a -> b) -> [a] -> [b] / fmap on lists."),
      primNoDef "maybeAt"     "Return the element at the given index, or Nothing if out of bounds." maybeAtSig (Just
        "maybeAt(i, xs) returns Just(xs[i]) if 0 <= i < length(xs), or Nothing otherwise. Total. Wraps the\
        \ Haskell (!!) operator, which is partial, in maybe to make out-of-bounds total."),
      primNoDef "maybeHead"   "Return the first element, or Nothing if the list is empty." maybeHeadSig (Just
        "maybeHead(xs) returns Just(x) where x is the first element of xs, or Nothing if xs is empty.\
        \ Total. Wraps Haskell's partial head in maybe."),
      primNoDef "maybeInit"   "Return all elements except the last, or Nothing if the list is empty." maybeInitSig (Just
        "maybeInit(xs) returns Just(ys) where ys is xs with its last element removed, or Nothing if xs is\
        \ empty. Total. Wraps Haskell's partial init in maybe."),
      primNoDef "maybeLast"   "Return the last element, or Nothing if the list is empty." maybeLastSig (Just
        "maybeLast(xs) returns Just(x) where x is the last element of xs, or Nothing if xs is empty.\
        \ Total. Wraps Haskell's partial last in maybe."),
      primNoDef "maybeTail"   "Return all elements except the first, or Nothing if the list is empty." maybeTailSig (Just
        "maybeTail(xs) returns Just(ys) where ys is xs with its first element removed, or Nothing if xs is\
        \ empty. Total. Wraps Haskell's partial tail in maybe."),
      primNoDef "nub"         "Remove duplicate elements from a list." nubSig (Just
        "nub(xs) returns the list of distinct elements of xs, in the order of their first occurrence.\
        \ Requires an 'equality' constraint on the element type. Quadratic in the length of xs in the\
        \ worst case. Total. Corresponds to Haskell's Data.List.nub :: Eq a => [a] -> [a]."),
      primNoDef "null"        "Test whether a list is empty." nullSig (Just
        "null(xs) returns true iff xs is the empty list. Total. Corresponds to Haskell's\
        \ null :: [a] -> Bool."),
      primNoDef "partition"   "Partition a list into elements that satisfy a predicate and those that do not." partitionSig (Just
        "partition(p, xs) returns a pair (yes, no) where yes is the list of elements of xs for which p is\
        \ true and no is the list of elements for which p is false, each preserving original order. Total.\
        \ Corresponds to Haskell's partition :: (a -> Bool) -> [a] -> ([a], [a])."),
      primNoDef "pure"        "Wrap a value in a single-element list." pureSig (Just
        "pure(x) = [x]. The applicative pure for lists. Total. Corresponds to Haskell's\
        \ pure :: a -> [a]."),
      primNoDef "replicate"   "Build a list of n copies of a value." replicateSig (Just
        "replicate(n, x) returns a list of n copies of x; for n <= 0 the result is the empty list. Total.\
        \ Corresponds to Haskell's replicate :: Int -> a -> [a]."),
      primNoDef "reverse"     "Reverse a list." reverseSig (Just
        "reverse(xs) returns the elements of xs in reverse order. Total. Corresponds to Haskell's\
        \ reverse :: [a] -> [a]."),
      primNoDef "singleton"   "Construct a single-element list." singletonSig (Just
        "singleton(x) = [x]. Identical to pure for lists. Total. Corresponds to Haskell's\
        \ singleton :: a -> [a]."),
      primNoDef "sort"        "Sort a list." sortSig (Just
        "sort(xs) returns xs sorted in ascending order under the element type's ordering. Sort is stable:\
        \ equal elements preserve their original relative order. Requires an 'ordering' constraint on the\
        \ element type. Total. Corresponds to Haskell's Data.List.sort :: Ord a => [a] -> [a]."),
      primNoDef "sortOn"      "Sort a list using a key-extraction function." sortOnSig (Just
        "sortOn(f, xs) returns xs sorted in ascending order by f(x) for each element x. Sort is stable:\
        \ elements with equal keys preserve their original relative order. Requires an 'ordering'\
        \ constraint on the key type. Total. Corresponds to Haskell's\
        \ Data.List.sortOn :: Ord b => (a -> b) -> [a] -> [a]."),
      primNoDef "span"        "Split a list at the first element where the predicate fails." spanSig (Just
        "span(p, xs) returns a pair (ys, zs) where ys is the longest prefix of xs whose elements all\
        \ satisfy p, and zs is the remainder of xs starting at the first element that fails p (or zs is\
        \ empty if all elements satisfy p). Total. Corresponds to Haskell's\
        \ span :: (a -> Bool) -> [a] -> ([a], [a])."),
      primNoDef "take"        "Take the first n elements of a list." takeSig (Just
        "take(n, xs) returns the prefix of xs of length min(n, length(xs)); if n is non-positive the\
        \ result is the empty list. Total. Corresponds to Haskell's take :: Int -> [a] -> [a]."),
      primNoDef "transpose"   "Transpose a list of lists." transposeSig (Just
        "transpose(xss) returns a list of lists where the i-th inner list contains the i-th element of\
        \ every inner list of xss that has at least i+1 elements. Inner lists of differing lengths produce\
        \ a ragged result rather than an error. Total. Corresponds to Haskell's\
        \ Data.List.transpose :: [[a]] -> [[a]]."),
      primNoDef "uncons"      "Decompose a list into its head and tail, or Nothing if empty." unconsSig (Just
        "uncons(xs) returns Just(head, tail) where head is the first element of xs and tail is the\
        \ remaining list, or Nothing if xs is empty. Total. Corresponds to Haskell's\
        \ Data.List.uncons :: [a] -> Maybe (a, [a])."),
      primNoDef "zip"         "Zip two lists element-wise into pairs." zipSig (Just
        "zip(xs, ys) returns the list of pairs (xs[i], ys[i]) for i from 0 to min(length(xs),\
        \ length(ys))-1. The result has length equal to the shorter of the two inputs. Total. Corresponds\
        \ to Haskell's zip :: [a] -> [b] -> [(a, b)]."),
      primNoDef "zipWith"     "Zip two lists with a combining function." zipWithSig (Just
        "zipWith(f, xs, ys) returns the list of f(xs[i], ys[i]) for i from 0 to min(length(xs),\
        \ length(ys))-1. The result has length equal to the shorter of the two inputs. Total.\
        \ Corresponds to Haskell's zipWith :: (a -> b -> c) -> [a] -> [b] -> [c].")]

l :: Type -> Type
l = Types.list

primNoDef :: String -> String -> TermSignature -> Maybe String -> Definition
primNoDef localName description s comments =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName)) comments

-- Shared type variables.
tx, ty, tz :: Type
tx = Types.var "x"
ty = Types.var "y"
tz = Types.var "z"

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

-- Signatures.

applySig, bindSig, concatSig, concat2Sig, consSig, dropSig, dropWhileSig,
  elemSig, filterSig, findSig, foldlSig, foldrSig, groupSig, intercalateSig,
  intersperseSig, lengthSig, mapSig, maybeAtSig, maybeHeadSig, maybeInitSig,
  maybeLastSig, maybeTailSig, nubSig, nullSig, partitionSig, pureSig,
  replicateSig, reverseSig, singletonSig, sortSig, sortOnSig, spanSig,
  takeSig, transposeSig, unconsSig, zipSig, zipWithSig :: TermSignature

applySig = sig $ TypeScheme [Name "x", Name "y"]
  (l (tx Types.~> ty) Types.~> l tx Types.~> l ty) Nothing
bindSig = sig $ TypeScheme [Name "x", Name "y"]
  (l tx Types.~> (tx Types.~> l ty) Types.~> l ty) Nothing
concatSig = sig $ TypeScheme [Name "x"] (l (l tx) Types.~> l tx) Nothing
concat2Sig = sig $ TypeScheme [Name "x"] (l tx Types.~> l tx Types.~> l tx) Nothing
consSig = sig $ TypeScheme [Name "x"] (tx Types.~> l tx Types.~> l tx) Nothing
dropSig = sig $ TypeScheme [Name "x"] (Types.int32 Types.~> l tx Types.~> l tx) Nothing
dropWhileSig = sig $ TypeScheme [Name "x"]
  ((tx Types.~> Types.boolean) Types.~> l tx Types.~> l tx) Nothing
elemSig = sig $ Types.polyConstrained [("x", [Name "equality"])]
  (tx Types.~> l tx Types.~> Types.boolean)
filterSig = sig $ TypeScheme [Name "x"]
  ((tx Types.~> Types.boolean) Types.~> l tx Types.~> l tx) Nothing
findSig = sig $ TypeScheme [Name "x"]
  ((tx Types.~> Types.boolean) Types.~> l tx Types.~> Types.optional tx) Nothing
foldlSig = sig $ TypeScheme [Name "y", Name "x"]
  ((ty Types.~> tx Types.~> ty) Types.~> ty Types.~> l tx Types.~> ty) Nothing
foldrSig = sig $ TypeScheme [Name "x", Name "y"]
  ((tx Types.~> ty Types.~> ty) Types.~> ty Types.~> l tx Types.~> ty) Nothing
groupSig = sig $ Types.polyConstrained [("x", [Name "equality"])]
  (l tx Types.~> l (l tx))
intercalateSig = sig $ TypeScheme [Name "x"]
  (l tx Types.~> l (l tx) Types.~> l tx) Nothing
intersperseSig = sig $ TypeScheme [Name "x"]
  (tx Types.~> l tx Types.~> l tx) Nothing
lengthSig = sig $ TypeScheme [Name "x"] (l tx Types.~> Types.int32) Nothing
mapSig = sig $ TypeScheme [Name "x", Name "y"]
  ((tx Types.~> ty) Types.~> l tx Types.~> l ty) Nothing
maybeAtSig = sig $ TypeScheme [Name "x"]
  (Types.int32 Types.~> l tx Types.~> Types.optional tx) Nothing
maybeHeadSig = sig $ TypeScheme [Name "x"] (l tx Types.~> Types.optional tx) Nothing
maybeInitSig = sig $ TypeScheme [Name "x"] (l tx Types.~> Types.optional (l tx)) Nothing
maybeLastSig = sig $ TypeScheme [Name "x"] (l tx Types.~> Types.optional tx) Nothing
maybeTailSig = sig $ TypeScheme [Name "x"] (l tx Types.~> Types.optional (l tx)) Nothing
nubSig = sig $ Types.polyConstrained [("x", [Name "equality"])] (l tx Types.~> l tx)
nullSig = sig $ TypeScheme [Name "x"] (l tx Types.~> Types.boolean) Nothing
partitionSig = sig $ TypeScheme [Name "x"]
  ((tx Types.~> Types.boolean) Types.~> l tx Types.~> Types.pair (l tx) (l tx)) Nothing
pureSig = sig $ TypeScheme [Name "x"] (tx Types.~> l tx) Nothing
replicateSig = sig $ TypeScheme [Name "x"] (Types.int32 Types.~> tx Types.~> l tx) Nothing
reverseSig = sig $ TypeScheme [Name "x"] (l tx Types.~> l tx) Nothing
singletonSig = sig $ TypeScheme [Name "x"] (tx Types.~> l tx) Nothing
sortSig = sig $ Types.polyConstrained [("x", [Name "ordering"])] (l tx Types.~> l tx)
sortOnSig = sig $ Types.polyConstrained [("x", []), ("y", [Name "ordering"])]
  ((tx Types.~> ty) Types.~> l tx Types.~> l tx)
spanSig = sig $ TypeScheme [Name "x"]
  ((tx Types.~> Types.boolean) Types.~> l tx Types.~> Types.pair (l tx) (l tx)) Nothing
takeSig = sig $ TypeScheme [Name "x"] (Types.int32 Types.~> l tx Types.~> l tx) Nothing
transposeSig = sig $ TypeScheme [Name "x"] (l (l tx) Types.~> l (l tx)) Nothing
unconsSig = sig $ TypeScheme [Name "x"]
  (l tx Types.~> Types.optional (Types.pair tx (l tx))) Nothing
zipSig = sig $ TypeScheme [Name "x", Name "y"]
  (l tx Types.~> l ty Types.~> l (Types.pair tx ty)) Nothing
zipWithSig = sig $ TypeScheme [Name "x", Name "y", Name "z"]
  ((tx Types.~> ty Types.~> tz) Types.~> l tx Types.~> l ty Types.~> l tz) Nothing
