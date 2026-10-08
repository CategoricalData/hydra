-- | Primitive declarations for the hydra.core.lib.sets namespace.
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Lib.Sets where

import Hydra.Kernel
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Core.Dsl.Lib.Lists    as Lists
import qualified Hydra.Core.Dsl.Lib.Logic    as Logic
import qualified Hydra.Core.Dsl.Lib.Sets     as Sets
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms hiding (map)
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), filter, map)
import qualified Data.Set                    as S


ns :: ModuleName
ns = ModuleName "hydra.core.lib.sets"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.core.lib.sets module.")}
  where
    -- The default-impl args below (difference/intersection/map/union/unions) carry an `:: ...Int...`
    -- placeholder instantiation. The generated `Hydra.Core.Dsl.Lib.Sets` (unlike the old `Meta.Lib.Sets`)
    -- exposes the primitive's `Ord` element constraint, so these polymorphic defs need a concrete `Ord`
    -- type here to satisfy GHC. `Int` is arbitrary and carries no meaning — the emitted primitive is
    -- type-agnostic and fully polymorphic; only the Haskell typechecker sees the `Int`. See #467.
    definitions = [delete, difference, empty, filter, fromList, insert, intersection, isEmpty, map,
                   member, singleton, size, toList, union, unions]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

defineWithDefault :: String -> String -> TermSignature -> [String] -> TypedTerm a -> PrimitiveDefinition
defineWithDefault = primitiveWithDefaultInModule module_

-- Helper: build a TermSignature for a one-ord-var-x signature.
setOp :: [(String, String)] -> Type -> TermSignature
setOp params body = sigWithParams params $ Types.polyConstrained [("x", [Name "ordering"])] body

sx :: Type
sx = Types.var "x"

ssx :: Type
ssx = Types.set sx

delete :: PrimitiveDefinition
delete = withProvisions [
  requirement "removesElement"
    "delete removes the given element from the set if present, leaving every other element\
    \ unchanged.",
  requirement "noOpWhenAbsentOrEmpty"
    "delete returns the set unchanged when the element is not present, including when the set is\
    \ empty."]
  $ define "delete" "Remove an element from a set."
  (setOp [("x", "the element to remove"), ("s", "the set to remove the element from")] (sx Types.~> ssx Types.~> ssx))
  ["delete(x, s) returns s with x removed; if x is not in s, s is returned unchanged.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.delete :: Ord a => a -> Set a -> Set a."]

difference :: PrimitiveDefinition
difference = withProvisions [
  requirement "removesElementsOfSecondSet"
    "difference returns the elements of the first set that do not occur in the second set.",
  requirement "identityWhenSecondEmpty"
    "difference with an empty second set returns the first set unchanged."]
  $ defineWithDefault "difference" "Compute the difference of two sets: elements in the first that are not in the second."
  (setOp [("s1", "the set to subtract from"), ("s2", "the set of elements to remove")] (ssx Types.~> ssx Types.~> ssx))
  ["difference(s1, s2) returns the set of elements that are in s1 but not in s2.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.difference :: Ord a => Set a -> Set a -> Set a."]
  (("s1" ~> "s2" ~>
    Lists.foldl
      ("acc" ~> "el" ~> Logic.ifElse (Sets.member (var "el" :: TypedTerm Int) (var "s2"))
        (var "acc" :: TypedTerm (S.Set Int))
        (Sets.insert (var "el" :: TypedTerm Int) (var "acc")))
      (Sets.empty :: TypedTerm (S.Set Int))
      (Sets.toList (var "s1" :: TypedTerm (S.Set Int)))) :: TypedTerm (S.Set Int -> S.Set Int -> S.Set Int))

empty :: PrimitiveDefinition
empty = withProvisions [
  requirement "hasNoElements"
    "empty is the set containing no elements, for any element type."]
  $ define "empty" "The empty set."
  (sig $ Types.polyConstrained [("x", [Name "ordering"])] ssx)
  ["empty is the set with no elements.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.empty :: Set a."]

filter :: PrimitiveDefinition
filter = defineWithDefault "filter" "Filter a set by a predicate."
  (setOp [("predicate", "the predicate to test each element"), ("s", "the set to filter")] ((sx Types.~> Types.boolean) Types.~> ssx Types.~> ssx))
  ["filter(p, s) returns the subset of s containing exactly the elements x for which p(x) is true.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.filter :: (a -> Bool) -> Set a -> Set a."]
  (("p" ~> "s" ~> (Sets.fromList (Lists.filter (var "p") (Sets.toList (var "s" :: TypedTerm (S.Set Int)))) :: TypedTerm (S.Set Int)))
    :: TypedTerm ((Int -> Bool) -> S.Set Int -> S.Set Int))

fromList :: PrimitiveDefinition
fromList = withProvisions [
  requirement "collectsDistinctElements"
    "fromList returns the set of the distinct elements of the given list, discarding duplicates\
    \ and order.",
  requirement "emptyListYieldsEmptySet"
    "fromList of the empty list is the empty set."]
  $ define "fromList" "Construct a set from a list of elements (duplicates removed)."
  (setOp [("xs", "the list of elements to build the set from")] (Types.list sx Types.~> ssx))
  ["fromList(xs) returns the set containing exactly the distinct elements of xs; duplicates are\
  \ discarded.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.fromList :: Ord a => [a] -> Set a."]

insert :: PrimitiveDefinition
insert = withProvisions [
  requirement "addsElement"
    "insert adds the given element to the set, leaving every other element unchanged.",
  requirement "noOpWhenAlreadyPresent"
    "insert returns the set unchanged when the element is already present."]
  $ define "insert" "Add an element to a set."
  (setOp [("x", "the element to add"), ("s", "the set to add the element to")] (sx Types.~> ssx Types.~> ssx))
  ["insert(x, s) returns s with x added; if x is already in s, s is returned unchanged.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.insert :: Ord a => a -> Set a -> Set a."]

intersection :: PrimitiveDefinition
intersection = withProvisions [
  requirement "keepsElementsInBoth"
    "intersection returns exactly the elements present in both sets.",
  requirement "emptyWhenNoOverlapOrEitherEmpty"
    "intersection is the empty set when the two sets share no elements, or when either set is\
    \ empty."]
  $ defineWithDefault "intersection" "Compute the intersection of two sets: elements present in both."
  (setOp [("s1", "the first set"), ("s2", "the second set")] (ssx Types.~> ssx Types.~> ssx))
  ["intersection(s1, s2) returns the set of elements present in both s1 and s2.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.intersection :: Ord a => Set a -> Set a -> Set a."]
  (("s1" ~> "s2" ~>
    Lists.foldl
      ("acc" ~> "el" ~> Logic.ifElse (Sets.member (var "el" :: TypedTerm Int) (var "s2"))
        (Sets.insert (var "el" :: TypedTerm Int) (var "acc"))
        (var "acc" :: TypedTerm (S.Set Int)))
      (Sets.empty :: TypedTerm (S.Set Int))
      (Sets.toList (var "s1" :: TypedTerm (S.Set Int)))) :: TypedTerm (S.Set Int -> S.Set Int -> S.Set Int))

isEmpty :: PrimitiveDefinition
isEmpty = withProvisions [
  requirement "testsZeroSize"
    "isEmpty returns true exactly when the set has no elements."]
  $ define "isEmpty" "Test whether a set is empty."
  (setOp [("s", "the set to test for emptiness")] (ssx Types.~> Types.boolean))
  ["isEmpty(s) returns true iff s has no elements.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.null :: Set a -> Bool."]

map :: PrimitiveDefinition
map = withProvisions [
  requirement "appliesFunctionToEachElement"
    "map returns the set of applying the given function to each element, deduplicating results\
    \ that collide under the output type's ordering.",
  requirement "emptyInput"
    "map over the empty set is the empty set, regardless of the function."]
  $ defineWithDefault "map" "Map a function over a set."
  (sigWithParams [("f", "the function to apply to each element"), ("s", "the set to map over")] $ Types.polyConstrained [("x", [Name "ordering"]), ("y", [Name "ordering"])]
    ((Types.var "x" Types.~> Types.var "y") Types.~> Types.set (Types.var "x") Types.~> Types.set (Types.var "y")))
  ["map(f, s) returns the set of f(x) for each x in s. Elements that f maps to the same image are\
  \ deduplicated by the result type's ordering.",
   "Requires 'ordering' constraints on both the input and output element types.",
   "Total. Corresponds to Haskell's Data.Set.map :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b."]
  (("f" ~> "s" ~> (Sets.fromList (Lists.map (var "f") (Sets.toList (var "s" :: TypedTerm (S.Set Int)))) :: TypedTerm (S.Set Int)))
    :: TypedTerm ((Int -> Int) -> S.Set Int -> S.Set Int))

member :: PrimitiveDefinition
member = withProvisions [
  requirement "testsSetMembership"
    "member returns true exactly when the given value is an element of the set.",
  requirement "falseWhenAbsentOrEmpty"
    "member returns false when the value is not an element of the set, including when the set is\
    \ empty."]
  $ define "member" "Test whether an element is in a set."
  (setOp [("x", "the element to test for membership"), ("s", "the set to test against")] (sx Types.~> ssx Types.~> Types.boolean))
  ["member(x, s) returns true iff x is an element of s.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.member :: Ord a => a -> Set a -> Bool."]

singleton :: PrimitiveDefinition
singleton = withProvisions [
  requirement "wrapsOneElement"
    "singleton x returns the set containing exactly the element x."]
  $ define "singleton" "Construct a set containing a single element."
  (setOp [("x", "the single element of the set")] (sx Types.~> ssx))
  ["singleton(x) returns the set containing exactly the element x.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.singleton :: a -> Set a."]

size :: PrimitiveDefinition
size = withProvisions [
  requirement "countsElements"
    "size returns the number of elements in the set, including zero for the empty set."]
  $ define "size" "Return the number of elements in a set."
  (setOp [("s", "the set whose size is returned")] (ssx Types.~> Types.int32))
  ["size(s) returns the number of elements in s as an int32.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.size :: Set a -> Int (with narrowing to int32)."]

toList :: PrimitiveDefinition
toList = withProvisions [
  requirement "ordersAscending"
    "toList returns the elements of the set as a list in ascending order under the element type's\
    \ ordering, regardless of insertion order.",
  requirement "emptySetYieldsEmptyList"
    "toList of the empty set is the empty list."]
  $ define "toList" "Convert a set to a list (in ascending order)."
  (setOp [("s", "the set to convert to a list")] (ssx Types.~> Types.list sx))
  ["toList(s) returns the elements of s as a list, in ascending order under the element type's\
  \ ordering.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.toAscList :: Set a -> [a]."]

union :: PrimitiveDefinition
union = withProvisions [
  requirement "keepsElementsInEither"
    "union returns the elements that are in either set (or both), with no duplicates.",
  requirement "identityWhenEitherEmpty"
    "union with either set empty returns the other set unchanged."]
  $ defineWithDefault "union" "Compute the union of two sets: elements in either."
  (setOp [("s1", "the first set"), ("s2", "the second set")] (ssx Types.~> ssx Types.~> ssx))
  ["union(s1, s2) returns the set of elements that are in s1 or in s2 (or both).",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.union :: Ord a => Set a -> Set a -> Set a."]
  (("s1" ~> "s2" ~>
    Lists.foldl
      ("acc" ~> "el" ~> Sets.insert (var "el" :: TypedTerm Int) (var "acc"))
      (var "s2" :: TypedTerm (S.Set Int))
      (Sets.toList (var "s1" :: TypedTerm (S.Set Int)))) :: TypedTerm (S.Set Int -> S.Set Int -> S.Set Int))

unions :: PrimitiveDefinition
unions = withProvisions [
  requirement "keepsElementsInAny"
    "unions returns the elements that occur in at least one of the given sets, equivalent to\
    \ folding union over the list starting from empty.",
  requirement "emptyListOfSetsYieldsEmptySet"
    "unions of an empty list of sets is the empty set."]
  $ defineWithDefault "unions" "Compute the union of a list of sets."
  (setOp [("ss", "the list of sets to union together")] (Types.list ssx Types.~> ssx))
  ["unions(ss) returns the union of every set in ss. Equivalent to folding union over ss starting\
  \ from empty.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.unions :: Ord a => [Set a] -> Set a."]
  (("ss" ~> Lists.foldl
    ("acc" ~> "s" ~> Sets.union (var "acc" :: TypedTerm (S.Set Int)) (var "s"))
    (Sets.empty :: TypedTerm (S.Set Int))
    (var "ss")) :: TypedTerm ([S.Set Int] -> S.Set Int))
