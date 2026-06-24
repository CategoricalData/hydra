-- | Primitive declarations for the hydra.lib.sets namespace.
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Lib.Sets where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Phantoms     as Phantoms hiding (map)
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), map, null)
import qualified Data.Set                    as S


ns :: ModuleName
ns = ModuleName "hydra.lib.sets"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.sets module.")}
  where
    -- The default-impl args below (difference/intersection/map/union/unions) carry an `:: ...Int...`
    -- placeholder instantiation. The generated `Hydra.Dsl.Lib.Sets` (unlike the old `Meta.Lib.Sets`)
    -- exposes the primitive's `Ord` element constraint, so these polymorphic defs need a concrete `Ord`
    -- type here to satisfy GHC. `Int` is arbitrary and carries no meaning — the emitted primitive is
    -- type-agnostic and fully polymorphic; only the Haskell typechecker sees the `Int`. See #467.
    definitions = [delete, difference, empty, fromList, insert, intersection, map, member, null,
                   singleton, size, toList, union, unions]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

defineWithDefault :: String -> String -> TermSignature -> [String] -> TypedTerm a -> PrimitiveDefinition
defineWithDefault = primitiveWithDefaultInModule module_

-- Helper: build a TermSignature for a one-ord-var-x signature.
setOp :: Type -> TermSignature
setOp body = sig $ Types.polyConstrained [("x", [Name "ordering"])] body

sx :: Type
sx = Types.var "x"

ssx :: Type
ssx = Types.set sx

delete :: PrimitiveDefinition
delete = define "delete" "Remove an element from a set."
  (setOp (sx Types.~> ssx Types.~> ssx))
  ["delete(x, s) returns s with x removed; if x is not in s, s is returned unchanged.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.delete :: Ord a => a -> Set a -> Set a."]

difference :: PrimitiveDefinition
difference = defineWithDefault "difference" "Compute the difference of two sets: elements in the first that are not in the second."
  (setOp (ssx Types.~> ssx Types.~> ssx))
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
empty = define "empty" "The empty set."
  (setOp ssx)
  ["empty is the set with no elements.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.empty :: Set a."]

fromList :: PrimitiveDefinition
fromList = define "fromList" "Construct a set from a list of elements (duplicates removed)."
  (setOp (Types.list sx Types.~> ssx))
  ["fromList(xs) returns the set containing exactly the distinct elements of xs; duplicates are\
  \ discarded.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.fromList :: Ord a => [a] -> Set a."]

insert :: PrimitiveDefinition
insert = define "insert" "Add an element to a set."
  (setOp (sx Types.~> ssx Types.~> ssx))
  ["insert(x, s) returns s with x added; if x is already in s, s is returned unchanged.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.insert :: Ord a => a -> Set a -> Set a."]

intersection :: PrimitiveDefinition
intersection = defineWithDefault "intersection" "Compute the intersection of two sets: elements present in both."
  (setOp (ssx Types.~> ssx Types.~> ssx))
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

map :: PrimitiveDefinition
map = defineWithDefault "map" "Map a function over a set."
  (sig $ Types.polyConstrained [("x", [Name "ordering"]), ("y", [Name "ordering"])]
    ((Types.var "x" Types.~> Types.var "y") Types.~> Types.set (Types.var "x") Types.~> Types.set (Types.var "y")))
  ["map(f, s) returns the set of f(x) for each x in s. Elements that f maps to the same image are\
  \ deduplicated by the result type's ordering.",
   "Requires 'ordering' constraints on both the input and output element types.",
   "Total. Corresponds to Haskell's Data.Set.map :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b."]
  (("f" ~> "s" ~> (Sets.fromList (Lists.map (var "f") (Sets.toList (var "s" :: TypedTerm (S.Set Int)))) :: TypedTerm (S.Set Int)))
    :: TypedTerm ((Int -> Int) -> S.Set Int -> S.Set Int))

member :: PrimitiveDefinition
member = define "member" "Test whether an element is in a set."
  (setOp (sx Types.~> ssx Types.~> Types.boolean))
  ["member(x, s) returns true iff x is an element of s.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.member :: Ord a => a -> Set a -> Bool."]

null :: PrimitiveDefinition
null = define "null" "Test whether a set is empty."
  (setOp (ssx Types.~> Types.boolean))
  ["null(s) returns true iff s has no elements.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.null :: Set a -> Bool."]

singleton :: PrimitiveDefinition
singleton = define "singleton" "Construct a set containing a single element."
  (setOp (sx Types.~> ssx))
  ["singleton(x) returns the set containing exactly the element x.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.singleton :: a -> Set a."]

size :: PrimitiveDefinition
size = define "size" "Return the number of elements in a set."
  (setOp (ssx Types.~> Types.int32))
  ["size(s) returns the number of elements in s as an int32.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.size :: Set a -> Int (with narrowing to int32)."]

toList :: PrimitiveDefinition
toList = define "toList" "Convert a set to a list (in ascending order)."
  (setOp (ssx Types.~> Types.list sx))
  ["toList(s) returns the elements of s as a list, in ascending order under the element type's\
  \ ordering.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.toAscList :: Set a -> [a]."]

union :: PrimitiveDefinition
union = defineWithDefault "union" "Compute the union of two sets: elements in either."
  (setOp (ssx Types.~> ssx Types.~> ssx))
  ["union(s1, s2) returns the set of elements that are in s1 or in s2 (or both).",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.union :: Ord a => Set a -> Set a -> Set a."]
  (("s1" ~> "s2" ~>
    Lists.foldl
      ("acc" ~> "el" ~> Sets.insert (var "el" :: TypedTerm Int) (var "acc"))
      (var "s2" :: TypedTerm (S.Set Int))
      (Sets.toList (var "s1" :: TypedTerm (S.Set Int)))) :: TypedTerm (S.Set Int -> S.Set Int -> S.Set Int))

unions :: PrimitiveDefinition
unions = defineWithDefault "unions" "Compute the union of a list of sets."
  (setOp (Types.list ssx Types.~> ssx))
  ["unions(ss) returns the union of every set in ss. Equivalent to folding union over ss starting\
  \ from empty.",
   "Requires an 'ordering' constraint on the element type.",
   "Total. Corresponds to Haskell's Data.Set.unions :: Ord a => [Set a] -> Set a."]
  (("ss" ~> Lists.foldl
    ("acc" ~> "s" ~> Sets.union (var "acc" :: TypedTerm (S.Set Int)) (var "s"))
    (Sets.empty :: TypedTerm (S.Set Int))
    (var "ss")) :: TypedTerm ([S.Set Int] -> S.Set Int))
