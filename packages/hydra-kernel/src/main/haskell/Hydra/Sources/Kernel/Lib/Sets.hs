-- | Primitive declarations for the hydra.lib.sets namespace.

module Hydra.Sources.Kernel.Lib.Sets where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Set                    as S


ns :: ModuleName
ns = ModuleName "hydra.lib.sets"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.sets namespace."}
  where
    sx = Types.var "x"
    ssx = Types.set sx
    definitions = [
      primNoDef "delete"   "Remove an element from a set." (setOpSig (sx Types.~> ssx Types.~> ssx)) (Just
        "delete(x, s) returns s with x removed; if x is not in s, s is returned unchanged. Requires an\
        \ 'ordering' constraint on the element type. Total. Corresponds to Haskell's\
        \ Data.Set.delete :: Ord a => a -> Set a -> Set a."),
      toPrimitive "Compute the difference of two sets: elements in the first that are not in the second." (setOpSig (ssx Types.~> ssx Types.~> ssx)) (Just
        "difference(s1, s2) returns the set of elements that are in s1 but not in s2. Requires an\
        \ 'ordering' constraint on the element type. Total. Corresponds to Haskell's\
        \ Data.Set.difference :: Ord a => Set a -> Set a -> Set a.") difference_,
      primNoDef "empty"    "The empty set." (setOpSig ssx) (Just
        "empty is the set with no elements. Requires an 'ordering' constraint on the element type. Total.\
        \ Corresponds to Haskell's Data.Set.empty :: Set a."),
      primNoDef "fromList" "Construct a set from a list of elements (duplicates removed)." (setOpSig (Types.list sx Types.~> ssx)) (Just
        "fromList(xs) returns the set containing exactly the distinct elements of xs; duplicates are\
        \ discarded. Requires an 'ordering' constraint on the element type. Total. Corresponds to\
        \ Haskell's Data.Set.fromList :: Ord a => [a] -> Set a."),
      primNoDef "insert"   "Add an element to a set." (setOpSig (sx Types.~> ssx Types.~> ssx)) (Just
        "insert(x, s) returns s with x added; if x is already in s, s is returned unchanged. Requires an\
        \ 'ordering' constraint on the element type. Total. Corresponds to Haskell's\
        \ Data.Set.insert :: Ord a => a -> Set a -> Set a."),
      toPrimitive "Compute the intersection of two sets: elements present in both." (setOpSig (ssx Types.~> ssx Types.~> ssx)) (Just
        "intersection(s1, s2) returns the set of elements present in both s1 and s2. Requires an\
        \ 'ordering' constraint on the element type. Total. Corresponds to Haskell's\
        \ Data.Set.intersection :: Ord a => Set a -> Set a -> Set a.") intersection_,
      toPrimitive "Map a function over a set." mapSig (Just
        "map(f, s) returns the set of f(x) for each x in s. Elements that f maps to the same image are\
        \ deduplicated by the result type's ordering. Requires 'ordering' constraints on both the input\
        \ and output element types. Total. Corresponds to Haskell's\
        \ Data.Set.map :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b.") map_,
      primNoDef "member"   "Test whether an element is in a set." (setOpSig (sx Types.~> ssx Types.~> Types.boolean)) (Just
        "member(x, s) returns true iff x is an element of s. Requires an 'ordering' constraint on the\
        \ element type. Total. Corresponds to Haskell's\
        \ Data.Set.member :: Ord a => a -> Set a -> Bool."),
      primNoDef "null"     "Test whether a set is empty." (setOpSig (ssx Types.~> Types.boolean)) (Just
        "null(s) returns true iff s has no elements. Requires an 'ordering' constraint on the element\
        \ type. Total. Corresponds to Haskell's Data.Set.null :: Set a -> Bool."),
      primNoDef "singleton" "Construct a set containing a single element." (setOpSig (sx Types.~> ssx)) (Just
        "singleton(x) returns the set containing exactly the element x. Requires an 'ordering'\
        \ constraint on the element type. Total. Corresponds to Haskell's\
        \ Data.Set.singleton :: a -> Set a."),
      primNoDef "size"     "Return the number of elements in a set." (setOpSig (ssx Types.~> Types.int32)) (Just
        "size(s) returns the number of elements in s as an int32. Requires an 'ordering' constraint on\
        \ the element type. Total. Corresponds to Haskell's Data.Set.size :: Set a -> Int (with\
        \ narrowing to int32)."),
      primNoDef "toList"   "Convert a set to a list (in ascending order)." (setOpSig (ssx Types.~> Types.list sx)) (Just
        "toList(s) returns the elements of s as a list, in ascending order under the element type's\
        \ ordering. Requires an 'ordering' constraint on the element type. Total. Corresponds to\
        \ Haskell's Data.Set.toAscList :: Set a -> [a]."),
      toPrimitive "Compute the union of two sets: elements in either." (setOpSig (ssx Types.~> ssx Types.~> ssx)) (Just
        "union(s1, s2) returns the set of elements that are in s1 or in s2 (or both). Requires an\
        \ 'ordering' constraint on the element type. Total. Corresponds to Haskell's\
        \ Data.Set.union :: Ord a => Set a -> Set a -> Set a.") union_,
      toPrimitive "Compute the union of a list of sets." (setOpSig (Types.list ssx Types.~> ssx)) (Just
        "unions(ss) returns the union of every set in ss. Equivalent to folding union over ss starting\
        \ from empty. Requires an 'ordering' constraint on the element type. Total. Corresponds to\
        \ Haskell's Data.Set.unions :: Ord a => [Set a] -> Set a.") unions_]

-- map needs two ord-constrained type vars: x and y
mapSig :: TermSignature
mapSig = sig $ Types.polyConstrained [("x", [Name "ordering"]), ("y", [Name "ordering"])]
  ((Types.var "x" Types.~> Types.var "y") Types.~>
   Types.set (Types.var "x") Types.~>
   Types.set (Types.var "y"))

primNoDef :: String -> String -> TermSignature -> Maybe String -> Definition
primNoDef localName description s comments =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName)) comments

-- Helper: build a TermSignature for a one-ord-var-x signature.
setOpSig :: Type -> TermSignature
setOpSig body = sig $ Types.polyConstrained [("x", [Name "ordering"])] body

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

-- Default implementations.

-- difference s1 s2 = foldl (\acc el -> ifElse (member el s2) acc (insert el acc)) empty (toList s1)
difference_ :: TTermDefinition (S.Set a -> S.Set a -> S.Set a)
difference_ = define "difference" $
  doc "Set difference, defined in terms of member and insert." $
  "s1" ~> "s2" ~>
    Lists.foldl
      ("acc" ~> "el" ~> Logic.ifElse (Sets.member (var "el") (var "s2"))
        (var "acc" :: TTerm (S.Set a))
        (Sets.insert (var "el") (var "acc")))
      (Sets.empty :: TTerm (S.Set a))
      (Sets.toList (var "s1"))

-- intersection s1 s2 = foldl (\acc el -> ifElse (member el s2) (insert el acc) acc) empty (toList s1)
intersection_ :: TTermDefinition (S.Set a -> S.Set a -> S.Set a)
intersection_ = define "intersection" $
  doc "Set intersection, defined in terms of member and insert." $
  "s1" ~> "s2" ~>
    Lists.foldl
      ("acc" ~> "el" ~> Logic.ifElse (Sets.member (var "el") (var "s2"))
        (Sets.insert (var "el") (var "acc"))
        (var "acc" :: TTerm (S.Set a)))
      (Sets.empty :: TTerm (S.Set a))
      (Sets.toList (var "s1"))

-- map f s = fromList (Lists.map f (toList s))
map_ :: TTermDefinition ((a -> b) -> S.Set a -> S.Set b)
map_ = define "map" $
  doc "Map a function over a set, defined in terms of toList, lists.map and fromList." $
  "f" ~> "s" ~> Sets.fromList (Lists.map (var "f") (Sets.toList (var "s")))

-- union s1 s2 = foldl (\acc el -> insert el acc) s2 (toList s1)
union_ :: TTermDefinition (S.Set a -> S.Set a -> S.Set a)
union_ = define "union" $
  doc "Set union, defined in terms of insert and toList." $
  "s1" ~> "s2" ~>
    Lists.foldl
      ("acc" ~> "el" ~> Sets.insert (var "el") (var "acc"))
      (var "s2" :: TTerm (S.Set a))
      (Sets.toList (var "s1"))

-- unions ss = foldl union empty ss
unions_ :: TTermDefinition ([S.Set a] -> S.Set a)
unions_ = define "unions" $
  doc "Union of a list of sets, defined in terms of foldl and union." $
  "ss" ~> Lists.foldl
    ("acc" ~> "s" ~> Sets.union (var "acc") (var "s"))
    (Sets.empty :: TTerm (S.Set a))
    (var "ss")
