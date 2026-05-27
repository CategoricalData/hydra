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

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

primNoDef :: String -> String -> TermSignature -> Definition
primNoDef localName description s =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName))

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
      primNoDef "delete"   "Remove an element from a set." (setOpSig (sx Types.~> ssx Types.~> ssx)),
      toPrimitive "Compute the difference of two sets: elements in the first that are not in the second." (setOpSig (ssx Types.~> ssx Types.~> ssx)) difference_,
      primNoDef "empty"    "The empty set." (setOpSig ssx),
      primNoDef "fromList" "Construct a set from a list of elements (duplicates removed)." (setOpSig (Types.list sx Types.~> ssx)),
      primNoDef "insert"   "Add an element to a set." (setOpSig (sx Types.~> ssx Types.~> ssx)),
      toPrimitive "Compute the intersection of two sets: elements present in both." (setOpSig (ssx Types.~> ssx Types.~> ssx)) intersection_,
      toPrimitive "Map a function over a set." mapSig map_,
      primNoDef "member"   "Test whether an element is in a set." (setOpSig (sx Types.~> ssx Types.~> Types.boolean)),
      primNoDef "null"     "Test whether a set is empty." (setOpSig (ssx Types.~> Types.boolean)),
      primNoDef "singleton" "Construct a set containing a single element." (setOpSig (sx Types.~> ssx)),
      primNoDef "size"     "Return the number of elements in a set." (setOpSig (ssx Types.~> Types.int32)),
      primNoDef "toList"   "Convert a set to a list (in unspecified order)." (setOpSig (ssx Types.~> Types.list sx)),
      toPrimitive "Compute the union of two sets: elements in either." (setOpSig (ssx Types.~> ssx Types.~> ssx)) union_,
      toPrimitive "Compute the union of a list of sets." (setOpSig (Types.list ssx Types.~> ssx)) unions_]

-- Helper: build a TermSignature for a one-ord-var-x signature.
setOpSig :: Type -> TermSignature
setOpSig body = sig $ Types.polyConstrained [("x", [Name "ordering"])] body

-- map needs two ord-constrained type vars: x and y
mapSig :: TermSignature
mapSig = sig $ Types.polyConstrained [("x", [Name "ordering"]), ("y", [Name "ordering"])]
  ((Types.var "x" Types.~> Types.var "y") Types.~>
   Types.set (Types.var "x") Types.~>
   Types.set (Types.var "y"))

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
