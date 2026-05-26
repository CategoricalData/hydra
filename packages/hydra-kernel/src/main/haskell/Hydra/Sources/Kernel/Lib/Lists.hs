-- | Primitive declarations for the hydra.lib.lists namespace.

module Hydra.Sources.Kernel.Lib.Lists where

import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.lists"

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

primNoDef :: String -> String -> TermSignature -> Definition
primNoDef localName description s =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName))

-- Shared type variables.
tx, ty, tz :: Type
tx = Types.var "x"
ty = Types.var "y"
tz = Types.var "z"

l :: Type -> Type
l = Types.list

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.lists namespace."}
  where
    definitions = [
      primNoDef "apply"       "Apply a list of functions to a list of values (applicative style)." applySig,
      primNoDef "bind"        "Apply a function that returns lists to each element and flatten the results." bindSig,
      primNoDef "concat"      "Concatenate a list of lists." concatSig,
      primNoDef "concat2"     "Concatenate two lists." concat2Sig,
      primNoDef "cons"        "Prepend a value to a list." consSig,
      primNoDef "drop"        "Drop the first n elements from a list." dropSig,
      primNoDef "dropWhile"   "Drop elements from the beginning of a list while the predicate is true." dropWhileSig,
      primNoDef "elem"        "Test whether an element is in a list." elemSig,
      primNoDef "filter"      "Filter a list by a predicate." filterSig,
      primNoDef "find"        "Find the first element matching a predicate." findSig,
      primNoDef "foldl"       "Left-fold a list with an accumulator." foldlSig,
      primNoDef "foldr"       "Right-fold a list with an accumulator." foldrSig,
      primNoDef "group"       "Group consecutive equal elements." groupSig,
      primNoDef "intercalate" "Intercalate a list of lists with a separator list between each." intercalateSig,
      primNoDef "intersperse" "Intersperse a value between consecutive elements of a list." intersperseSig,
      primNoDef "length"      "Return the length of a list." lengthSig,
      primNoDef "map"         "Map a function over a list." mapSig,
      primNoDef "maybeAt"     "Return the element at the given index, or Nothing if out of bounds." maybeAtSig,
      primNoDef "maybeHead"   "Return the first element, or Nothing if the list is empty." maybeHeadSig,
      primNoDef "maybeInit"   "Return all elements except the last, or Nothing if the list is empty." maybeInitSig,
      primNoDef "maybeLast"   "Return the last element, or Nothing if the list is empty." maybeLastSig,
      primNoDef "maybeTail"   "Return all elements except the first, or Nothing if the list is empty." maybeTailSig,
      primNoDef "nub"         "Remove duplicate elements from a list." nubSig,
      primNoDef "null"        "Test whether a list is empty." nullSig,
      primNoDef "partition"   "Partition a list into elements that satisfy a predicate and those that do not." partitionSig,
      primNoDef "pure"        "Wrap a value in a single-element list." pureSig,
      primNoDef "replicate"   "Build a list of n copies of a value." replicateSig,
      primNoDef "reverse"     "Reverse a list." reverseSig,
      primNoDef "singleton"   "Construct a single-element list." singletonSig,
      primNoDef "sort"        "Sort a list." sortSig,
      primNoDef "sortOn"      "Sort a list using a key-extraction function." sortOnSig,
      primNoDef "span"        "Split a list at the first element where the predicate fails." spanSig,
      primNoDef "take"        "Take the first n elements of a list." takeSig,
      primNoDef "transpose"   "Transpose a list of lists." transposeSig,
      primNoDef "uncons"      "Decompose a list into its head and tail, or Nothing if empty." unconsSig,
      primNoDef "zip"         "Zip two lists element-wise into pairs." zipSig,
      primNoDef "zipWith"     "Zip two lists with a combining function." zipWithSig]

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
