-- | Primitive declarations for the hydra.lib.maps namespace.

module Hydra.Sources.Kernel.Lib.Maps where

import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.maps"

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

primNoDef :: String -> String -> TermSignature -> Definition
primNoDef localName description s =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName))

-- Type-var shortcuts.
tk, tk1, tk2, tv, tv1, tv2 :: Type
tk  = Types.var "k"
tk1 = Types.var "k1"
tk2 = Types.var "k2"
tv  = Types.var "v"
tv1 = Types.var "v1"
tv2 = Types.var "v2"

mp :: Type -> Type -> Type
mp = Types.map

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.maps namespace."}
  where
    definitions = [
      primNoDef "alter"           "Alter a value at a key using a function which sees the optional current value." alterSig,
      primNoDef "bimap"           "Map functions over both the keys and values of a map." bimapSig,
      primNoDef "delete"          "Remove a key from a map." deleteSig,
      primNoDef "elems"           "Return the values of a map (in key order)." elemsSig,
      primNoDef "empty"           "The empty map." emptySig,
      primNoDef "filter"          "Filter a map by value." filterSig,
      primNoDef "filterWithKey"   "Filter a map by key and value." filterWithKeySig,
      primNoDef "findWithDefault" "Look up a value with a default if the key is absent." findWithDefaultSig,
      primNoDef "fromList"        "Build a map from a list of key-value pairs." fromListSig,
      primNoDef "insert"          "Insert a key-value pair into a map." insertSig,
      primNoDef "keys"            "Return the keys of a map (in key order)." keysSig,
      primNoDef "lookup"          "Look up a value in a map by key, returning Nothing if absent." lookupSig,
      primNoDef "map"             "Map a function over the values of a map." mapSig,
      primNoDef "mapKeys"         "Map a function over the keys of a map." mapKeysSig,
      primNoDef "member"          "Test whether a key is present in a map." memberSig,
      primNoDef "null"            "Test whether a map is empty." nullSig,
      primNoDef "singleton"       "Construct a map with a single key-value pair." singletonSig,
      primNoDef "size"            "Return the number of key-value pairs in a map." sizeSig,
      primNoDef "toList"          "Convert a map to a list of key-value pairs (in key order)." toListSig,
      primNoDef "union"           "Compute the union of two maps; the first map's bindings take precedence on key collision." unionSig]

-- Signatures (k/v unconstrained except where ordering is required on key types).

-- alter : forall v. forall k:Ord. (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alterSig :: TermSignature
alterSig = sig $ Types.polyConstrained [("v", []), ("k", [Name "ordering"])]
  ((Types.optional tv Types.~> Types.optional tv) Types.~> tk Types.~> mp tk tv Types.~> mp tk tv)

-- bimap : forall k1:Ord k2:Ord v1 v2. (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2
bimapSig :: TermSignature
bimapSig = sig $ Types.polyConstrained
  [("k1", [Name "ordering"]), ("k2", [Name "ordering"]), ("v1", []), ("v2", [])]
  ((tk1 Types.~> tk2) Types.~> (tv1 Types.~> tv2) Types.~> mp tk1 tv1 Types.~> mp tk2 tv2)

deleteSig :: TermSignature
deleteSig = sig $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])]
  (tk Types.~> mp tk tv Types.~> mp tk tv)

elemsSig :: TermSignature
elemsSig = sig $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])]
  (mp tk tv Types.~> Types.list tv)

emptySig :: TermSignature
emptySig = sig $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])] (mp tk tv)

filterSig :: TermSignature
filterSig = sig $ Types.polyConstrained [("v", []), ("k", [Name "ordering"])]
  ((tv Types.~> Types.boolean) Types.~> mp tk tv Types.~> mp tk tv)

filterWithKeySig :: TermSignature
filterWithKeySig = sig $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])]
  ((tk Types.~> tv Types.~> Types.boolean) Types.~> mp tk tv Types.~> mp tk tv)

findWithDefaultSig :: TermSignature
findWithDefaultSig = sig $ Types.polyConstrained [("v", []), ("k", [Name "ordering"])]
  (tv Types.~> tk Types.~> mp tk tv Types.~> tv)

fromListSig :: TermSignature
fromListSig = sig $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])]
  (Types.list (Types.pair tk tv) Types.~> mp tk tv)

insertSig :: TermSignature
insertSig = sig $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])]
  (tk Types.~> tv Types.~> mp tk tv Types.~> mp tk tv)

keysSig :: TermSignature
keysSig = sig $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])]
  (mp tk tv Types.~> Types.list tk)

lookupSig :: TermSignature
lookupSig = sig $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])]
  (tk Types.~> mp tk tv Types.~> Types.optional tv)

mapSig :: TermSignature
mapSig = sig $ Types.polyConstrained [("v1", []), ("v2", []), ("k", [Name "ordering"])]
  ((tv1 Types.~> tv2) Types.~> mp tk tv1 Types.~> mp tk tv2)

mapKeysSig :: TermSignature
mapKeysSig = sig $ Types.polyConstrained
  [("k1", [Name "ordering"]), ("k2", [Name "ordering"]), ("v", [])]
  ((tk1 Types.~> tk2) Types.~> mp tk1 tv Types.~> mp tk2 tv)

memberSig :: TermSignature
memberSig = sig $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])]
  (tk Types.~> mp tk tv Types.~> Types.boolean)

nullSig :: TermSignature
nullSig = sig $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])]
  (mp tk tv Types.~> Types.boolean)

singletonSig :: TermSignature
singletonSig = sig $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])]
  (tk Types.~> tv Types.~> mp tk tv)

sizeSig :: TermSignature
sizeSig = sig $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])]
  (mp tk tv Types.~> Types.int32)

toListSig :: TermSignature
toListSig = sig $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])]
  (mp tk tv Types.~> Types.list (Types.pair tk tv))

unionSig :: TermSignature
unionSig = sig $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])]
  (mp tk tv Types.~> mp tk tv Types.~> mp tk tv)
