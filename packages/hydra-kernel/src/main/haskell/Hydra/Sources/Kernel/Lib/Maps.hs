-- | Primitive declarations for the hydra.lib.maps namespace.

module Hydra.Sources.Kernel.Lib.Maps where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.maps"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.maps module."}
  where
    definitions = [
      primNoDef "alter"           "Alter a value at a key using a function which sees the optional current value." alterSig (Just
        "alter(f, k, m) applies f to Just(v) when m contains key k with value v, or to Nothing when k is\
        \ absent. If f returns Just(v'), the binding (k, v') is set in the result; if f returns Nothing,\
        \ k is removed from the result. A single primitive that subsumes insert, delete, and adjust.\
        \ Requires an 'ordering' constraint on the key type. Total. Corresponds to Haskell's\
        \ Data.Map.alter :: Ord k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v."),
      primNoDef "bimap"           "Map functions over both the keys and values of a map." bimapSig (Just
        "bimap(fk, fv, m) returns a new map with key fk(k) and value fv(v) for each binding (k, v) in m.\
        \ Key collisions after applying fk are resolved by keeping the last binding encountered (host\
        \ may differ on collision policy if fk is not injective). Requires 'ordering' constraints on both\
        \ the input and output key types. Total. Corresponds to a key-and-value lift of Haskell's\
        \ Data.Map.fromList . map (\\(k,v) -> (fk k, fv v)) . toList."),
      primNoDef "delete"          "Remove a key from a map." deleteSig (Just
        "delete(k, m) returns m with the binding for k removed; if k is not present, m is returned\
        \ unchanged. Requires an 'ordering' constraint on the key type. Total. Corresponds to Haskell's\
        \ Data.Map.delete :: Ord k => k -> Map k v -> Map k v."),
      primNoDef "elems"           "Return the values of a map (in key order)." elemsSig (Just
        "elems(m) returns the values of m as a list, ordered by their keys' ascending order. Requires an\
        \ 'ordering' constraint on the key type. Total. Corresponds to Haskell's\
        \ Data.Map.elems :: Map k v -> [v]."),
      primNoDef "empty"           "The empty map." emptySig (Just
        "empty is the map with no bindings. Requires an 'ordering' constraint on the key type. Total.\
        \ Corresponds to Haskell's Data.Map.empty :: Map k v."),
      primNoDef "filter"          "Filter a map by value." filterSig (Just
        "filter(p, m) returns the submap of m containing exactly the bindings (k, v) for which p(v) is\
        \ true. Requires an 'ordering' constraint on the key type. Total. Corresponds to Haskell's\
        \ Data.Map.filter :: (v -> Bool) -> Map k v -> Map k v."),
      primNoDef "filterWithKey"   "Filter a map by key and value." filterWithKeySig (Just
        "filterWithKey(p, m) returns the submap of m containing exactly the bindings (k, v) for which\
        \ p(k, v) is true. Requires an 'ordering' constraint on the key type. Total. Corresponds to\
        \ Haskell's Data.Map.filterWithKey :: (k -> v -> Bool) -> Map k v -> Map k v."),
      primNoDef "findWithDefault" "Look up a value with a default if the key is absent." findWithDefaultSig (Just
        "findWithDefault(def, k, m) returns the value bound to k in m if k is present, or def otherwise.\
        \ Equivalent to maybe(def, identity, lookup(k, m)). Requires an 'ordering' constraint on the key\
        \ type. Total. Corresponds to Haskell's\
        \ Data.Map.findWithDefault :: Ord k => v -> k -> Map k v -> v."),
      primNoDef "fromList"        "Build a map from a list of key-value pairs." fromListSig (Just
        "fromList(xs) returns the map containing exactly the bindings in xs. If xs contains multiple\
        \ entries for the same key, the last one wins (matching Haskell's fromList behavior). Requires an\
        \ 'ordering' constraint on the key type. Total. Corresponds to Haskell's\
        \ Data.Map.fromList :: Ord k => [(k, v)] -> Map k v."),
      primNoDef "insert"          "Insert a key-value pair into a map." insertSig (Just
        "insert(k, v, m) returns m with the binding (k, v) added or updated. If k is already present, its\
        \ value is overwritten. Requires an 'ordering' constraint on the key type. Total. Corresponds to\
        \ Haskell's Data.Map.insert :: Ord k => k -> v -> Map k v -> Map k v."),
      primNoDef "keys"            "Return the keys of a map (in key order)." keysSig (Just
        "keys(m) returns the keys of m as a list, in ascending order. Requires an 'ordering' constraint\
        \ on the key type. Total. Corresponds to Haskell's Data.Map.keys :: Map k v -> [k]."),
      primNoDef "lookup"          "Look up a value in a map by key, returning Nothing if absent." lookupSig (Just
        "lookup(k, m) returns Just(v) where v is the value bound to k in m, or Nothing if k is not\
        \ present. Requires an 'ordering' constraint on the key type. Total. Corresponds to Haskell's\
        \ Data.Map.lookup :: Ord k => k -> Map k v -> Maybe v."),
      primNoDef "map"             "Map a function over the values of a map." mapSig (Just
        "map(f, m) returns a map with the same keys as m and value f(v) for each binding (k, v).\
        \ Requires an 'ordering' constraint on the key type. Total. Corresponds to Haskell's\
        \ Data.Map.map :: (v -> w) -> Map k v -> Map k w / fmap on Map."),
      primNoDef "mapKeys"         "Map a function over the keys of a map." mapKeysSig (Just
        "mapKeys(f, m) returns a map where each binding (k, v) becomes (f(k), v). If f maps multiple\
        \ keys to the same image, key collisions are resolved by keeping the binding with the greater\
        \ original key (matching Haskell's mapKeys behavior). Requires 'ordering' constraints on both\
        \ the input and output key types. Total. Corresponds to Haskell's\
        \ Data.Map.mapKeys :: (Ord k1, Ord k2) => (k1 -> k2) -> Map k1 v -> Map k2 v."),
      primNoDef "member"          "Test whether a key is present in a map." memberSig (Just
        "member(k, m) returns true iff k is a key in m. Requires an 'ordering' constraint on the key\
        \ type. Total. Corresponds to Haskell's Data.Map.member :: Ord k => k -> Map k v -> Bool."),
      primNoDef "null"            "Test whether a map is empty." nullSig (Just
        "null(m) returns true iff m has no bindings. Requires an 'ordering' constraint on the key type.\
        \ Total. Corresponds to Haskell's Data.Map.null :: Map k v -> Bool."),
      primNoDef "singleton"       "Construct a map with a single key-value pair." singletonSig (Just
        "singleton(k, v) returns the map containing exactly the binding (k, v). Requires an 'ordering'\
        \ constraint on the key type. Total. Corresponds to Haskell's\
        \ Data.Map.singleton :: k -> v -> Map k v."),
      primNoDef "size"            "Return the number of key-value pairs in a map." sizeSig (Just
        "size(m) returns the number of bindings in m as an int32. Requires an 'ordering' constraint on\
        \ the key type. Total. Corresponds to Haskell's Data.Map.size :: Map k v -> Int (with narrowing\
        \ to int32)."),
      primNoDef "toList"          "Convert a map to a list of key-value pairs (in key order)." toListSig (Just
        "toList(m) returns the bindings of m as a list of (key, value) pairs, in ascending key order.\
        \ Requires an 'ordering' constraint on the key type. Total. Corresponds to Haskell's\
        \ Data.Map.toList :: Map k v -> [(k, v)]."),
      primNoDef "union"           "Compute the union of two maps; the first map's bindings take precedence on key collision." unionSig (Just
        "union(m1, m2) returns the map containing all bindings from m1 plus the bindings of m2 whose\
        \ keys are not in m1. On key collision, the binding from m1 is preferred. Requires an 'ordering'\
        \ constraint on the key type. Total. Corresponds to Haskell's\
        \ Data.Map.union :: Ord k => Map k v -> Map k v -> Map k v.")]

mp :: Type -> Type -> Type
mp = Types.map

primNoDef :: String -> String -> TermSignature -> Maybe String -> Definition
primNoDef localName description s comments =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName)) comments

-- Type-var shortcuts.
tk, tk1, tk2, tv, tv1, tv2 :: Type
tk  = Types.var "k"
tk1 = Types.var "k1"
tk2 = Types.var "k2"
tv  = Types.var "v"
tv1 = Types.var "v1"
tv2 = Types.var "v2"

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

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

mapKeysSig :: TermSignature
mapKeysSig = sig $ Types.polyConstrained
  [("k1", [Name "ordering"]), ("k2", [Name "ordering"]), ("v", [])]
  ((tk1 Types.~> tk2) Types.~> mp tk1 tv Types.~> mp tk2 tv)

mapSig :: TermSignature
mapSig = sig $ Types.polyConstrained [("v1", []), ("v2", []), ("k", [Name "ordering"])]
  ((tv1 Types.~> tv2) Types.~> mp tk tv1 Types.~> mp tk tv2)

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
