-- | Default-library primitive adapters. Wraps the generated default library functions
--   (from Hydra.Lib.Defaults.*) into Primitive values for the test infrastructure.
--   The generated code is the source of truth; this module only adapts calling conventions.

module Hydra.DefaultsPrimitives (
  defaultLibraries,
) where

import Hydra.Kernel
import Hydra.Sources.Libraries
import Hydra.Dsl.Prims as Prims

import qualified Hydra.Lib.Defaults.Eithers as DefaultEithers
import qualified Hydra.Lib.Defaults.Equality as DefaultEquality
import qualified Hydra.Lib.Defaults.Lists as DefaultLists
import qualified Hydra.Lib.Defaults.Maps as DefaultMaps
import qualified Hydra.Lib.Defaults.Math as DefaultMath
import qualified Hydra.Lib.Defaults.Maybes as DefaultMaybes
import qualified Hydra.Lib.Defaults.Pairs as DefaultPairs
import qualified Hydra.Lib.Defaults.Sets as DefaultSets


-- | All default-library implementations, to be used as replacements for standard libraries in test mode.
defaultLibraries :: [Library]
defaultLibraries = [
  defaultLibEithers,
  defaultLibEquality,
  defaultLibLists,
  defaultLibMaps,
  defaultLibMath,
  defaultLibMaybes,
  defaultLibPairs,
  defaultLibSets]

-- Helpers

mkPrim :: Name -> Int -> (InferenceContext -> Graph -> [Term] -> Either Error Term) -> Primitive
mkPrim name arity impl = Primitive {
  primitiveDefinition = PrimitiveDefinition {
    primitiveDefinitionName = name,
    primitiveDefinitionSignature = typeSchemeToTermSignature (dummyType arity),
    primitiveDefinitionMetadata = Nothing,
    primitiveDefinitionIsPure = True,
    primitiveDefinitionIsTotal = True,
    primitiveDefinitionDefaultImplementation = Nothing},
  primitiveImplementation = impl}

dummyType :: Int -> TypeScheme
dummyType 0 = TypeScheme [] (TypeVariable (Name "a")) Nothing
dummyType n = TypeScheme [] (go n) Nothing
  where
    go 1 = funType (TypeVariable (Name "a")) (TypeVariable (Name "b"))
    go k = funType (TypeVariable (Name "a")) (go (k - 1))

funType :: Type -> Type -> Type
funType a b = TypeFunction (FunctionType a b)

-- | Coerce polymorphic error type to Error for generated eval primitives
--   whose type parameters are universally quantified (they always return Right).
coerceError :: Either e Term -> Either Error Term
coerceError (Right t) = Right t
coerceError (Left _) = error "coerceError: impossible Left from always-Right eval primitive"

-- ---- Eithers ----

defaultLibEithers :: Library
defaultLibEithers = standardLibrary _hydra_lib_eithers [
  mkPrim _eithers_bimap 3 $ \cx g args -> case args of
    [lf, rf, t] -> DefaultEithers.bimap cx g lf rf t
    _ -> unexpected cx "eithers.bimap" 3,
  mkPrim _eithers_bind 2 $ \cx g args -> case args of
    [t, f] -> DefaultEithers.bind cx g t f
    _ -> unexpected cx "eithers.bind" 2,
  mkPrim _eithers_either 3 $ \cx g args -> case args of
    [lf, rf, t] -> DefaultEithers.either cx g lf rf t
    _ -> unexpected cx "eithers.either" 3,
  mkPrim _eithers_foldl 3 $ \cx g args -> case args of
    [f, init_, lt] -> DefaultEithers.foldl cx g f init_ lt
    _ -> unexpected cx "eithers.foldl" 3,
  mkPrim _eithers_fromLeft 2 $ \cx g args -> case args of
    [def, t] -> DefaultEithers.fromLeft cx g def t
    _ -> unexpected cx "eithers.fromLeft" 2,
  mkPrim _eithers_fromRight 2 $ \cx g args -> case args of
    [def, t] -> DefaultEithers.fromRight cx g def t
    _ -> unexpected cx "eithers.fromRight" 2,
  mkPrim _eithers_isLeft 1 $ \cx g args -> case args of
    [t] -> DefaultEithers.isLeft cx g t
    _ -> unexpected cx "eithers.isLeft" 1,
  mkPrim _eithers_isRight 1 $ \cx g args -> case args of
    [t] -> DefaultEithers.isRight cx g t
    _ -> unexpected cx "eithers.isRight" 1,
  mkPrim _eithers_lefts 1 $ \cx g args -> case args of
    [t] -> DefaultEithers.lefts cx g t
    _ -> unexpected cx "eithers.lefts" 1,
  mkPrim _eithers_map 2 $ \cx g args -> case args of
    [f, t] -> DefaultEithers.map cx g f t
    _ -> unexpected cx "eithers.map" 2,
  mkPrim _eithers_mapList 2 $ \cx g args -> case args of
    [f, lt] -> DefaultEithers.mapList cx g f lt
    _ -> unexpected cx "eithers.mapList" 2,
  mkPrim _eithers_mapMaybe 2 $ \cx g args -> case args of
    [f, mt] -> DefaultEithers.mapMaybe cx g f mt
    _ -> unexpected cx "eithers.mapMaybe" 2,
  mkPrim _eithers_mapSet 2 $ \cx g args -> case args of
    [f, st] -> DefaultEithers.mapSet cx g f st
    _ -> unexpected cx "eithers.mapSet" 2,
  mkPrim _eithers_partitionEithers 1 $ \cx g args -> case args of
    [t] -> fmap (\(ls, rs) -> TermPair (TermList ls, TermList rs)) (DefaultEithers.partitionEithers cx g t)
    _ -> unexpected cx "eithers.partitionEithers" 1,
  mkPrim _eithers_rights 1 $ \cx g args -> case args of
    [t] -> DefaultEithers.rights cx g t
    _ -> unexpected cx "eithers.rights" 1]

-- ---- Equality ----

defaultLibEquality :: Library
defaultLibEquality = standardLibrary _hydra_lib_equality [
  mkPrim _equality_identity 1 $ \cx g args -> case args of
    [x] -> coerceError $ DefaultEquality.identity cx g x
    _ -> unexpected cx "equality.identity" 1,
  mkPrim _equality_max 2 $ \cx g args -> case args of
    [x, y] -> coerceError $ DefaultEquality.max cx g x y
    _ -> unexpected cx "equality.max" 2,
  mkPrim _equality_min 2 $ \cx g args -> case args of
    [x, y] -> coerceError $ DefaultEquality.min cx g x y
    _ -> unexpected cx "equality.min" 2]

-- ---- Lists ----

defaultLibLists :: Library
defaultLibLists = standardLibrary _hydra_lib_lists [
  mkPrim _lists_apply 2 $ \cx g args -> case args of
    [ft, xt] -> DefaultLists.apply cx g ft xt
    _ -> unexpected cx "lists.apply" 2,
  mkPrim _lists_bind 2 $ \cx g args -> case args of
    [lt, f] -> DefaultLists.bind cx g lt f
    _ -> unexpected cx "lists.bind" 2,
  mkPrim _lists_concat2 2 $ \cx g args -> case args of
    [l1, l2] -> coerceError $ DefaultLists.concat2 cx g l1 l2
    _ -> unexpected cx "lists.concat2" 2,
  mkPrim _lists_dropWhile 2 $ \cx g args -> case args of
    [p, lt] -> coerceError $ DefaultLists.dropWhile cx g p lt
    _ -> unexpected cx "lists.dropWhile" 2,
  mkPrim _lists_elem 2 $ \cx g args -> case args of
    [x, lt] -> coerceError $ DefaultLists.elem cx g x lt
    _ -> unexpected cx "lists.elem" 2,
  mkPrim _lists_filter 2 $ \cx g args -> case args of
    [p, lt] -> DefaultLists.filter cx g p lt
    _ -> unexpected cx "lists.filter" 2,
  mkPrim _lists_find 2 $ \cx g args -> case args of
    [p, lt] -> coerceError $ DefaultLists.find cx g p lt
    _ -> unexpected cx "lists.find" 2,
  mkPrim _lists_foldl 3 $ \cx g args -> case args of
    [f, init_, lt] -> DefaultLists.foldl cx g f init_ lt
    _ -> unexpected cx "lists.foldl" 3,
  mkPrim _lists_foldr 3 $ \cx g args -> case args of
    [f, init_, lt] -> DefaultLists.foldr cx g f init_ lt
    _ -> unexpected cx "lists.foldr" 3,
  mkPrim _lists_group 1 $ \cx g args -> case args of
    [lt] -> DefaultLists.group cx g lt
    _ -> unexpected cx "lists.group" 1,
  mkPrim _lists_intercalate 2 $ \cx g args -> case args of
    [sep, xss] -> coerceError $ DefaultLists.intercalate cx g sep xss
    _ -> unexpected cx "lists.intercalate" 2,
  mkPrim _lists_intersperse 2 $ \cx g args -> case args of
    [sep, lt] -> DefaultLists.intersperse cx g sep lt
    _ -> unexpected cx "lists.intersperse" 2,
  mkPrim _lists_map 2 $ \cx g args -> case args of
    [f, lt] -> DefaultLists.map cx g f lt
    _ -> unexpected cx "lists.map" 2,
  mkPrim _lists_maybeHead 1 $ \cx g args -> case args of
    [lt] -> DefaultLists.maybeHead cx g lt
    _ -> unexpected cx "lists.maybeHead" 1,
  mkPrim _lists_nub 1 $ \cx g args -> case args of
    [lt] -> DefaultLists.nub cx g lt
    _ -> unexpected cx "lists.nub" 1,
  mkPrim _lists_partition 2 $ \cx g args -> case args of
    [p, lt] -> DefaultLists.partition cx g p lt
    _ -> unexpected cx "lists.partition" 2,
  mkPrim _lists_pure 1 $ \cx g args -> case args of
    [x] -> coerceError $ DefaultLists.pure cx g x
    _ -> unexpected cx "lists.pure" 1,
  mkPrim _lists_replicate 2 $ \cx g args -> case args of
    [n, x] -> coerceError $ DefaultLists.replicate cx g n x
    _ -> unexpected cx "lists.replicate" 2,
  mkPrim _lists_singleton 1 $ \cx g args -> case args of
    [x] -> coerceError $ DefaultLists.singleton cx g x
    _ -> unexpected cx "lists.singleton" 1,
  mkPrim _lists_sort 1 $ \cx g args -> case args of
    [lt] -> coerceError $ DefaultLists.sort cx g lt
    _ -> unexpected cx "lists.sort" 1,
  mkPrim _lists_sortOn 2 $ \cx g args -> case args of
    [proj, lt] -> DefaultLists.sortOn cx g proj lt
    _ -> unexpected cx "lists.sortOn" 2,
  mkPrim _lists_span 2 $ \cx g args -> case args of
    [p, lt] -> DefaultLists.span cx g p lt
    _ -> unexpected cx "lists.span" 2,
  mkPrim _lists_zipWith 3 $ \cx g args -> case args of
    [f, lt1, lt2] -> DefaultLists.zipWith cx g f lt1 lt2
    _ -> unexpected cx "lists.zipWith" 3]

-- ---- Maps ----

defaultLibMaps :: Library
defaultLibMaps = standardLibrary _hydra_lib_maps [
  mkPrim _maps_alter 3 $ \cx g args -> case args of
    [f, k, mt] -> DefaultMaps.alter cx g f k mt
    _ -> unexpected cx "maps.alter" 3,
  mkPrim _maps_bimap 3 $ \cx g args -> case args of
    [kf, vf, mt] -> DefaultMaps.bimap cx g kf vf mt
    _ -> unexpected cx "maps.bimap" 3,
  mkPrim _maps_filter 2 $ \cx g args -> case args of
    [p, mt] -> DefaultMaps.filter cx g p mt
    _ -> unexpected cx "maps.filter" 2,
  mkPrim _maps_filterWithKey 2 $ \cx g args -> case args of
    [p, mt] -> DefaultMaps.filterWithKey cx g p mt
    _ -> unexpected cx "maps.filterWithKey" 2,
  mkPrim _maps_findWithDefault 3 $ \cx g args -> case args of
    [def, k, mt] -> coerceError $ DefaultMaps.findWithDefault cx g def k mt
    _ -> unexpected cx "maps.findWithDefault" 3,
  mkPrim _maps_map 2 $ \cx g args -> case args of
    [vf, mt] -> DefaultMaps.map cx g vf mt
    _ -> unexpected cx "maps.map" 2,
  mkPrim _maps_mapKeys 2 $ \cx g args -> case args of
    [kf, mt] -> DefaultMaps.mapKeys cx g kf mt
    _ -> unexpected cx "maps.mapKeys" 2]

-- ---- Math ----

defaultLibMath :: Library
defaultLibMath = standardLibrary _hydra_lib_math [
  mkPrim _math_even 1 $ \cx g args -> case args of
    [x] -> coerceError $ DefaultMath.even cx g x
    _ -> unexpected cx "math.even" 1,
  mkPrim _math_odd 1 $ \cx g args -> case args of
    [x] -> coerceError $ DefaultMath.odd cx g x
    _ -> unexpected cx "math.odd" 1]

-- ---- Maybes ----

defaultLibMaybes :: Library
defaultLibMaybes = standardLibrary _hydra_lib_maybes [
  mkPrim _maybes_apply 2 $ \cx g args -> case args of
    [ft, xt] -> DefaultMaybes.apply cx g ft xt
    _ -> unexpected cx "maybes.apply" 2,
  mkPrim _maybes_bind 2 $ \cx g args -> case args of
    [t, f] -> DefaultMaybes.bind cx g t f
    _ -> unexpected cx "maybes.bind" 2,
  mkPrim _maybes_cases 3 $ \cx g args -> case args of
    [t, def, f] -> DefaultMaybes.cases cx g t def f
    _ -> unexpected cx "maybes.cases" 3,
  mkPrim _maybes_cat 1 $ \cx g args -> case args of
    [t] -> fmap TermList (DefaultMaybes.cat cx g t)
    _ -> unexpected cx "maybes.cat" 1,
  mkPrim _maybes_compose 3 $ \cx g args -> case args of
    [f, g_, x] -> coerceError $ DefaultMaybes.compose cx g f g_ x
    _ -> unexpected cx "maybes.compose" 3,
  mkPrim _maybes_fromMaybe 2 $ \cx g args -> case args of
    [def, t] -> DefaultMaybes.fromMaybe cx g def t
    _ -> unexpected cx "maybes.fromMaybe" 2,
  mkPrim _maybes_isJust 1 $ \cx g args -> case args of
    [t] -> DefaultMaybes.isJust cx g t
    _ -> unexpected cx "maybes.isJust" 1,
  mkPrim _maybes_isNothing 1 $ \cx g args -> case args of
    [t] -> DefaultMaybes.isNothing cx g t
    _ -> unexpected cx "maybes.isNothing" 1,
  mkPrim _maybes_map 2 $ \cx g args -> case args of
    [f, t] -> DefaultMaybes.map cx g f t
    _ -> unexpected cx "maybes.map" 2,
  mkPrim _maybes_mapMaybe 2 $ \cx g args -> case args of
    [f, t] -> DefaultMaybes.mapMaybe cx g f t
    _ -> unexpected cx "maybes.mapMaybe" 2,
  mkPrim _maybes_maybe 3 $ \cx g args -> case args of
    [def, f, t] -> DefaultMaybes.maybe cx g def f t
    _ -> unexpected cx "maybes.maybe" 3,
  mkPrim _maybes_pure 1 $ \cx g args -> case args of
    [x] -> coerceError $ DefaultMaybes.pure cx g x
    _ -> unexpected cx "maybes.pure" 1,
  mkPrim _maybes_toList 1 $ \cx g args -> case args of
    [t] -> DefaultMaybes.toList cx g t
    _ -> unexpected cx "maybes.toList" 1]

-- ---- Pairs ----

defaultLibPairs :: Library
defaultLibPairs = standardLibrary _hydra_lib_pairs [
  mkPrim _pairs_bimap 3 $ \cx g args -> case args of
    [ff, sf, p] -> DefaultPairs.bimap cx g ff sf p
    _ -> unexpected cx "pairs.bimap" 3,
  mkPrim _pairs_first 1 $ \cx g args -> case args of
    [p] -> DefaultPairs.first cx g p
    _ -> unexpected cx "pairs.first" 1,
  mkPrim _pairs_second 1 $ \cx g args -> case args of
    [p] -> DefaultPairs.second cx g p
    _ -> unexpected cx "pairs.second" 1]

-- ---- Sets ----

defaultLibSets :: Library
defaultLibSets = standardLibrary _hydra_lib_sets [
  mkPrim _sets_difference 2 $ \cx g args -> case args of
    [s1, s2] -> DefaultSets.difference cx g s1 s2
    _ -> unexpected cx "sets.difference" 2,
  mkPrim _sets_intersection 2 $ \cx g args -> case args of
    [s1, s2] -> DefaultSets.intersection cx g s1 s2
    _ -> unexpected cx "sets.intersection" 2,
  mkPrim _sets_map 2 $ \cx g args -> case args of
    [f, st] -> DefaultSets.map cx g f st
    _ -> unexpected cx "sets.map" 2,
  mkPrim _sets_union 2 $ \cx g args -> case args of
    [s1, s2] -> DefaultSets.union cx g s1 s2
    _ -> unexpected cx "sets.union" 2,
  mkPrim _sets_unions 1 $ \cx g args -> case args of
    [lt] -> DefaultSets.unions cx g lt
    _ -> unexpected cx "sets.unions" 1]

-- | Error helper for wrong argument count
unexpected :: InferenceContext -> String -> Int -> Either Error Term
unexpected _cx name arity = Left $ ErrorOther (OtherError (name ++ ": expected " ++ show arity ++ " args"))
