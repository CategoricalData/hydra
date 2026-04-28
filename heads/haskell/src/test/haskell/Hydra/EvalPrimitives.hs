-- | Eval-mode primitive adapters. Wraps the generated eval library functions
--   (from Hydra.Eval.Lib.*) into Primitive values for the test infrastructure.
--   The generated code is the source of truth; this module only adapts calling conventions.

module Hydra.EvalPrimitives (
  evalLibraries,
) where

import Hydra.Kernel
import Hydra.Sources.Libraries
import Hydra.Dsl.Prims as Prims

import qualified Hydra.Eval.Lib.Eithers as EvalEithers
import qualified Hydra.Eval.Lib.Equality as EvalEquality
import qualified Hydra.Eval.Lib.Lists as EvalLists
import qualified Hydra.Eval.Lib.Logic as EvalLogic
import qualified Hydra.Eval.Lib.Maps as EvalMaps
import qualified Hydra.Eval.Lib.Math as EvalMath
import qualified Hydra.Eval.Lib.Maybes as EvalMaybes
import qualified Hydra.Eval.Lib.Pairs as EvalPairs
import qualified Hydra.Eval.Lib.Sets as EvalSets


-- | All eval-mode libraries, to be used as replacements for standard libraries in test mode.
evalLibraries :: [Library]
evalLibraries = [
  evalLibEithers,
  evalLibEquality,
  evalLibLists,
  evalLibLogic,
  evalLibMaps,
  evalLibMath,
  evalLibMaybes,
  evalLibPairs,
  evalLibSets]

-- Helpers

mkPrim :: Name -> Int -> (Context -> Graph -> [Term] -> Either Error Term) -> Primitive
mkPrim name arity impl = Primitive {
  primitiveName = name,
  primitiveTypeScheme = dummyType arity,
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

-- | Bridge: convert InContext Error to Error (temporary until gen-main is regenerated)
dropCtx :: Either (InContext Error) Term -> Either Error Term
dropCtx (Left ic) = Left (inContextObject ic)
dropCtx (Right t) = Right t

-- ---- Eithers ----

evalLibEithers :: Library
evalLibEithers = standardLibrary _hydra_lib_eithers [
  mkPrim _eithers_bimap 3 $ \cx g args -> case args of
    [lf, rf, t] -> EvalEithers.bimap cx g lf rf t
    _ -> unexpected cx "eithers.bimap" 3,
  mkPrim _eithers_bind 2 $ \cx g args -> case args of
    [t, f] -> EvalEithers.bind cx g t f
    _ -> unexpected cx "eithers.bind" 2,
  mkPrim _eithers_either 3 $ \cx g args -> case args of
    [lf, rf, t] -> EvalEithers.either cx g lf rf t
    _ -> unexpected cx "eithers.either" 3,
  mkPrim _eithers_foldl 3 $ \cx g args -> case args of
    [f, init_, lt] -> EvalEithers.foldl cx g f init_ lt
    _ -> unexpected cx "eithers.foldl" 3,
  mkPrim _eithers_fromLeft 2 $ \cx g args -> case args of
    [def, t] -> EvalEithers.fromLeft cx g def t
    _ -> unexpected cx "eithers.fromLeft" 2,
  mkPrim _eithers_fromRight 2 $ \cx g args -> case args of
    [def, t] -> EvalEithers.fromRight cx g def t
    _ -> unexpected cx "eithers.fromRight" 2,
  mkPrim _eithers_isLeft 1 $ \cx g args -> case args of
    [t] -> EvalEithers.isLeft cx g t
    _ -> unexpected cx "eithers.isLeft" 1,
  mkPrim _eithers_isRight 1 $ \cx g args -> case args of
    [t] -> EvalEithers.isRight cx g t
    _ -> unexpected cx "eithers.isRight" 1,
  mkPrim _eithers_lefts 1 $ \cx g args -> case args of
    [t] -> EvalEithers.lefts cx g t
    _ -> unexpected cx "eithers.lefts" 1,
  mkPrim _eithers_map 2 $ \cx g args -> case args of
    [f, t] -> EvalEithers.map cx g f t
    _ -> unexpected cx "eithers.map" 2,
  mkPrim _eithers_mapList 2 $ \cx g args -> case args of
    [f, lt] -> EvalEithers.mapList cx g f lt
    _ -> unexpected cx "eithers.mapList" 2,
  mkPrim _eithers_mapMaybe 2 $ \cx g args -> case args of
    [f, mt] -> EvalEithers.mapMaybe cx g f mt
    _ -> unexpected cx "eithers.mapMaybe" 2,
  mkPrim _eithers_mapSet 2 $ \cx g args -> case args of
    [f, st] -> EvalEithers.mapSet cx g f st
    _ -> unexpected cx "eithers.mapSet" 2,
  mkPrim _eithers_partitionEithers 1 $ \cx g args -> case args of
    [t] -> fmap (\(ls, rs) -> TermPair (TermList ls, TermList rs)) (EvalEithers.partitionEithers cx g t)
    _ -> unexpected cx "eithers.partitionEithers" 1,
  mkPrim _eithers_rights 1 $ \cx g args -> case args of
    [t] -> EvalEithers.rights cx g t
    _ -> unexpected cx "eithers.rights" 1]

-- ---- Equality ----

evalLibEquality :: Library
evalLibEquality = standardLibrary _hydra_lib_equality [
  mkPrim _equality_identity 1 $ \cx g args -> case args of
    [x] -> coerceError $ EvalEquality.identity cx g x
    _ -> unexpected cx "equality.identity" 1,
  mkPrim _equality_max 2 $ \cx g args -> case args of
    [x, y] -> coerceError $ EvalEquality.max cx g x y
    _ -> unexpected cx "equality.max" 2,
  mkPrim _equality_min 2 $ \cx g args -> case args of
    [x, y] -> coerceError $ EvalEquality.min cx g x y
    _ -> unexpected cx "equality.min" 2]

-- ---- Lists ----

evalLibLists :: Library
evalLibLists = standardLibrary _hydra_lib_lists [
  mkPrim _lists_apply 2 $ \cx g args -> case args of
    [ft, xt] -> EvalLists.apply cx g ft xt
    _ -> unexpected cx "lists.apply" 2,
  mkPrim _lists_bind 2 $ \cx g args -> case args of
    [lt, f] -> EvalLists.bind cx g lt f
    _ -> unexpected cx "lists.bind" 2,
  mkPrim _lists_concat2 2 $ \cx g args -> case args of
    [l1, l2] -> coerceError $ EvalLists.concat2 cx g l1 l2
    _ -> unexpected cx "lists.concat2" 2,
  mkPrim _lists_dropWhile 2 $ \cx g args -> case args of
    [p, lt] -> coerceError $ EvalLists.dropWhile cx g p lt
    _ -> unexpected cx "lists.dropWhile" 2,
  mkPrim _lists_elem 2 $ \cx g args -> case args of
    [x, lt] -> coerceError $ EvalLists.elem cx g x lt
    _ -> unexpected cx "lists.elem" 2,
  mkPrim _lists_filter 2 $ \cx g args -> case args of
    [p, lt] -> EvalLists.filter cx g p lt
    _ -> unexpected cx "lists.filter" 2,
  mkPrim _lists_find 2 $ \cx g args -> case args of
    [p, lt] -> coerceError $ EvalLists.find cx g p lt
    _ -> unexpected cx "lists.find" 2,
  mkPrim _lists_foldl 3 $ \cx g args -> case args of
    [f, init_, lt] -> EvalLists.foldl cx g f init_ lt
    _ -> unexpected cx "lists.foldl" 3,
  mkPrim _lists_foldr 3 $ \cx g args -> case args of
    [f, init_, lt] -> EvalLists.foldr cx g f init_ lt
    _ -> unexpected cx "lists.foldr" 3,
  mkPrim _lists_group 1 $ \cx g args -> case args of
    [lt] -> EvalLists.group cx g lt
    _ -> unexpected cx "lists.group" 1,
  mkPrim _lists_intercalate 2 $ \cx g args -> case args of
    [sep, xss] -> coerceError $ EvalLists.intercalate cx g sep xss
    _ -> unexpected cx "lists.intercalate" 2,
  mkPrim _lists_intersperse 2 $ \cx g args -> case args of
    [sep, lt] -> EvalLists.intersperse cx g sep lt
    _ -> unexpected cx "lists.intersperse" 2,
  mkPrim _lists_map 2 $ \cx g args -> case args of
    [f, lt] -> EvalLists.map cx g f lt
    _ -> unexpected cx "lists.map" 2,
  mkPrim _lists_maybeHead 1 $ \cx g args -> case args of
    [lt] -> EvalLists.maybeHead cx g lt
    _ -> unexpected cx "lists.maybeHead" 1,
  mkPrim _lists_nub 1 $ \cx g args -> case args of
    [lt] -> EvalLists.nub cx g lt
    _ -> unexpected cx "lists.nub" 1,
  mkPrim _lists_partition 2 $ \cx g args -> case args of
    [p, lt] -> EvalLists.partition cx g p lt
    _ -> unexpected cx "lists.partition" 2,
  mkPrim _lists_pure 1 $ \cx g args -> case args of
    [x] -> coerceError $ EvalLists.pure cx g x
    _ -> unexpected cx "lists.pure" 1,
  mkPrim _lists_replicate 2 $ \cx g args -> case args of
    [n, x] -> coerceError $ EvalLists.replicate cx g n x
    _ -> unexpected cx "lists.replicate" 2,
  mkPrim _lists_singleton 1 $ \cx g args -> case args of
    [x] -> coerceError $ EvalLists.singleton cx g x
    _ -> unexpected cx "lists.singleton" 1,
  mkPrim _lists_sort 1 $ \cx g args -> case args of
    [lt] -> coerceError $ EvalLists.sort cx g lt
    _ -> unexpected cx "lists.sort" 1,
  mkPrim _lists_sortOn 2 $ \cx g args -> case args of
    [proj, lt] -> EvalLists.sortOn cx g proj lt
    _ -> unexpected cx "lists.sortOn" 2,
  mkPrim _lists_span 2 $ \cx g args -> case args of
    [p, lt] -> EvalLists.span cx g p lt
    _ -> unexpected cx "lists.span" 2,
  mkPrim _lists_zipWith 3 $ \cx g args -> case args of
    [f, lt1, lt2] -> EvalLists.zipWith cx g f lt1 lt2
    _ -> unexpected cx "lists.zipWith" 3]

-- ---- Logic ----

evalLibLogic :: Library
evalLibLogic = standardLibrary _hydra_lib_logic [
  mkPrim _logic_and 2 $ \cx g args -> case args of
    [a, b] -> coerceError $ EvalLogic.and cx g a b
    _ -> unexpected cx "logic.and" 2,
  mkPrim _logic_not 1 $ \cx g args -> case args of
    [a] -> coerceError $ EvalLogic.not cx g a
    _ -> unexpected cx "logic.not" 1,
  mkPrim _logic_or 2 $ \cx g args -> case args of
    [a, b] -> coerceError $ EvalLogic.or cx g a b
    _ -> unexpected cx "logic.or" 2]

-- ---- Maps ----

evalLibMaps :: Library
evalLibMaps = standardLibrary _hydra_lib_maps [
  mkPrim _maps_alter 3 $ \cx g args -> case args of
    [f, k, mt] -> EvalMaps.alter cx g f k mt
    _ -> unexpected cx "maps.alter" 3,
  mkPrim _maps_bimap 3 $ \cx g args -> case args of
    [kf, vf, mt] -> EvalMaps.bimap cx g kf vf mt
    _ -> unexpected cx "maps.bimap" 3,
  mkPrim _maps_filter 2 $ \cx g args -> case args of
    [p, mt] -> EvalMaps.filter cx g p mt
    _ -> unexpected cx "maps.filter" 2,
  mkPrim _maps_filterWithKey 2 $ \cx g args -> case args of
    [p, mt] -> EvalMaps.filterWithKey cx g p mt
    _ -> unexpected cx "maps.filterWithKey" 2,
  mkPrim _maps_findWithDefault 3 $ \cx g args -> case args of
    [def, k, mt] -> coerceError $ EvalMaps.findWithDefault cx g def k mt
    _ -> unexpected cx "maps.findWithDefault" 3,
  mkPrim _maps_map 2 $ \cx g args -> case args of
    [vf, mt] -> EvalMaps.map cx g vf mt
    _ -> unexpected cx "maps.map" 2,
  mkPrim _maps_mapKeys 2 $ \cx g args -> case args of
    [kf, mt] -> EvalMaps.mapKeys cx g kf mt
    _ -> unexpected cx "maps.mapKeys" 2]

-- ---- Math ----

evalLibMath :: Library
evalLibMath = standardLibrary _hydra_lib_math [
  mkPrim _math_even 1 $ \cx g args -> case args of
    [x] -> coerceError $ EvalMath.even cx g x
    _ -> unexpected cx "math.even" 1,
  mkPrim _math_odd 1 $ \cx g args -> case args of
    [x] -> coerceError $ EvalMath.odd cx g x
    _ -> unexpected cx "math.odd" 1]

-- ---- Maybes ----

evalLibMaybes :: Library
evalLibMaybes = standardLibrary _hydra_lib_maybes [
  mkPrim _maybes_apply 2 $ \cx g args -> case args of
    [ft, xt] -> EvalMaybes.apply cx g ft xt
    _ -> unexpected cx "maybes.apply" 2,
  mkPrim _maybes_bind 2 $ \cx g args -> case args of
    [t, f] -> EvalMaybes.bind cx g t f
    _ -> unexpected cx "maybes.bind" 2,
  mkPrim _maybes_cases 3 $ \cx g args -> case args of
    [t, def, f] -> EvalMaybes.cases cx g t def f
    _ -> unexpected cx "maybes.cases" 3,
  mkPrim _maybes_cat 1 $ \cx g args -> case args of
    [t] -> fmap TermList (EvalMaybes.cat cx g t)
    _ -> unexpected cx "maybes.cat" 1,
  mkPrim _maybes_compose 3 $ \cx g args -> case args of
    [f, g_, x] -> coerceError $ EvalMaybes.compose cx g f g_ x
    _ -> unexpected cx "maybes.compose" 3,
  mkPrim _maybes_fromMaybe 2 $ \cx g args -> case args of
    [def, t] -> EvalMaybes.fromMaybe cx g def t
    _ -> unexpected cx "maybes.fromMaybe" 2,
  mkPrim _maybes_isJust 1 $ \cx g args -> case args of
    [t] -> EvalMaybes.isJust cx g t
    _ -> unexpected cx "maybes.isJust" 1,
  mkPrim _maybes_isNothing 1 $ \cx g args -> case args of
    [t] -> EvalMaybes.isNothing cx g t
    _ -> unexpected cx "maybes.isNothing" 1,
  mkPrim _maybes_map 2 $ \cx g args -> case args of
    [f, t] -> EvalMaybes.map cx g f t
    _ -> unexpected cx "maybes.map" 2,
  mkPrim _maybes_mapMaybe 2 $ \cx g args -> case args of
    [f, t] -> EvalMaybes.mapMaybe cx g f t
    _ -> unexpected cx "maybes.mapMaybe" 2,
  mkPrim _maybes_maybe 3 $ \cx g args -> case args of
    [def, f, t] -> EvalMaybes.maybe cx g def f t
    _ -> unexpected cx "maybes.maybe" 3,
  mkPrim _maybes_pure 1 $ \cx g args -> case args of
    [x] -> coerceError $ EvalMaybes.pure cx g x
    _ -> unexpected cx "maybes.pure" 1,
  mkPrim _maybes_toList 1 $ \cx g args -> case args of
    [t] -> EvalMaybes.toList cx g t
    _ -> unexpected cx "maybes.toList" 1]

-- ---- Pairs ----

evalLibPairs :: Library
evalLibPairs = standardLibrary _hydra_lib_pairs [
  mkPrim _pairs_bimap 3 $ \cx g args -> case args of
    [ff, sf, p] -> EvalPairs.bimap cx g ff sf p
    _ -> unexpected cx "pairs.bimap" 3,
  mkPrim _pairs_first 1 $ \cx g args -> case args of
    [p] -> EvalPairs.first cx g p
    _ -> unexpected cx "pairs.first" 1,
  mkPrim _pairs_second 1 $ \cx g args -> case args of
    [p] -> EvalPairs.second cx g p
    _ -> unexpected cx "pairs.second" 1]

-- ---- Sets ----

evalLibSets :: Library
evalLibSets = standardLibrary _hydra_lib_sets [
  mkPrim _sets_difference 2 $ \cx g args -> case args of
    [s1, s2] -> EvalSets.difference cx g s1 s2
    _ -> unexpected cx "sets.difference" 2,
  mkPrim _sets_intersection 2 $ \cx g args -> case args of
    [s1, s2] -> EvalSets.intersection cx g s1 s2
    _ -> unexpected cx "sets.intersection" 2,
  mkPrim _sets_map 2 $ \cx g args -> case args of
    [f, st] -> EvalSets.map cx g f st
    _ -> unexpected cx "sets.map" 2,
  mkPrim _sets_union 2 $ \cx g args -> case args of
    [s1, s2] -> EvalSets.union cx g s1 s2
    _ -> unexpected cx "sets.union" 2,
  mkPrim _sets_unions 1 $ \cx g args -> case args of
    [lt] -> EvalSets.unions cx g lt
    _ -> unexpected cx "sets.unions" 1]

-- | Error helper for wrong argument count
unexpected :: Context -> String -> Int -> Either Error Term
unexpected _cx name arity = Left $ ErrorOther (OtherError (name ++ ": expected " ++ show arity ++ " args"))
