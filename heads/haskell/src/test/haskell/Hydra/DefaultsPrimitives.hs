-- | Default-library primitive adapters. Wraps the generated default library functions
--   (from Hydra.Lib.Defaults.*) into Primitive values for the test infrastructure.
--   The generated code is the source of truth; this module only adapts calling conventions.

module Hydra.DefaultsPrimitives (
  defaultLibraries,
) where

import Hydra.Kernel
import Hydra.Sources.Libraries
import Hydra.Dsl.Prims as Prims

import qualified Hydra.Lib.Defaults.Lists as DefaultLists
import qualified Hydra.Lib.Defaults.Maybes as DefaultMaybes
import qualified Hydra.Lib.Defaults.Pairs as DefaultPairs


-- | All default-library implementations, to be used as replacements for standard libraries in test mode.
defaultLibraries :: [Library]
defaultLibraries = [
  defaultLibLists,
  defaultLibMaybes,
  defaultLibPairs]

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

-- ---- Maybes ----

defaultLibMaybes :: Library
defaultLibMaybes = standardLibrary _hydra_lib_maybes [
  mkPrim _maybes_cases 3 $ \cx g args -> case args of
    [t, def, f] -> DefaultMaybes.cases cx g t def f
    _ -> unexpected cx "maybes.cases" 3,
  mkPrim _maybes_maybe 3 $ \cx g args -> case args of
    [def, f, t] -> DefaultMaybes.maybe cx g def f t
    _ -> unexpected cx "maybes.maybe" 3]

-- ---- Pairs ----

defaultLibPairs :: Library
defaultLibPairs = standardLibrary _hydra_lib_pairs [
  mkPrim _pairs_first 1 $ \cx g args -> case args of
    [p] -> DefaultPairs.first cx g p
    _ -> unexpected cx "pairs.first" 1,
  mkPrim _pairs_second 1 $ \cx g args -> case args of
    [p] -> DefaultPairs.second cx g p
    _ -> unexpected cx "pairs.second" 1]


-- | Error helper for wrong argument count
unexpected :: InferenceContext -> String -> Int -> Either Error Term
unexpected _cx name arity = Left $ ErrorOther (OtherError (name ++ ": expected " ++ show arity ++ " args"))
