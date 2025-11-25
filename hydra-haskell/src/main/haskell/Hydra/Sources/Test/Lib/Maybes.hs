module Hydra.Sources.Test.Lib.Maybes where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Types.Testing as TestingTypes
import qualified Hydra.Sources.Test.TestGraph as TestGraph


module_ :: Module
module_ = Module (Namespace "hydra.test.lib.maybes") elements [] [] $
    Just "Test cases for hydra.lib.maybes primitives"
  where
    elements = [el allTestsDef]

-- Helper to create Just terms for Int32 values
justInt32 :: Int -> TTerm Term
justInt32 x = Core.termMaybe $ just (int32 x)

-- Helper to create Nothing terms
nothingTerm :: TTerm Term
nothingTerm = Core.termMaybe nothing

-- Test groups for hydra.lib.maybes primitives

maybesIsJust :: TTerm TestGroup
maybesIsJust = subgroup "isJust" [
  test "just value" (justInt32 42) true,
  test "nothing" nothingTerm false]
  where
    test name x result = primCase name _maybes_isJust [x] result

maybesIsNothing :: TTerm TestGroup
maybesIsNothing = subgroup "isNothing" [
  test "just value" (justInt32 42) false,
  test "nothing" nothingTerm true]
  where
    test name x result = primCase name _maybes_isNothing [x] result

maybesFromMaybe :: TTerm TestGroup
maybesFromMaybe = subgroup "fromMaybe" [
  test "just value" 0 (justInt32 42) 42,
  test "nothing with default" 99 nothingTerm 99]
  where
    test name def x result = primCase name _maybes_fromMaybe [int32 def, x] (int32 result)

maybesMaybe :: TTerm TestGroup
maybesMaybe = subgroup "maybe" [
  test "just value applies function" 0 (justInt32 5) 10,
  test "nothing returns default" 99 nothingTerm 99]
  where
    test name def x result = primCase name _maybes_maybe [
      int32 def,
      lambda "x" (primitive _math_mul @@ var "x" @@ int32 2),
      x] (int32 result)

maybesPure :: TTerm TestGroup
maybesPure = subgroup "pure" [
  test "wraps integer" (int32 42) (justInt32 42),
  test "wraps string" (MetaTerms.string "hello") (Core.termMaybe $ just (MetaTerms.string "hello"))]
  where
    test name x expected = primCase name _maybes_pure [x] expected

maybesCat :: TTerm TestGroup
maybesCat = subgroup "cat" [
  test "filters nothings" [justInt32 1, nothingTerm, justInt32 2] [1, 2],
  test "all justs" [justInt32 1, justInt32 2] [1, 2],
  test "all nothings" [nothingTerm, nothingTerm] [],
  test "empty list" [] []]
  where
    test name input expected = primCase name _maybes_cat [
      list input
      ] (list $ Prelude.map int32 expected)

maybesMap :: TTerm TestGroup
maybesMap = subgroup "map" [
  test "maps just value" (justInt32 5) (justInt32 10),
  test "nothing unchanged" nothingTerm nothingTerm]
  where
    test name x expected = primCase name _maybes_map [
      lambda "x" (primitive _math_mul @@ var "x" @@ int32 2),
      x] expected

allTestsDef :: TBinding TestGroup
allTestsDef = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.maybes primitives" $
    supergroup "hydra.lib.maybes primitives" [
      maybesIsJust,
      maybesIsNothing,
      maybesFromMaybe,
      maybesMaybe,
      maybesPure,
      maybesCat,
      maybesMap]
