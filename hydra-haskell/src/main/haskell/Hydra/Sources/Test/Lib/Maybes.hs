module Hydra.Sources.Test.Lib.Maybes where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import Hydra.Sources.Libraries


ns :: Namespace
ns = Namespace "hydra.test.lib.maybes"

module_ :: Module
module_ = Module ns elements [] [] $
    Just "Test cases for hydra.lib.maybes primitives"
  where
    elements = [Phantoms.toBinding allTests]

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
  test "wraps string" (string "hello") (Core.termMaybe $ just (string "hello"))]
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
      ] (list $ fmap int32 expected)

maybesMap :: TTerm TestGroup
maybesMap = subgroup "map" [
  test "maps just value" (justInt32 5) (justInt32 10),
  test "nothing unchanged" nothingTerm nothingTerm]
  where
    test name x expected = primCase name _maybes_map [
      lambda "x" (primitive _math_mul @@ var "x" @@ int32 2),
      x] expected

maybesApply :: TTerm TestGroup
maybesApply = subgroup "apply" [
  test "both just" (Core.termMaybe $ just (primitive _math_add @@ int32 3)) (justInt32 5) (justInt32 8),
  test "nothing function" nothingTerm (justInt32 5) nothingTerm,
  test "nothing value" (Core.termMaybe $ just (primitive _math_add @@ int32 3)) nothingTerm nothingTerm]
  where
    test name f x expected = primCase name _maybes_apply [f, x] expected

maybesBind :: TTerm TestGroup
maybesBind = subgroup "bind" [
  test "just to just" (justInt32 5) (justInt32 10),
  test "nothing to nothing" nothingTerm nothingTerm]
  where
    test name x expected = primCase name _maybes_bind [
      x,
      lambda "x" (Core.termMaybe $ just (primitive _math_mul @@ var "x" @@ int32 2))] expected

maybesCases :: TTerm TestGroup
maybesCases = subgroup "cases" [
  test "just applies function" (justInt32 5) (int32 0) (int32 10),
  test "nothing returns default" nothingTerm (int32 99) (int32 99)]
  where
    test name opt def expected = primCase name _maybes_cases [
      opt,
      def,
      lambda "x" (primitive _math_mul @@ var "x" @@ int32 2)] expected

maybesFromJust :: TTerm TestGroup
maybesFromJust = subgroup "fromJust" [
  test "extract from just" (justInt32 42) (int32 42)]
  where
    test name x expected = primCase name _maybes_fromJust [x] expected

maybesMapMaybe :: TTerm TestGroup
maybesMapMaybe = subgroup "mapMaybe" [
  test "filter and transform" [1, 2, 3, 4, 5] [6, 8, 10],  -- x > 2: [3,4,5] doubled = [6,8,10]
  test "empty result" [1, 2] [],
  test "empty input" [] []]
  where
    -- Function that returns Just (x*2) if x > 2, else Nothing
    filterFn = lambda "x" (
      primitive _logic_ifElse
        @@ (primitive _equality_gt @@ var "x" @@ int32 2)
        @@ (Core.termMaybe $ just (primitive _math_mul @@ var "x" @@ int32 2))
        @@ nothingTerm)
    test name xs expected = primCase name _maybes_mapMaybe [
      filterFn,
      list $ Prelude.map int32 xs] (list $ Prelude.map int32 expected)

-- | Test cases for compose: Kleisli composition of two Maybe-returning functions
-- compose f g x = bind (f x) g
maybesCompose :: TTerm TestGroup
maybesCompose = subgroup "compose" [
  test "both succeed" 5 12,           -- f(5)=Just 6, g(6)=Just 12
  testFails "first fails" 10,         -- f(10)=Nothing (x > 5), so result is Nothing
  testFails "second fails" 3]         -- f(3)=Just 4, g(4)=Nothing (y < 5), so result is Nothing
  where
    -- f: x -> if x <= 5 then Just (x + 1) else Nothing
    funF = lambda "x" (
      primitive _logic_ifElse
        @@ (primitive _equality_lte @@ var "x" @@ int32 5)
        @@ (Core.termMaybe $ just (primitive _math_add @@ var "x" @@ int32 1))
        @@ nothingTerm)
    -- g: y -> if y >= 5 then Just (y * 2) else Nothing
    funG = lambda "y" (
      primitive _logic_ifElse
        @@ (primitive _equality_gte @@ var "y" @@ int32 5)
        @@ (Core.termMaybe $ just (primitive _math_mul @@ var "y" @@ int32 2))
        @@ nothingTerm)
    -- compose f g x computes bind (f x) g
    test name input expected = primCase name _maybes_compose [funF, funG, int32 input] (justInt32 expected)
    testFails name input = primCase name _maybes_compose [funF, funG, int32 input] nothingTerm

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.maybes primitives" $
    supergroup "hydra.lib.maybes primitives" [
      maybesApply,
      maybesBind,
      maybesCases,
      maybesCat,
      maybesCompose,
      maybesFromJust,
      maybesFromMaybe,
      maybesIsJust,
      maybesIsNothing,
      maybesMap,
      maybesMapMaybe,
      maybesMaybe,
      maybesPure]
