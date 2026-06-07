{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Test.Lib.Optionals where

-- Standard imports for tests
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms hiding ((@@))
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import           Hydra.Dsl.Meta.Phantoms                ((@@))
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic as Logic
import qualified Hydra.Dsl.Meta.Lib.Math as Math
import qualified Hydra.Dsl.Meta.Lib.Optionals as Optionals
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


ns :: ModuleName
ns = ModuleName "hydra.test.lib.optionals"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([ModuleName "hydra.reduction", ShowCore.ns] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.lib.optionals primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]


-- Phantom-typed helpers
justInt :: Int -> TypedTerm (Maybe Int)
justInt x = Phantoms.just (Phantoms.int32 x)

nothingInt :: TypedTerm (Maybe Int)
nothingInt = Phantoms.nothing

showBool :: TypedTerm (Bool -> String)
showBool = Phantoms.lambda "b" $ Literals.showBoolean (Phantoms.var "b")

showInt32 :: TypedTerm (Int -> String)
showInt32 = Phantoms.lambda "n" $ Literals.showInt32 (Phantoms.var "n")

showIntList :: TypedTerm ([Int] -> String)
showIntList = Phantoms.lambda "xs" $ ShowCore.list_ @@ showInt32 @@ Phantoms.var "xs"

showMaybeInt :: TypedTerm (Maybe Int -> String)
showMaybeInt = Phantoms.lambda "mx" $ ShowCore.optional_ @@ showInt32 @@ Phantoms.var "mx"

showMaybeString :: TypedTerm (Maybe String -> String)
showMaybeString = Phantoms.lambda "mx" $ ShowCore.optional_ @@ (Phantoms.lambda "s" $ Literals.showString (Phantoms.var "s")) @@ Phantoms.var "mx"

-- Test groups

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.optionals primitives" $
    supergroup "hydra.lib.optionals primitives" [
      optionalsApply,
      optionalsBind,
      optionalsCases,
      optionalsCat,
      optionalsCompose,
      optionalsFromOptional,
      optionalsIsGiven,
      optionalsIsNone,
      optionalsMap,
      optionalsMapOptional,
      optionalsPure,
      optionalsToList]

optionalsApply :: TypedTerm TestGroup
optionalsApply = subgroup "apply" [
  test "both just" (Phantoms.just (Phantoms.lambda "x" $ Math.add (Phantoms.int32 3) (Phantoms.var "x"))) (justInt 5) (justInt 8),
  test "nothing function" (Phantoms.nothing :: TypedTerm (Maybe (Int -> Int))) (justInt 5) nothingInt,
  test "nothing value" (Phantoms.just (Phantoms.lambda "x" $ Math.add (Phantoms.int32 3) (Phantoms.var "x"))) nothingInt nothingInt]
  where
    test name f x expected = evalPair name showMaybeInt
      (Optionals.apply f x)
      expected

optionalsBind :: TypedTerm TestGroup
optionalsBind = subgroup "bind" [
  test "just to just" (justInt 5) (justInt 10),
  test "nothing to nothing" nothingInt nothingInt]
  where
    test name x expected = evalPair name showMaybeInt
      (Optionals.bind x (Phantoms.lambda "x" $ Phantoms.just (Math.mul (Phantoms.var "x") (Phantoms.int32 2))))
      expected

optionalsCases :: TypedTerm TestGroup
optionalsCases = subgroup "cases" [
  test "just applies function" (justInt 5) 0 10,
  test "nothing returns default" nothingInt 99 99]
  where
    test name opt def expected = evalPair name showInt32
      (Optionals.cases opt (Phantoms.int32 def) (Phantoms.lambda "x" $ Math.mul (Phantoms.var "x") (Phantoms.int32 2)))
      (Phantoms.int32 expected)

optionalsCat :: TypedTerm TestGroup
optionalsCat = subgroup "cat" [
  test "filters nothings" [justInt 1, nothingInt, justInt 2] [1, 2],
  test "all justs" [justInt 1, justInt 2] [1, 2],
  test "all nothings" [nothingInt, nothingInt] [],
  test "empty list" ([] :: [TypedTerm (Maybe Int)]) []]
  where
    test name input expected = evalPair name showIntList
      (Optionals.cat (Phantoms.list input))
      (Phantoms.list (Phantoms.int32 <$> expected))

optionalsCompose :: TypedTerm TestGroup
optionalsCompose = subgroup "compose" [
  test "both succeed" 5 (justInt 12),
  testFails "first fails" 10,
  testFails "second fails" 3]
  where
    -- f: x -> if x <= 5 then Just (x + 1) else Nothing
    funF = Phantoms.lambda "x" $
      Logic.ifElse (Equality.lte (Phantoms.var "x") (Phantoms.int32 5))
        (Phantoms.just (Math.add (Phantoms.var "x") (Phantoms.int32 1)))
        (Phantoms.nothing :: TypedTerm (Maybe Int))
    -- g: y -> if y >= 5 then Just (y * 2) else Nothing
    funG = Phantoms.lambda "y" $
      Logic.ifElse (Equality.gte (Phantoms.var "y") (Phantoms.int32 5))
        (Phantoms.just (Math.mul (Phantoms.var "y") (Phantoms.int32 2)))
        (Phantoms.nothing :: TypedTerm (Maybe Int))
    test name input expected = evalPair name showMaybeInt
      (Optionals.compose funF funG @@ Phantoms.int32 input)
      expected
    testFails name input = evalPair name showMaybeInt
      (Optionals.compose funF funG @@ Phantoms.int32 input)
      nothingInt

optionalsFromOptional :: TypedTerm TestGroup
optionalsFromOptional = subgroup "fromOptional" [
  test "just value" 0 (justInt 42) 42,
  test "nothing with default" 99 nothingInt 99]
  where
    test name def x result = evalPair name showInt32
      (Optionals.fromOptional (Phantoms.int32 def) x)
      (Phantoms.int32 result)

optionalsIsGiven :: TypedTerm TestGroup
optionalsIsGiven = subgroup "isGiven" [
  test "just value" (justInt 42) True,
  test "nothing" nothingInt False]
  where
    test name x result = evalPair name showBool
      (Optionals.isGiven x)
      (Phantoms.boolean result)

optionalsIsNone :: TypedTerm TestGroup
optionalsIsNone = subgroup "isNone" [
  test "just value" (justInt 42) False,
  test "nothing" nothingInt True]
  where
    test name x result = evalPair name showBool
      (Optionals.isNone x)
      (Phantoms.boolean result)

optionalsMap :: TypedTerm TestGroup
optionalsMap = subgroup "map" [
  test "maps just value" (justInt 5) (justInt 10),
  test "nothing unchanged" nothingInt nothingInt]
  where
    test name x expected = evalPair name showMaybeInt
      (Optionals.map (Phantoms.lambda "x" $ Math.mul (Phantoms.var "x") (Phantoms.int32 2)) x)
      expected

optionalsMapOptional :: TypedTerm TestGroup
optionalsMapOptional = subgroup "mapOptional" [
  test "filter and transform" [1, 2, 3, 4, 5] [6, 8, 10],
  test "empty result" [1, 2] [],
  test "empty input" [] []]
  where
    filterFn = Phantoms.lambda "x" $
      Logic.ifElse (Equality.gt (Phantoms.var "x") (Phantoms.int32 2))
        (Phantoms.just (Math.mul (Phantoms.var "x") (Phantoms.int32 2)))
        (Phantoms.nothing :: TypedTerm (Maybe Int))
    test name xs expected = evalPair name showIntList
      (Optionals.mapOptional filterFn (Phantoms.list $ Phantoms.int32 <$> xs))
      (Phantoms.list $ Phantoms.int32 <$> expected)

optionalsPure :: TypedTerm TestGroup
optionalsPure = subgroup "pure" [
  testInt "wraps integer" 42 42,
  testStr "wraps string" "hello" "hello"]
  where
    testInt name x expected = evalPair name showMaybeInt
      (Optionals.pure (Phantoms.int32 x))
      (justInt expected)
    testStr name x expected = evalPair name showMaybeString
      (Optionals.pure (Phantoms.string x))
      (Phantoms.just (Phantoms.string expected))

optionalsToList :: TypedTerm TestGroup
optionalsToList = subgroup "toList" [
  test "just value" (justInt 42) [42],
  test "nothing" nothingInt []]
  where
    test name x expected = evalPair name showIntList
      (Optionals.toList x)
      (Phantoms.list $ Phantoms.int32 <$> expected)
