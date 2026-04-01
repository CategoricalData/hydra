{-# LANGUAGE FlexibleContexts #-}

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
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic as Logic
import qualified Hydra.Dsl.Meta.Lib.Math as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes as Maybes
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


ns :: Namespace
ns = Namespace "hydra.test.lib.maybes"

module_ :: Module
module_ = Module ns elements
    [Namespace "hydra.reduction", ShowCore.ns]
    kernelTypesNamespaces $
    Just "Test cases for hydra.lib.maybes primitives"
  where
    elements = [Phantoms.toDefinition allTests]

(#) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TTerm b
(#) = (Phantoms.@@)
infixl 1 #

showInt32 :: TTerm (Int -> String)
showInt32 = Phantoms.lambda "n" $ Literals.showInt32 (Phantoms.var "n")

showBool :: TTerm (Bool -> String)
showBool = Phantoms.lambda "b" $ Literals.showBoolean (Phantoms.var "b")

showMaybeInt :: TTerm (Maybe Int -> String)
showMaybeInt = Phantoms.lambda "mx" $ ShowCore.maybe_ # showInt32 # Phantoms.var "mx"

showIntList :: TTerm ([Int] -> String)
showIntList = Phantoms.lambda "xs" $ ShowCore.list_ # showInt32 # Phantoms.var "xs"

showMaybeString :: TTerm (Maybe String -> String)
showMaybeString = Phantoms.lambda "mx" $ ShowCore.maybe_ # (Phantoms.lambda "s" $ Literals.showString (Phantoms.var "s")) # Phantoms.var "mx"

-- Phantom-typed helpers
justInt :: Int -> TTerm (Maybe Int)
justInt x = Phantoms.just (Phantoms.int32 x)

nothingInt :: TTerm (Maybe Int)
nothingInt = Phantoms.nothing

-- Test groups

maybesIsJust :: TTerm TestGroup
maybesIsJust = subgroup "isJust" [
  test "just value" (justInt 42) True,
  test "nothing" nothingInt False]
  where
    test name x result = evalPair name showBool
      (Maybes.isJust x)
      (Phantoms.boolean result)

maybesIsNothing :: TTerm TestGroup
maybesIsNothing = subgroup "isNothing" [
  test "just value" (justInt 42) False,
  test "nothing" nothingInt True]
  where
    test name x result = evalPair name showBool
      (Maybes.isNothing x)
      (Phantoms.boolean result)

maybesFromMaybe :: TTerm TestGroup
maybesFromMaybe = subgroup "fromMaybe" [
  test "just value" 0 (justInt 42) 42,
  test "nothing with default" 99 nothingInt 99]
  where
    test name def x result = evalPair name showInt32
      (Maybes.fromMaybe (Phantoms.int32 def) x)
      (Phantoms.int32 result)

maybesMaybe :: TTerm TestGroup
maybesMaybe = subgroup "maybe" [
  test "just value applies function" 0 (justInt 5) 10,
  test "nothing returns default" 99 nothingInt 99]
  where
    test name def x result = evalPair name showInt32
      (Maybes.maybe (Phantoms.int32 def) (Phantoms.lambda "x" $ Math.mul (Phantoms.var "x") (Phantoms.int32 2)) x)
      (Phantoms.int32 result)

maybesPure :: TTerm TestGroup
maybesPure = subgroup "pure" [
  testInt "wraps integer" 42 42,
  testStr "wraps string" "hello" "hello"]
  where
    testInt name x expected = evalPair name showMaybeInt
      (Maybes.pure (Phantoms.int32 x))
      (justInt expected)
    testStr name x expected = evalPair name showMaybeString
      (Maybes.pure (Phantoms.string x))
      (Phantoms.just (Phantoms.string expected))

maybesCat :: TTerm TestGroup
maybesCat = subgroup "cat" [
  test "filters nothings" [justInt 1, nothingInt, justInt 2] [1, 2],
  test "all justs" [justInt 1, justInt 2] [1, 2],
  test "all nothings" [nothingInt, nothingInt] [],
  test "empty list" ([] :: [TTerm (Maybe Int)]) []]
  where
    test name input expected = evalPair name showIntList
      (Maybes.cat (Phantoms.list input))
      (Phantoms.list (Phantoms.int32 <$> expected))

maybesMap :: TTerm TestGroup
maybesMap = subgroup "map" [
  test "maps just value" (justInt 5) (justInt 10),
  test "nothing unchanged" nothingInt nothingInt]
  where
    test name x expected = evalPair name showMaybeInt
      (Maybes.map (Phantoms.lambda "x" $ Math.mul (Phantoms.var "x") (Phantoms.int32 2)) x)
      expected

maybesApply :: TTerm TestGroup
maybesApply = subgroup "apply" [
  test "both just" (Phantoms.just (Phantoms.lambda "x" $ Math.add (Phantoms.int32 3) (Phantoms.var "x"))) (justInt 5) (justInt 8),
  test "nothing function" (Phantoms.nothing :: TTerm (Maybe (Int -> Int))) (justInt 5) nothingInt,
  test "nothing value" (Phantoms.just (Phantoms.lambda "x" $ Math.add (Phantoms.int32 3) (Phantoms.var "x"))) nothingInt nothingInt]
  where
    test name f x expected = evalPair name showMaybeInt
      (Maybes.apply f x)
      expected

maybesBind :: TTerm TestGroup
maybesBind = subgroup "bind" [
  test "just to just" (justInt 5) (justInt 10),
  test "nothing to nothing" nothingInt nothingInt]
  where
    test name x expected = evalPair name showMaybeInt
      (Maybes.bind x (Phantoms.lambda "x" $ Phantoms.just (Math.mul (Phantoms.var "x") (Phantoms.int32 2))))
      expected

maybesCases :: TTerm TestGroup
maybesCases = subgroup "cases" [
  test "just applies function" (justInt 5) 0 10,
  test "nothing returns default" nothingInt 99 99]
  where
    test name opt def expected = evalPair name showInt32
      (Maybes.cases opt (Phantoms.int32 def) (Phantoms.lambda "x" $ Math.mul (Phantoms.var "x") (Phantoms.int32 2)))
      (Phantoms.int32 expected)

maybesFromJust :: TTerm TestGroup
maybesFromJust = subgroup "fromJust" [
  test "extract from just" (justInt 42) 42]
  where
    test name x expected = evalPair name showInt32
      (Maybes.fromJust x)
      (Phantoms.int32 expected)

maybesMapMaybe :: TTerm TestGroup
maybesMapMaybe = subgroup "mapMaybe" [
  test "filter and transform" [1, 2, 3, 4, 5] [6, 8, 10],
  test "empty result" [1, 2] [],
  test "empty input" [] []]
  where
    filterFn = Phantoms.lambda "x" $
      Logic.ifElse (Equality.gt (Phantoms.var "x") (Phantoms.int32 2))
        (Phantoms.just (Math.mul (Phantoms.var "x") (Phantoms.int32 2)))
        (Phantoms.nothing :: TTerm (Maybe Int))
    test name xs expected = evalPair name showIntList
      (Maybes.mapMaybe filterFn (Phantoms.list $ Phantoms.int32 <$> xs))
      (Phantoms.list $ Phantoms.int32 <$> expected)

maybesCompose :: TTerm TestGroup
maybesCompose = subgroup "compose" [
  test "both succeed" 5 (justInt 12),
  testFails "first fails" 10,
  testFails "second fails" 3]
  where
    -- f: x -> if x <= 5 then Just (x + 1) else Nothing
    funF = Phantoms.lambda "x" $
      Logic.ifElse (Equality.lte (Phantoms.var "x") (Phantoms.int32 5))
        (Phantoms.just (Math.add (Phantoms.var "x") (Phantoms.int32 1)))
        (Phantoms.nothing :: TTerm (Maybe Int))
    -- g: y -> if y >= 5 then Just (y * 2) else Nothing
    funG = Phantoms.lambda "y" $
      Logic.ifElse (Equality.gte (Phantoms.var "y") (Phantoms.int32 5))
        (Phantoms.just (Math.mul (Phantoms.var "y") (Phantoms.int32 2)))
        (Phantoms.nothing :: TTerm (Maybe Int))
    test name input expected = evalPair name showMaybeInt
      (Maybes.compose funF funG # Phantoms.int32 input)
      expected
    testFails name input = evalPair name showMaybeInt
      (Maybes.compose funF funG # Phantoms.int32 input)
      nothingInt

maybesToList :: TTerm TestGroup
maybesToList = subgroup "toList" [
  test "just value" (justInt 42) [42],
  test "nothing" nothingInt []]
  where
    test name x expected = evalPair name showIntList
      (Maybes.toList x)
      (Phantoms.list $ Phantoms.int32 <$> expected)

allTests :: TTermDefinition TestGroup
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
      maybesPure,
      maybesToList]
