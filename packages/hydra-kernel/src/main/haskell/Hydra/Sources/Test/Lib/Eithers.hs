{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Test.Lib.Eithers where

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
import qualified Hydra.Dsl.Meta.Lib.Eithers as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic as Logic
import qualified Hydra.Dsl.Meta.Lib.Math as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes as Maybes
import qualified Hydra.Dsl.Meta.Lib.Strings as Strings
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


ns :: ModuleName
ns = ModuleName "hydra.test.lib.eithers"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([ModuleName "hydra.reduction", ShowCore.ns] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.lib.eithers primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]


-- Show functions

showBool :: TypedTerm (Bool -> String)
showBool = Phantoms.lambda "b" $ Literals.showBoolean (Phantoms.var "b")

showEitherIntInt :: TypedTerm (Either Int Int -> String)
showEitherIntInt = Phantoms.lambda "e" $ ShowCore.either_ @@ showInt32 @@ showInt32 @@ Phantoms.var "e"

showEitherIntString :: TypedTerm (Either Int String -> String)
showEitherIntString = Phantoms.lambda "e" $ ShowCore.either_ @@ showInt32 @@ showStr @@ Phantoms.var "e"

showEitherStringIntList :: TypedTerm (Either String [Int] -> String)
showEitherStringIntList = Phantoms.lambda "e" $ ShowCore.either_ @@ showStr @@ showIntList @@ Phantoms.var "e"

showEitherStringMaybeInt :: TypedTerm (Either String (Maybe Int) -> String)
showEitherStringMaybeInt = Phantoms.lambda "e" $ ShowCore.either_ @@ showStr @@ showMaybeInt @@ Phantoms.var "e"

showInt32 :: TypedTerm (Int -> String)
showInt32 = Phantoms.lambda "n" $ Literals.showInt32 (Phantoms.var "n")

showIntList :: TypedTerm ([Int] -> String)
showIntList = Phantoms.lambda "xs" $ ShowCore.list_ @@ showInt32 @@ Phantoms.var "xs"

showMaybeInt :: TypedTerm (Maybe Int -> String)
showMaybeInt = Phantoms.lambda "mx" $ ShowCore.maybe_ @@ showInt32 @@ Phantoms.var "mx"

showPairIntListStringList :: TypedTerm (([Int], [String]) -> String)
showPairIntListStringList = Phantoms.lambda "p" $ ShowCore.pair_ @@ showIntList @@ showStrList @@ Phantoms.var "p"

showStr :: TypedTerm (String -> String)
showStr = Phantoms.lambda "s" $ Literals.showString (Phantoms.var "s")

showStrList :: TypedTerm ([String] -> String)
showStrList = Phantoms.lambda "xs" $ ShowCore.list_ @@ showStr @@ Phantoms.var "xs"

-- Phantom-typed helpers

leftInt :: Int -> TypedTerm (Either Int b)
leftInt x = Phantoms.left (Phantoms.int32 x)

rightInt :: Int -> TypedTerm (Either a Int)
rightInt x = Phantoms.right (Phantoms.int32 x)

rightStr :: String -> TypedTerm (Either a String)
rightStr s = Phantoms.right (Phantoms.string s)

-- Test groups for hydra.lib.eithers primitives

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.eithers primitives" $
    supergroup "hydra.lib.eithers primitives" [
      eithersBind,
      eithersBimap,
      eithersIsLeft,
      eithersIsRight,
      eithersFromLeft,
      eithersFromRight,
      eithersEither,
      eithersLefts,
      eithersRights,
      eithersPartitionEithers,
      eithersMap,
      eithersMapList,
      eithersMapMaybe]

eithersBimap :: TypedTerm TestGroup
eithersBimap = subgroup "bimap" [
  test "map left value" (leftInt 5) (leftInt 10),
  test "map right value" (rightStr "ab") (rightInt 2)]
  where
    test name input expected = evalPair name showEitherIntInt
      (Eithers.bimap
        (Phantoms.lambda "x" $ Math.mul (Phantoms.var "x") (Phantoms.int32 2))
        (Phantoms.lambda "s" $ Strings.length (Phantoms.var "s"))
        input)
      expected

eithersBind :: TypedTerm TestGroup
eithersBind = subgroup "bind" [
  test "bind Right with success" (rightStr "ab") (rightInt 2),
  test "bind Right with failure" (rightStr "") (leftInt 0),
  test "bind Left returns Left unchanged" (leftInt 42) (leftInt 42)]
  where
    bindFn = Phantoms.lambda "s" $
      Logic.ifElse (Strings.null (Phantoms.var "s"))
        (Phantoms.left (Phantoms.int32 0))
        (Phantoms.right (Strings.length (Phantoms.var "s")))
    test name input expected = evalPair name showEitherIntInt
      (Eithers.bind input bindFn)
      expected

eithersEither :: TypedTerm TestGroup
eithersEither = subgroup "either" [
  test "apply left function" (leftInt 5) 10,
  test "apply right function" (rightStr "ab") 2]
  where
    test name x result = evalPair name showInt32
      (Eithers.either_
        (Phantoms.lambda "x" $ Math.mul (Phantoms.var "x") (Phantoms.int32 2))
        (Phantoms.lambda "s" $ Strings.length (Phantoms.var "s"))
        x)
      (Phantoms.int32 result)

eithersFromLeft :: TypedTerm TestGroup
eithersFromLeft = subgroup "fromLeft" [
  test "extract left" 99 (leftInt 42) 42,
  test "use default for right" 99 (rightStr "test") 99]
  where
    test name def x result = evalPair name showInt32
      (Eithers.fromLeft (Phantoms.int32 def) x)
      (Phantoms.int32 result)

eithersFromRight :: TypedTerm TestGroup
eithersFromRight = subgroup "fromRight" [
  test "extract right" "default" (rightStr "test") "test",
  test "use default for left" "default" (leftInt 42) "default"]
  where
    test name def x result = evalPair name showStr
      (Eithers.fromRight (Phantoms.string def) x)
      (Phantoms.string result)

eithersIsLeft :: TypedTerm TestGroup
eithersIsLeft = subgroup "isLeft" [
  test "left value" (leftInt 42) True,
  test "right value" (rightStr "test") False]
  where
    test name x result = evalPair name showBool
      (Eithers.isLeft x)
      (Phantoms.boolean result)

eithersIsRight :: TypedTerm TestGroup
eithersIsRight = subgroup "isRight" [
  test "right value" (rightStr "test") True,
  test "left value" (leftInt 42) False]
  where
    test name x result = evalPair name showBool
      (Eithers.isRight x)
      (Phantoms.boolean result)

eithersLefts :: TypedTerm TestGroup
eithersLefts = subgroup "lefts" [
  test "filter left values" [leftInt 1, rightStr "a", leftInt 2, rightStr "b"] [1, 2],
  test "all lefts" [leftInt 1, leftInt 2] [1, 2],
  test "all rights" [rightStr "a", rightStr "b"] [],
  test "empty list" ([] :: [TypedTerm (Either Int String)]) []]
  where
    test name input expected = evalPair name showIntList
      (Eithers.lefts (Phantoms.list input))
      (Phantoms.list (Phantoms.int32 <$> expected))

eithersMap :: TypedTerm TestGroup
eithersMap = subgroup "map" [
  test "map right value" (rightInt 5) (rightInt 10),
  test "preserve left" (leftInt 99) (leftInt 99)]
  where
    test name input expected = evalPair name showEitherIntInt
      (Eithers.map
        (Phantoms.lambda "x" $ Math.mul (Phantoms.var "x") (Phantoms.int32 2))
        input)
      expected

eithersMapList :: TypedTerm TestGroup
eithersMapList = subgroup "mapList" [
  test "all succeed" [1, 2, 3] (Phantoms.right (Phantoms.list [Phantoms.int32 2, Phantoms.int32 4, Phantoms.int32 6])),
  test "first fails" [1, 0, 3] (Phantoms.left (Phantoms.string "zero")),
  test "empty list" [] (Phantoms.right (Phantoms.list ([] :: [TypedTerm Int])))]
  where
    mapFn = Phantoms.lambda "x" $
      Logic.ifElse (Equality.equal (Phantoms.var "x") (Phantoms.int32 0))
        (Phantoms.left (Phantoms.string "zero"))
        (Phantoms.right (Math.mul (Phantoms.var "x") (Phantoms.int32 2)))
    test name input expected = evalPair name showEitherStringIntList
      (Eithers.mapList mapFn (Phantoms.list (Phantoms.int32 <$> input)))
      expected

eithersMapMaybe :: TypedTerm TestGroup
eithersMapMaybe = subgroup "mapMaybe" [
  test "just succeeds" (Phantoms.just (Phantoms.int32 5)) (Phantoms.right (Phantoms.just (Phantoms.int32 10))),
  test "just fails" (Phantoms.just (Phantoms.int32 0)) (Phantoms.left (Phantoms.string "zero")),
  test "nothing" (Phantoms.nothing :: TypedTerm (Maybe Int)) (Phantoms.right (Phantoms.nothing :: TypedTerm (Maybe Int)))]
  where
    mapFn = Phantoms.lambda "x" $
      Logic.ifElse (Equality.equal (Phantoms.var "x") (Phantoms.int32 0))
        (Phantoms.left (Phantoms.string "zero"))
        (Phantoms.right (Math.mul (Phantoms.var "x") (Phantoms.int32 2)))
    test name input expected = evalPair name showEitherStringMaybeInt
      (Eithers.mapMaybe mapFn input)
      expected

eithersPartitionEithers :: TypedTerm TestGroup
eithersPartitionEithers = subgroup "partitionEithers" [
  test "partition mixed" [leftInt 1, rightStr "a", leftInt 2, rightStr "b"] ([1, 2], ["a", "b"]),
  test "all lefts" [leftInt 1, leftInt 2] ([1, 2], []),
  test "all rights" [rightStr "a", rightStr "b"] ([], ["a", "b"]),
  test "empty list" ([] :: [TypedTerm (Either Int String)]) ([], [])]
  where
    test name input (lefts, rights) = evalPair name showPairIntListStringList
      (Eithers.partitionEithers (Phantoms.list input))
      (Phantoms.pair (Phantoms.list (Phantoms.int32 <$> lefts)) (Phantoms.list (Phantoms.string <$> rights)))

eithersRights :: TypedTerm TestGroup
eithersRights = subgroup "rights" [
  test "filter right values" [leftInt 1, rightStr "a", leftInt 2, rightStr "b"] ["a", "b"],
  test "all rights" [rightStr "a", rightStr "b"] ["a", "b"],
  test "all lefts" [leftInt 1, leftInt 2] [],
  test "empty list" ([] :: [TypedTerm (Either Int String)]) []]
  where
    test name input expected = evalPair name showStrList
      (Eithers.rights (Phantoms.list input))
      (Phantoms.list (Phantoms.string <$> expected))
