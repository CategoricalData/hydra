{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Test.Lib.Eithers where

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
import qualified Hydra.Dsl.Meta.Lib.Eithers as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic as Logic
import qualified Hydra.Dsl.Meta.Lib.Math as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes as Maybes
import qualified Hydra.Dsl.Meta.Lib.Strings as Strings
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


ns :: Namespace
ns = Namespace "hydra.test.lib.eithers"

module_ :: Module
module_ = Module ns elements
    [Namespace "hydra.reduction", ShowCore.ns]
    kernelTypesNamespaces $
    Just "Test cases for hydra.lib.eithers primitives"
  where
    elements = [Phantoms.toTermDefinition allTests]

(#) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TTerm b
(#) = (Phantoms.@@)
infixl 1 #

-- Show functions

showInt32 :: TTerm (Int -> String)
showInt32 = Phantoms.lambda "n" $ Literals.showInt32 (Phantoms.var "n")

showBool :: TTerm (Bool -> String)
showBool = Phantoms.lambda "b" $ Literals.showBoolean (Phantoms.var "b")

showStr :: TTerm (String -> String)
showStr = Phantoms.lambda "s" $ Literals.showString (Phantoms.var "s")

showIntList :: TTerm ([Int] -> String)
showIntList = Phantoms.lambda "xs" $ ShowCore.list_ # showInt32 # Phantoms.var "xs"

showStrList :: TTerm ([String] -> String)
showStrList = Phantoms.lambda "xs" $ ShowCore.list_ # showStr # Phantoms.var "xs"

showEitherIntString :: TTerm (Either Int String -> String)
showEitherIntString = Phantoms.lambda "e" $ ShowCore.either_ # showInt32 # showStr # Phantoms.var "e"

showEitherIntInt :: TTerm (Either Int Int -> String)
showEitherIntInt = Phantoms.lambda "e" $ ShowCore.either_ # showInt32 # showInt32 # Phantoms.var "e"

showEitherStringIntList :: TTerm (Either String [Int] -> String)
showEitherStringIntList = Phantoms.lambda "e" $ ShowCore.either_ # showStr # showIntList # Phantoms.var "e"

showEitherStringMaybeInt :: TTerm (Either String (Maybe Int) -> String)
showEitherStringMaybeInt = Phantoms.lambda "e" $ ShowCore.either_ # showStr # showMaybeInt # Phantoms.var "e"

showMaybeInt :: TTerm (Maybe Int -> String)
showMaybeInt = Phantoms.lambda "mx" $ ShowCore.maybe_ # showInt32 # Phantoms.var "mx"

showPairIntListStringList :: TTerm (([Int], [String]) -> String)
showPairIntListStringList = Phantoms.lambda "p" $ ShowCore.pair_ # showIntList # showStrList # Phantoms.var "p"

-- Phantom-typed helpers

leftInt :: Int -> TTerm (Either Int b)
leftInt x = Phantoms.left (Phantoms.int32 x)

rightStr :: String -> TTerm (Either a String)
rightStr s = Phantoms.right (Phantoms.string s)

rightInt :: Int -> TTerm (Either a Int)
rightInt x = Phantoms.right (Phantoms.int32 x)

-- Test groups for hydra.lib.eithers primitives

eithersBind :: TTerm TestGroup
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

eithersBimap :: TTerm TestGroup
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

eithersIsLeft :: TTerm TestGroup
eithersIsLeft = subgroup "isLeft" [
  test "left value" (leftInt 42) True,
  test "right value" (rightStr "test") False]
  where
    test name x result = evalPair name showBool
      (Eithers.isLeft x)
      (Phantoms.boolean result)

eithersIsRight :: TTerm TestGroup
eithersIsRight = subgroup "isRight" [
  test "right value" (rightStr "test") True,
  test "left value" (leftInt 42) False]
  where
    test name x result = evalPair name showBool
      (Eithers.isRight x)
      (Phantoms.boolean result)

eithersFromLeft :: TTerm TestGroup
eithersFromLeft = subgroup "fromLeft" [
  test "extract left" 99 (leftInt 42) 42,
  test "use default for right" 99 (rightStr "test") 99]
  where
    test name def x result = evalPair name showInt32
      (Eithers.fromLeft (Phantoms.int32 def) x)
      (Phantoms.int32 result)

eithersFromRight :: TTerm TestGroup
eithersFromRight = subgroup "fromRight" [
  test "extract right" "default" (rightStr "test") "test",
  test "use default for left" "default" (leftInt 42) "default"]
  where
    test name def x result = evalPair name showStr
      (Eithers.fromRight (Phantoms.string def) x)
      (Phantoms.string result)

eithersEither :: TTerm TestGroup
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

eithersLefts :: TTerm TestGroup
eithersLefts = subgroup "lefts" [
  test "filter left values" [leftInt 1, rightStr "a", leftInt 2, rightStr "b"] [1, 2],
  test "all lefts" [leftInt 1, leftInt 2] [1, 2],
  test "all rights" [rightStr "a", rightStr "b"] [],
  test "empty list" ([] :: [TTerm (Either Int String)]) []]
  where
    test name input expected = evalPair name showIntList
      (Eithers.lefts (Phantoms.list input))
      (Phantoms.list (Phantoms.int32 <$> expected))

eithersRights :: TTerm TestGroup
eithersRights = subgroup "rights" [
  test "filter right values" [leftInt 1, rightStr "a", leftInt 2, rightStr "b"] ["a", "b"],
  test "all rights" [rightStr "a", rightStr "b"] ["a", "b"],
  test "all lefts" [leftInt 1, leftInt 2] [],
  test "empty list" ([] :: [TTerm (Either Int String)]) []]
  where
    test name input expected = evalPair name showStrList
      (Eithers.rights (Phantoms.list input))
      (Phantoms.list (Phantoms.string <$> expected))

eithersPartitionEithers :: TTerm TestGroup
eithersPartitionEithers = subgroup "partitionEithers" [
  test "partition mixed" [leftInt 1, rightStr "a", leftInt 2, rightStr "b"] ([1, 2], ["a", "b"]),
  test "all lefts" [leftInt 1, leftInt 2] ([1, 2], []),
  test "all rights" [rightStr "a", rightStr "b"] ([], ["a", "b"]),
  test "empty list" ([] :: [TTerm (Either Int String)]) ([], [])]
  where
    test name input (lefts, rights) = evalPair name showPairIntListStringList
      (Eithers.partitionEithers (Phantoms.list input))
      (Phantoms.pair (Phantoms.list (Phantoms.int32 <$> lefts)) (Phantoms.list (Phantoms.string <$> rights)))

eithersMap :: TTerm TestGroup
eithersMap = subgroup "map" [
  test "map right value" (rightInt 5) (rightInt 10),
  test "preserve left" (leftInt 99) (leftInt 99)]
  where
    test name input expected = evalPair name showEitherIntInt
      (Eithers.map
        (Phantoms.lambda "x" $ Math.mul (Phantoms.var "x") (Phantoms.int32 2))
        input)
      expected

eithersMapList :: TTerm TestGroup
eithersMapList = subgroup "mapList" [
  test "all succeed" [1, 2, 3] (Phantoms.right (Phantoms.list [Phantoms.int32 2, Phantoms.int32 4, Phantoms.int32 6])),
  test "first fails" [1, 0, 3] (Phantoms.left (Phantoms.string "zero")),
  test "empty list" [] (Phantoms.right (Phantoms.list ([] :: [TTerm Int])))]
  where
    mapFn = Phantoms.lambda "x" $
      Logic.ifElse (Equality.equal (Phantoms.var "x") (Phantoms.int32 0))
        (Phantoms.left (Phantoms.string "zero"))
        (Phantoms.right (Math.mul (Phantoms.var "x") (Phantoms.int32 2)))
    test name input expected = evalPair name showEitherStringIntList
      (Eithers.mapList mapFn (Phantoms.list (Phantoms.int32 <$> input)))
      expected

eithersMapMaybe :: TTerm TestGroup
eithersMapMaybe = subgroup "mapMaybe" [
  test "just succeeds" (Phantoms.just (Phantoms.int32 5)) (Phantoms.right (Phantoms.just (Phantoms.int32 10))),
  test "just fails" (Phantoms.just (Phantoms.int32 0)) (Phantoms.left (Phantoms.string "zero")),
  test "nothing" (Phantoms.nothing :: TTerm (Maybe Int)) (Phantoms.right (Phantoms.nothing :: TTerm (Maybe Int)))]
  where
    mapFn = Phantoms.lambda "x" $
      Logic.ifElse (Equality.equal (Phantoms.var "x") (Phantoms.int32 0))
        (Phantoms.left (Phantoms.string "zero"))
        (Phantoms.right (Math.mul (Phantoms.var "x") (Phantoms.int32 2)))
    test name input expected = evalPair name showEitherStringMaybeInt
      (Eithers.mapMaybe mapFn input)
      expected

allTests :: TBinding TestGroup
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
