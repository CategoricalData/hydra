{-# LANGUAGE OverloadedStrings #-}

-- | Collection type checking test cases: lists, sets, maps
module Hydra.Sources.Test.Checking.Collections where

import Hydra.Kernel hiding (map)
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Dsl.Meta.Types as T
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Meta.Core as Core
import Hydra.Sources.Test.TestGraph

import Prelude hiding (map, product, sum)
import qualified Data.List as L
import qualified Data.Map as M


collectionsTests :: TTerm TestGroup
collectionsTests = supergroup "Collections" [
  listsTests,
  setsTests,
  mapsTests]

------ Helper functions ------

-- Helper function to create a type checking test case
checkTest :: String -> [Tag] -> TTerm Term -> TTerm Term -> TTerm Type -> TTerm TestCaseWithMetadata
checkTest name tags input outputTerm outputType = testCaseWithMetadata (Phantoms.string name)
  (testCaseTypeChecking $ typeCheckingTestCase input outputTerm outputType) Phantoms.nothing (Phantoms.list $ tag . unTag <$> tags)

-- Helper for tests where the term doesn't change during type checking
noChange :: String -> TTerm Term -> TTerm Type -> TTerm TestCaseWithMetadata
noChange name term typ = checkTest name [] term term typ

-- Create a TestCase inject for type checking
testCaseTypeChecking :: TTerm TypeCheckingTestCase -> TTerm TestCase
testCaseTypeChecking = Phantoms.inject _TestCase _TestCase_typeChecking

-- Create a TypeCheckingTestCase record
typeCheckingTestCase :: TTerm Term -> TTerm Term -> TTerm Type -> TTerm TypeCheckingTestCase
typeCheckingTestCase input outputTerm outputType = Phantoms.record _TypeCheckingTestCase [
  Phantoms.field _TypeCheckingTestCase_input input,
  Phantoms.field _TypeCheckingTestCase_outputTerm outputTerm,
  Phantoms.field _TypeCheckingTestCase_outputType outputType]

------ Lists ------

listsTests :: TTerm TestGroup
listsTests = supergroup "Lists" [
  listsOfLiteralsTests,
  emptyListsTests,
  polymorphicListsTests,
  nestedListsTests,
  listsInComplexContextsTests]

listsOfLiteralsTests :: TTerm TestGroup
listsOfLiteralsTests = subgroup "Lists of literals" [
  noChange "int list"
    (list [int32 1, int32 2])
    (T.list T.int32),
  noChange "string list"
    (list [string "hello", string "world"])
    (T.list T.string),
  noChange "single element list"
    (list [bigint 42])
    (T.list T.bigint),
  noChange "mixed numeric types"
    (list [float32 1.0, float32 2.5, float32 3.14])
    (T.list T.float32)]

emptyListsTests :: TTerm TestGroup
emptyListsTests = subgroup "Empty lists" [
  checkTest "empty list" []
    (list [])
    (tylam "t0" $ tyapp (list []) (T.var "t0"))
    (T.forAll "t0" $ T.list $ T.var "t0"),
  checkTest "pair of empty lists" []
    (tuple2 (list []) (list []))
    (tylams ["t0", "t1"] $ tuple2 (tyapp (list []) (T.var "t0")) (tyapp (list []) (T.var "t1")))
    (T.forAlls ["t0", "t1"] $ T.tuple2 (T.list $ T.var "t0") (T.list $ T.var "t1")),
  checkTest "empty list in tuple" []
    (tuple [list [], string "context"])
    (tylam "t0" $ tuple [tyapp (list []) (T.var "t0"), string "context"])
    (T.forAll "t0" $ T.product [T.list $ T.var "t0", T.string])]

polymorphicListsTests :: TTerm TestGroup
polymorphicListsTests = subgroup "Polymorphic lists" [
  checkTest "list from lambda" []
    (lambda "x" $ list [var "x"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ list [var "x"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.list $ T.var "t0")),
  checkTest "list with repeated var" []
    (lambda "x" $ list [var "x", var "x"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ list [var "x", var "x"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.list $ T.var "t0")),
  checkTest "list from two lambdas" []
    (lambda "x" $ lambda "y" $ list [var "x", var "y"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t0") $ list [var "x", var "y"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.function (T.var "t0") (T.list $ T.var "t0")))]

nestedListsTests :: TTerm TestGroup
nestedListsTests = subgroup "Nested lists" [
  noChange "list of lists"
    (list [list [int32 1], list [int32 2, int32 3]])
    (T.list $ T.list T.int32),
  checkTest "empty nested lists" []
    (list [list [], list []])
    (tylam "t0" $ list [tyapp (list []) (T.var "t0"), tyapp (list []) (T.var "t0")])
    (T.forAll "t0" $ T.list $ T.list $ T.var "t0"),
  checkTest "nested polymorphic" []
    (lambda "x" $ list [list [var "x"]])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ list [list [var "x"]])
    (T.forAll "t0" $ T.function (T.var "t0") (T.list $ T.list $ T.var "t0"))]

listsInComplexContextsTests :: TTerm TestGroup
listsInComplexContextsTests = subgroup "Lists in complex contexts" [
  noChange "multiple lists in tuple"
    (tuple [
      list [int32 1, int32 2],
      list [string "a", string "b"]])
    (T.product [T.list T.int32, T.list T.string])]

------ Maps ------

mapsTests :: TTerm TestGroup
mapsTests = supergroup "Maps" [
  monomorphicMapsTests,
  polymorphicMapsTests,
  mapsInComplexContextsTests]
--  mapsWithComplexTypesTests] -- TODO: restore this group

monomorphicMapsTests :: TTerm TestGroup
monomorphicMapsTests = subgroup "Monomorphic maps" [
  checkTest "empty map" []
    (mapTerm [])
    (tylams ["t0", "t1"] $ tyapps (mapTerm []) [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.map (T.var "t0") (T.var "t1")),
  noChange "int to string map"
    (mapTerm [(int32 1, string "one"),
                        (int32 2, string "two")])
    (T.map T.int32 T.string),
  noChange "string to int map"
    (mapTerm [(string "a", int32 1),
                        (string "b", int32 2)])
    (T.map T.string T.int32),
  noChange "single entry map"
    (mapTerm [(bigint 42, boolean True)])
    (T.map T.bigint T.boolean)]

polymorphicMapsTests :: TTerm TestGroup
polymorphicMapsTests = subgroup "Polymorphic maps" [
  checkTest "map from lambda keys" []
    (lambda "k" $ mapTerm [(var "k", string "value")])
    (tylam "t0" $ lambdaTyped "k" (T.var "t0") $ mapTerm [(var "k", string "value")])
    (T.forAll "t0" $ T.function (T.var "t0") (T.map (T.var "t0") T.string)),
  checkTest "map from lambda values" []
    (lambda "v" $ mapTerm [(string "key", var "v")])
    (tylam "t0" $ lambdaTyped "v" (T.var "t0") $ mapTerm [(string "key", var "v")])
    (T.forAll "t0" $ T.function (T.var "t0") (T.map T.string (T.var "t0"))),
  checkTest "map from lambda both" []
    (lambda "k" $ lambda "v" $ mapTerm [(var "k", var "v")])
    (tylams ["t0", "t1"] $ lambdaTyped "k" (T.var "t0") $ lambdaTyped "v" (T.var "t1") $ mapTerm [(var "k", var "v")])
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.function (T.var "t1") (T.map (T.var "t0") (T.var "t1")))),
  checkTest "map with repeated variables" []
    (lambda "x" $ mapTerm [(var "x", var "x")])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ mapTerm [(var "x", var "x")])
    (T.forAll "t0" $ T.function (T.var "t0") (T.map (T.var "t0") (T.var "t0")))]

mapsInComplexContextsTests :: TTerm TestGroup
mapsInComplexContextsTests = subgroup "Maps in complex contexts" [
  noChange "map in tuple"
    (tuple [mapTerm [(int32 1, string "one")],
            string "context"])
    (T.product [T.map T.int32 T.string, T.string]),
  noChange "nested maps"
    (mapTerm [(string "outer", mapTerm [(int32 1, boolean True)])])
    (T.map T.string (T.map T.int32 T.boolean)),
  checkTest "map in let binding" []
    (lets ["lookup">: mapTerm [(string "key1", int32 100),
                                         (string "key2", int32 200)]] $
          var "lookup")
    (letsTyped [("lookup", mapTerm [(string "key1", int32 100),
                                              (string "key2", int32 200)],
                 T.mono $ T.map T.string T.int32)] $
      var "lookup")
    (T.map T.string T.int32)]

mapsWithComplexTypesTests :: TTerm TestGroup
mapsWithComplexTypesTests = subgroup "Maps with complex types" [
  noChange "map of records"
    (mapTerm [(string "person1",
                   record (ref testTypePersonNameDef) [
                     "firstName">: string "Alice",
                     "lastName">: string "Smith",
                     "age">: int32 25])])
    (T.map T.string (Core.typeVariable $ ref testTypePersonNameDef)),
  noChange "map of lists"
    (mapTerm [(int32 1, list [string "a", string "b"]),
                        (int32 2, list [string "c", string "d"])])
    (T.map T.int32 (T.list T.string)),
  noChange "map of tuples"
    (mapTerm [(string "coords", tuple [int32 10, int32 20])])
    (T.map T.string (T.product [T.int32, T.int32]))]

------ Sets ------

setsTests :: TTerm TestGroup
setsTests = supergroup "Sets" [
  monomorphicSetsTests,
  polymorphicSetsTests,
  setsInComplexContextsTests,
  nestedSetsTests,
  setsWithComplexTypesTests]

monomorphicSetsTests :: TTerm TestGroup
monomorphicSetsTests = subgroup "Monomorphic sets" [
  checkTest "empty set" []
    (MetaTerms.set [])
    (tylam "t0" $ tyapp (MetaTerms.set []) (T.var "t0"))
    (T.forAll "t0" $ T.set $ T.var "t0"),
  noChange "int set"
    (MetaTerms.set [int32 1, int32 2, int32 3])
    (T.set T.int32),
  noChange "string set"
    (MetaTerms.set [string "apple", string "banana", string "cherry"])
    (T.set T.string),
  noChange "single element set"
    (MetaTerms.set [boolean True])
    (T.set T.boolean)]

polymorphicSetsTests :: TTerm TestGroup
polymorphicSetsTests = subgroup "Polymorphic sets" [
  checkTest "set from lambda" []
    (lambda "x" $ MetaTerms.set [var "x"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ MetaTerms.set [var "x"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.set $ T.var "t0")),
  checkTest "set with repeated variable" []
    (lambda "x" $ MetaTerms.set [var "x", var "x"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ MetaTerms.set [var "x", var "x"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.set $ T.var "t0")),
  checkTest "set from two variables" []
    (lambda "x" $ lambda "y" $ MetaTerms.set [var "x", var "y"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t0") $ MetaTerms.set [var "x", var "y"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.function (T.var "t0") (T.set $ T.var "t0")))]

setsInComplexContextsTests :: TTerm TestGroup
setsInComplexContextsTests = subgroup "Sets in complex contexts" [
  noChange "set in tuple"
    (tuple [MetaTerms.set [int32 1, int32 2], string "context"])
    (T.product [T.set T.int32, T.string]),
  checkTest "set in let binding" []
    (lets ["numbers">: MetaTerms.set [int32 10, int32 20, int32 30]] $
      var "numbers")
    (letsTyped [("numbers", MetaTerms.set [int32 10, int32 20, int32 30],
      T.mono $ T.set T.int32)] $
      var "numbers")
    (T.set T.int32)]

nestedSetsTests :: TTerm TestGroup
nestedSetsTests = subgroup "Nested sets" [
  noChange "set of lists"
    (MetaTerms.set [
      list [string "a", string "b"],
      list [string "c", string "d"]])
    (T.set $ T.list T.string),
  noChange "set of tuples"
    (MetaTerms.set [
      tuple [int32 1, int32 2],
      tuple [int32 3, int32 4]])
    (T.set $ T.product [T.int32, T.int32]),
  noChange "set of sets"
    (MetaTerms.set [MetaTerms.set [string "nested"]])
    (T.set $ T.set T.string)]

setsWithComplexTypesTests :: TTerm TestGroup
setsWithComplexTypesTests = subgroup "Sets with complex types" [
  noChange "set of records"
    (MetaTerms.set [record (ref testTypePersonNameDef) [
      "firstName">: string "Alice",
      "lastName">: string "Smith",
      "age">: int32 30]])
    (T.set $ Core.typeVariable $ ref testTypePersonNameDef),
  checkTest "set of optionals" []
    (MetaTerms.set [
      optional $ just $ int32 42,
      optional nothing])
    (MetaTerms.set [
      optional $ just $ int32 42,
      tyapp (optional nothing) T.int32])
    (T.set $ T.optional T.int32),
  noChange "set of maps"
    (MetaTerms.set [MetaTerms.map $ Phantoms.map $ M.singleton (string "key") (int32 42)])
    (T.set $ T.map T.string T.int32)]
