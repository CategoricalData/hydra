{-# LANGUAGE OverloadedStrings #-}

-- | Collection type checking test cases: lists, sets, maps
module Hydra.Sources.Test.Checking.Collections where

-- Standard imports for kernel tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing as Testing
import Hydra.Dsl.Meta.Terms as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Meta.Types as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List as L
import qualified Data.Map  as M


module_ :: Module
module_ = Module (Namespace "hydra.test.checking.collections") elements
    [TestGraph.module_]
    kernelTypesModules
    (Just "Collection type checking test cases: lists, sets, maps")
  where
    elements = [
      el allTestsDef,
      el listsTestsDef,
      el listsOfLiteralsTestsDef,
      el emptyListsTestsDef,
      el polymorphicListsTestsDef,
      el nestedListsTestsDef,
      el listsInComplexContextsTestsDef,
      el mapsTestsDef,
      el monomorphicMapsTestsDef,
      el polymorphicMapsTestsDef,
      el mapsInComplexContextsTestsDef,
      el mapsWithComplexTypesTestsDef,
      el setsTestsDef,
      el monomorphicSetsTestsDef,
      el polymorphicSetsTestsDef,
      el setsInComplexContextsTestsDef,
      el nestedSetsTestsDef,
      el setsWithComplexTypesTestsDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTestsDef :: TBinding TestGroup
allTestsDef = define "allTests" $
  supergroup "Collections" [
  ref listsTestsDef,
  ref setsTestsDef,
  ref mapsTestsDef]

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

listsTestsDef :: TBinding TestGroup
listsTestsDef = define "listsTests" $
  supergroup "Lists" [
  ref listsOfLiteralsTestsDef,
  ref emptyListsTestsDef,
  ref polymorphicListsTestsDef,
  ref nestedListsTestsDef,
  ref listsInComplexContextsTestsDef]

listsOfLiteralsTestsDef :: TBinding TestGroup
listsOfLiteralsTestsDef = define "listsOfLiteralsTests" $
  subgroup "Lists of literals" [
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

emptyListsTestsDef :: TBinding TestGroup
emptyListsTestsDef = define "emptyListsTests" $
  subgroup "Empty lists" [
  checkTest "empty list" []
    (list [])
    (tylam "t0" $ tyapp (list []) (T.var "t0"))
    (T.forAll "t0" $ T.list $ T.var "t0"),
  checkTest "pair of empty lists" []
    (pair (list []) (list []))
    (tylams ["t0", "t1"] $
      tyapps (pair (tyapp (list []) (T.var "t0")) (tyapp (list []) (T.var "t1"))) [T.list $ T.var "t0", T.list $ T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.pair (T.list $ T.var "t0") (T.list $ T.var "t1")),
  checkTest "empty list in tuple" []
    (tuple [list [], string "context"])
    (tylam "t0" $ tuple [tyapp (list []) (T.var "t0"), string "context"])
    (T.forAll "t0" $ T.product [T.list $ T.var "t0", T.string])]

polymorphicListsTestsDef :: TBinding TestGroup
polymorphicListsTestsDef = define "polymorphicListsTests" $
  subgroup "Polymorphic lists" [
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

nestedListsTestsDef :: TBinding TestGroup
nestedListsTestsDef = define "nestedListsTests" $
  subgroup "Nested lists" [
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

listsInComplexContextsTestsDef :: TBinding TestGroup
listsInComplexContextsTestsDef = define "listsInComplexContextsTests" $
  subgroup "Lists in complex contexts" [
  noChange "multiple lists in tuple"
    (tuple [
      list [int32 1, int32 2],
      list [string "a", string "b"]])
    (T.product [T.list T.int32, T.list T.string])]

------ Maps ------

mapsTestsDef :: TBinding TestGroup
mapsTestsDef = define "mapsTests" $
  supergroup "Maps" [
  ref monomorphicMapsTestsDef,
  ref polymorphicMapsTestsDef,
  ref mapsInComplexContextsTestsDef,
  ref mapsWithComplexTypesTestsDef]

monomorphicMapsTestsDef :: TBinding TestGroup
monomorphicMapsTestsDef = define "monomorphicMapsTests" $
  subgroup "Monomorphic maps" [
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

polymorphicMapsTestsDef :: TBinding TestGroup
polymorphicMapsTestsDef = define "polymorphicMapsTests" $
  subgroup "Polymorphic maps" [
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

mapsInComplexContextsTestsDef :: TBinding TestGroup
mapsInComplexContextsTestsDef = define "mapsInComplexContextsTests" $
  subgroup "Maps in complex contexts" [
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

mapsWithComplexTypesTestsDef :: TBinding TestGroup
mapsWithComplexTypesTestsDef = define "mapsWithComplexTypesTests" $
  subgroup "Maps with complex types" [
  noChange "map of records"
    (mapTerm [(string "person1",
                   record (ref TestTypes.testTypePersonNameDef) [
                     "firstName">: string "Alice",
                     "lastName">: string "Smith",
                     "age">: int32 25])])
    (T.map T.string (Core.typeVariable $ ref TestTypes.testTypePersonNameDef)),
  noChange "map of lists"
    (mapTerm [(int32 1, list [string "a", string "b"]),
                        (int32 2, list [string "c", string "d"])])
    (T.map T.int32 (T.list T.string)),
  noChange "map of tuples"
    (mapTerm [(string "coords", tuple [int32 10, int32 20])])
    (T.map T.string (T.product [T.int32, T.int32]))]

------ Sets ------

setsTestsDef :: TBinding TestGroup
setsTestsDef = define "setsTests" $
  supergroup "Sets" [
  ref monomorphicSetsTestsDef,
  ref polymorphicSetsTestsDef,
  ref setsInComplexContextsTestsDef,
  ref nestedSetsTestsDef,
  ref setsWithComplexTypesTestsDef]

monomorphicSetsTestsDef :: TBinding TestGroup
monomorphicSetsTestsDef = define "monomorphicSetsTests" $
  subgroup "Monomorphic sets" [
  checkTest "empty set" []
    (Terms.set [])
    (tylam "t0" $ tyapp (Terms.set []) (T.var "t0"))
    (T.forAll "t0" $ T.set $ T.var "t0"),
  noChange "int set"
    (Terms.set [int32 1, int32 2, int32 3])
    (T.set T.int32),
  noChange "string set"
    (Terms.set [string "apple", string "banana", string "cherry"])
    (T.set T.string),
  noChange "single element set"
    (Terms.set [boolean True])
    (T.set T.boolean)]

polymorphicSetsTestsDef :: TBinding TestGroup
polymorphicSetsTestsDef = define "polymorphicSetsTests" $
  subgroup "Polymorphic sets" [
  checkTest "set from lambda" []
    (lambda "x" $ Terms.set [var "x"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ Terms.set [var "x"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.set $ T.var "t0")),
  checkTest "set with repeated variable" []
    (lambda "x" $ Terms.set [var "x", var "x"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ Terms.set [var "x", var "x"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.set $ T.var "t0")),
  checkTest "set from two variables" []
    (lambda "x" $ lambda "y" $ Terms.set [var "x", var "y"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t0") $ Terms.set [var "x", var "y"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.function (T.var "t0") (T.set $ T.var "t0")))]

setsInComplexContextsTestsDef :: TBinding TestGroup
setsInComplexContextsTestsDef = define "setsInComplexContextsTests" $
  subgroup "Sets in complex contexts" [
  noChange "set in tuple"
    (tuple [Terms.set [int32 1, int32 2], string "context"])
    (T.product [T.set T.int32, T.string]),
  checkTest "set in let binding" []
    (lets ["numbers">: Terms.set [int32 10, int32 20, int32 30]] $
      var "numbers")
    (letsTyped [("numbers", Terms.set [int32 10, int32 20, int32 30],
      T.mono $ T.set T.int32)] $
      var "numbers")
    (T.set T.int32)]

nestedSetsTestsDef :: TBinding TestGroup
nestedSetsTestsDef = define "nestedSetsTests" $
  subgroup "Nested sets" [
  noChange "set of lists"
    (Terms.set [
      list [string "a", string "b"],
      list [string "c", string "d"]])
    (T.set $ T.list T.string),
  noChange "set of tuples"
    (Terms.set [
      tuple [int32 1, int32 2],
      tuple [int32 3, int32 4]])
    (T.set $ T.product [T.int32, T.int32]),
  noChange "set of sets"
    (Terms.set [Terms.set [string "nested"]])
    (T.set $ T.set T.string)]

setsWithComplexTypesTestsDef :: TBinding TestGroup
setsWithComplexTypesTestsDef = define "setsWithComplexTypesTests" $
  subgroup "Sets with complex types" [
  noChange "set of records"
    (Terms.set [record (ref TestTypes.testTypePersonNameDef) [
      "firstName">: string "Alice",
      "lastName">: string "Smith",
      "age">: int32 30]])
    (T.set $ Core.typeVariable $ ref TestTypes.testTypePersonNameDef),
  checkTest "set of optionals" []
    (Terms.set [
      optional $ just $ int32 42,
      optional nothing])
    (Terms.set [
      optional $ just $ int32 42,
      tyapp (optional nothing) T.int32])
    (T.set $ T.optional T.int32),
  noChange "set of maps"
    (Terms.set [Terms.map $ Phantoms.map $ M.singleton (string "key") (int32 42)])
    (T.set $ T.map T.string T.int32)]
