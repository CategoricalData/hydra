module Hydra.Sources.Test.Inference.NominalTypes where

-- Standard imports for kernel tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing as Testing
import Hydra.Dsl.Meta.Terms as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Meta.Types as T
import Hydra.Sources.Libraries
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List as L
import qualified Data.Map  as M


ns :: Namespace
ns = Namespace "hydra.test.inference.nominalTypes"

module_ :: Module
module_ = Module ns elements
    [TestGraph.ns]
    kernelTypesNamespaces
    (Just "Inference tests for nominal types")
  where
    elements = [
      Phantoms.toBinding allTests,
      Phantoms.toBinding testGroupForCaseStatements,
      Phantoms.toBinding testGroupForProjections,
      Phantoms.toBinding testGroupForRecords,
      Phantoms.toBinding testGroupForVariants,
      Phantoms.toBinding testGroupForWrappers]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
  Phantoms.doc "Nominal type tests" $
  supergroup "Nominal terms" [
    testGroupForCaseStatements,
    testGroupForProjections,
    testGroupForRecords,
    testGroupForVariants,
    testGroupForWrappers]

testGroupForCaseStatements :: TBinding TestGroup
testGroupForCaseStatements = define "testGroupForCaseStatements" $
  subgroup "Case statements" [
    expectMono 1 [tag_disabledForMinimalInference]
      (match (TestTypes.testTypeSimpleNumberName) nothing [
        "int">: lambda "x" $ var "x",
        "float">: lambda "x" $ int32 42])
      (T.function (Core.typeVariable $ TestTypes.testTypeSimpleNumberName) T.int32),
    expectMono 2 [tag_disabledForMinimalInference]
      (match (TestTypes.testTypeUnionMonomorphicName) nothing [
        "bool">: constant true,
        "string">: constant false,
        "unit">: constant false])
      (T.function (Core.typeVariable $ TestTypes.testTypeUnionMonomorphicName) T.boolean)]

testGroupForProjections :: TBinding TestGroup
testGroupForProjections = define "testGroupForProjections" $
  supergroup "Projections" [
    subgroup "Record eliminations" [
      expectMono 1 [tag_disabledForMinimalInference]
        (project (TestTypes.testTypePersonName) (name "firstName"))
        (T.function (Core.typeVariable $ TestTypes.testTypePersonName) T.string)],

    subgroup "Pair projections" [
      expectPoly 1 [tag_disabledForMinimalInference]
        (primitive _pairs_first)
        ["t0", "t1"] (T.function (T.pair (T.var "t0") (T.var "t1")) (T.var "t0")),
      expectMono 2 [tag_disabledForMinimalInference]
        (primitive _pairs_second @@ pair (int32 42) (string "foo"))
        T.string]]

testGroupForRecords :: TBinding TestGroup
testGroupForRecords = define "testGroupForRecords" $
  supergroup "Records" [

    subgroup "Simple records" [
      expectMono 1 [tag_disabledForMinimalInference]
        (record (TestTypes.testTypeLatLonName) [
          "lat">: float32 37.7749,
          "lon">: float32 $ negate 122.4194])
        (Core.typeVariable (TestTypes.testTypeLatLonName)),
      expectMono 2 [tag_disabledForMinimalInference]
        (record (TestTypes.testTypeLatLonPolyName) [
          "lat">: float32 37.7749,
          "lon">: float32 $ negate 122.4194])
        (T.apply (Core.typeVariable (TestTypes.testTypeLatLonPolyName)) T.float32),
      expectMono 3 [tag_disabledForMinimalInference]
        (lambda "lon" (record (TestTypes.testTypeLatLonPolyName) [
          "lat">: float32 37.7749,
          "lon">: var "lon"]))
        (T.function T.float32 (T.apply (Core.typeVariable (TestTypes.testTypeLatLonPolyName)) T.float32)),
      expectPoly 4 [tag_disabledForMinimalInference]
        (lambda "latlon" (record (TestTypes.testTypeLatLonPolyName) [
          "lat">: var "latlon",
          "lon">: var "latlon"]))
        ["t0"] (T.function (T.var "t0") (T.apply (Core.typeVariable (TestTypes.testTypeLatLonPolyName)) (T.var "t0"))),
      expectMono 5 [tag_disabledForMinimalInference]
        (TestTerms.testDataArthur)
        (Core.typeVariable (TestTypes.testTypePersonName))],

    subgroup "Record instances of simply recursive record types" [
      expectMono 1 [tag_disabledForMinimalInference]
        (record (TestTypes.testTypeIntListName) [
          "head">: int32 42,
          "tail">: optional $ just (record (TestTypes.testTypeIntListName) [
            "head">: int32 43,
            "tail">: optional nothing])])
        (Core.typeVariable (TestTypes.testTypeIntListName)),
      expectMono 2 [tag_disabledForMinimalInference]
        ((lambda "x" $ record (TestTypes.testTypeIntListName) [
          "head">: var "x",
          "tail">: optional $ just (record (TestTypes.testTypeIntListName) [
            "head">: var "x",
            "tail">: optional nothing])]) @@ int32 42)
        (Core.typeVariable (TestTypes.testTypeIntListName)),
      expectMono 3 [tag_disabledForMinimalInference]
        (record (TestTypes.testTypeListName) [
          "head">: int32 42,
          "tail">: optional $ just (record (TestTypes.testTypeListName) [
            "head">: int32 43,
            "tail">: optional nothing])])
        (T.apply (Core.typeVariable (TestTypes.testTypeListName)) T.int32),
      expectMono 4 [tag_disabledForMinimalInference]
        ((lambda "x" $ record (TestTypes.testTypeListName) [
          "head">: var "x",
          "tail">: optional $ just (record (TestTypes.testTypeListName) [
            "head">: var "x",
            "tail">: optional nothing])]) @@ int32 42)
        (T.apply (Core.typeVariable (TestTypes.testTypeListName)) T.int32),
      expectPoly 5 [tag_disabledForMinimalInference]
        (lambda "x" $ record (TestTypes.testTypeListName) [
          "head">: var "x",
          "tail">: optional $ just (record (TestTypes.testTypeListName) [
            "head">: var "x",
            "tail">: optional nothing])])
        ["t0"] (T.function (T.var "t0") (T.apply (Core.typeVariable (TestTypes.testTypeListName)) (T.var "t0")))],

    subgroup "Record instances of mutually recursive record types" [
      expectMono 1 [tag_disabledForMinimalInference]
        ((lambda "x" $ record (TestTypes.testTypeBuddyListAName) [
          "head">: var "x",
          "tail">: optional $ just $ record (TestTypes.testTypeBuddyListBName) [
            "head">: var "x",
            "tail">: optional nothing]]) @@ int32 42)
        (T.apply (Core.typeVariable $ TestTypes.testTypeBuddyListAName) T.int32),
      expectPoly 2 [tag_disabledForMinimalInference]
        (lambda "x" $ record (TestTypes.testTypeBuddyListAName) [
          "head">: var "x",
          "tail">: optional $ just $ record (TestTypes.testTypeBuddyListBName) [
            "head">: var "x",
            "tail">: optional nothing]])
        ["t0"] (T.function (T.var "t0") (T.apply (Core.typeVariable $ TestTypes.testTypeBuddyListAName) (T.var "t0")))]]

testGroupForVariants :: TBinding TestGroup
testGroupForVariants = define "testGroupForVariants" $
  supergroup "Variant terms" [

    subgroup "Variants" [
      expectMono 1 [tag_disabledForMinimalInference]
        (inject (TestTypes.testTypeTimestampName) "unixTimeMillis" $ uint64 1638200308368)
        (Core.typeVariable (TestTypes.testTypeTimestampName)),
      expectMono 2 [tag_disabledForMinimalInference]
        (inject (TestTypes.testTypeUnionMonomorphicName) "string" $ string "bar")
        (Core.typeVariable (TestTypes.testTypeUnionMonomorphicName))],
--    TODO: inference failure test cases
--      H.it "test #3" $
--        expectFailure
--          (inject TestTypes.testTypeUnionMonomorphicName $ Field (Name "string") $ int32 42)

    subgroup "Polymorphic and recursive variants" [
      expectPoly 1 [tag_disabledForMinimalInference]
        (inject (TestTypes.testTypeUnionPolymorphicRecursiveName) "bool" true)
        ["t0"] (T.apply (Core.typeVariable (TestTypes.testTypeUnionPolymorphicRecursiveName)) (T.var "t0")),
      expectMono 2 [tag_disabledForMinimalInference]
        (inject (TestTypes.testTypeUnionPolymorphicRecursiveName) "value" $ string "foo")
        (T.apply (Core.typeVariable (TestTypes.testTypeUnionPolymorphicRecursiveName)) T.string),
      expectMono 3 [tag_disabledForMinimalInference]
        (lets [
          "other">: inject (TestTypes.testTypeUnionPolymorphicRecursiveName) "value" $ int32 42]
          $ inject (TestTypes.testTypeUnionPolymorphicRecursiveName) "other" $ var "other")
        (T.apply (Core.typeVariable (TestTypes.testTypeUnionPolymorphicRecursiveName)) T.int32)]]

testGroupForWrappers :: TBinding TestGroup
testGroupForWrappers = define "testGroupForWrappers" $
  supergroup "Wrapper introductions and eliminations" [

    subgroup "Wrapper introductions" [
      expectMono 1 [tag_disabledForMinimalInference]
        (wrap (TestTypes.testTypeStringAliasName) $ string "foo")
        (Core.typeVariable $ TestTypes.testTypeStringAliasName),
      expectMono 2 [tag_disabledForMinimalInference]
        (lambda "v" $ wrap (TestTypes.testTypeStringAliasName) $ var "v")
        (T.function T.string (Core.typeVariable $ TestTypes.testTypeStringAliasName))],

    subgroup "Wrapper eliminations" [
      expectMono 1 [tag_disabledForMinimalInference]
        (unwrap (TestTypes.testTypeStringAliasName))
        (T.function (Core.typeVariable $ TestTypes.testTypeStringAliasName) T.string),
      expectMono 2 [tag_disabledForMinimalInference]
        (unwrap (TestTypes.testTypeStringAliasName) @@ (wrap (TestTypes.testTypeStringAliasName) $ string "foo"))
        T.string]]
