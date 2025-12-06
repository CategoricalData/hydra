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


module_ :: Module
module_ = Module (Namespace "hydra.test.inference.nominalTypes") elements
    [TestGraph.module_]
    kernelTypesModules
    (Just "Inference tests for nominal types")
  where
    elements = [
      el allTestsDef,
      el testGroupForCaseStatementsDef,
      el testGroupForProjectionsDef,
      el testGroupForRecordsDef,
      el testGroupForVariantsDef,
      el testGroupForWrappersDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTestsDef :: TBinding TestGroup
allTestsDef = define "allTests" $
  Phantoms.doc "Nominal type tests" $
  supergroup "Nominal terms" [
    ref testGroupForCaseStatementsDef,
    ref testGroupForProjectionsDef,
    ref testGroupForRecordsDef,
    ref testGroupForVariantsDef,
    ref testGroupForWrappersDef]

testGroupForCaseStatementsDef :: TBinding TestGroup
testGroupForCaseStatementsDef = define "testGroupForCaseStatements" $
  subgroup "Case statements" [
    expectMono 1 [tag_disabledForMinimalInference]
      (match (ref TestTypes.testTypeSimpleNumberNameDef) nothing [
        "int">: lambda "x" $ var "x",
        "float">: lambda "x" $ int32 42])
      (T.function (Core.typeVariable $ ref TestTypes.testTypeSimpleNumberNameDef) T.int32),
    expectMono 2 [tag_disabledForMinimalInference]
      (match (ref TestTypes.testTypeUnionMonomorphicNameDef) nothing [
        "bool">: constant true,
        "string">: constant false,
        "unit">: constant false])
      (T.function (Core.typeVariable $ ref TestTypes.testTypeUnionMonomorphicNameDef) T.boolean)]

testGroupForProjectionsDef :: TBinding TestGroup
testGroupForProjectionsDef = define "testGroupForProjections" $
  supergroup "Projections" [
    subgroup "Record eliminations" [
      expectMono 1 [tag_disabledForMinimalInference]
        (project (ref TestTypes.testTypePersonNameDef) (name "firstName"))
        (T.function (Core.typeVariable $ ref TestTypes.testTypePersonNameDef) T.string)],

    subgroup "Pair projections" [
      expectPoly 1 [tag_disabledForMinimalInference]
        (primitive _pairs_first)
        ["t0", "t1"] (T.function (T.pair (T.var "t0") (T.var "t1")) (T.var "t0")),
      expectMono 2 [tag_disabledForMinimalInference]
        (primitive _pairs_second @@ pair (int32 42) (string "foo"))
        T.string]]

testGroupForRecordsDef :: TBinding TestGroup
testGroupForRecordsDef = define "testGroupForRecords" $
  supergroup "Records" [

    subgroup "Simple records" [
      expectMono 1 [tag_disabledForMinimalInference]
        (record (ref TestTypes.testTypeLatLonNameDef) [
          "lat">: float32 37.7749,
          "lon">: float32 $ negate 122.4194])
        (Core.typeVariable (ref TestTypes.testTypeLatLonNameDef)),
      expectMono 2 [tag_disabledForMinimalInference]
        (record (ref TestTypes.testTypeLatLonPolyNameDef) [
          "lat">: float32 37.7749,
          "lon">: float32 $ negate 122.4194])
        (T.apply (Core.typeVariable (ref TestTypes.testTypeLatLonPolyNameDef)) T.float32),
      expectMono 3 [tag_disabledForMinimalInference]
        (lambda "lon" (record (ref TestTypes.testTypeLatLonPolyNameDef) [
          "lat">: float32 37.7749,
          "lon">: var "lon"]))
        (T.function T.float32 (T.apply (Core.typeVariable (ref TestTypes.testTypeLatLonPolyNameDef)) T.float32)),
      expectPoly 4 [tag_disabledForMinimalInference]
        (lambda "latlon" (record (ref TestTypes.testTypeLatLonPolyNameDef) [
          "lat">: var "latlon",
          "lon">: var "latlon"]))
        ["t0"] (T.function (T.var "t0") (T.apply (Core.typeVariable (ref TestTypes.testTypeLatLonPolyNameDef)) (T.var "t0"))),
      expectMono 5 [tag_disabledForMinimalInference]
        (ref TestTerms.testDataArthurDef)
        (Core.typeVariable (ref TestTypes.testTypePersonNameDef))],

    subgroup "Record instances of simply recursive record types" [
      expectMono 1 [tag_disabledForMinimalInference]
        (record (ref TestTypes.testTypeIntListNameDef) [
          "head">: int32 42,
          "tail">: optional $ just (record (ref TestTypes.testTypeIntListNameDef) [
            "head">: int32 43,
            "tail">: optional nothing])])
        (Core.typeVariable (ref TestTypes.testTypeIntListNameDef)),
      expectMono 2 [tag_disabledForMinimalInference]
        ((lambda "x" $ record (ref TestTypes.testTypeIntListNameDef) [
          "head">: var "x",
          "tail">: optional $ just (record (ref TestTypes.testTypeIntListNameDef) [
            "head">: var "x",
            "tail">: optional nothing])]) @@ int32 42)
        (Core.typeVariable (ref TestTypes.testTypeIntListNameDef)),
      expectMono 3 [tag_disabledForMinimalInference]
        (record (ref TestTypes.testTypeListNameDef) [
          "head">: int32 42,
          "tail">: optional $ just (record (ref TestTypes.testTypeListNameDef) [
            "head">: int32 43,
            "tail">: optional nothing])])
        (T.apply (Core.typeVariable (ref TestTypes.testTypeListNameDef)) T.int32),
      expectMono 4 [tag_disabledForMinimalInference]
        ((lambda "x" $ record (ref TestTypes.testTypeListNameDef) [
          "head">: var "x",
          "tail">: optional $ just (record (ref TestTypes.testTypeListNameDef) [
            "head">: var "x",
            "tail">: optional nothing])]) @@ int32 42)
        (T.apply (Core.typeVariable (ref TestTypes.testTypeListNameDef)) T.int32),
      expectPoly 5 [tag_disabledForMinimalInference]
        (lambda "x" $ record (ref TestTypes.testTypeListNameDef) [
          "head">: var "x",
          "tail">: optional $ just (record (ref TestTypes.testTypeListNameDef) [
            "head">: var "x",
            "tail">: optional nothing])])
        ["t0"] (T.function (T.var "t0") (T.apply (Core.typeVariable (ref TestTypes.testTypeListNameDef)) (T.var "t0")))],

    subgroup "Record instances of mutually recursive record types" [
      expectMono 1 [tag_disabledForMinimalInference]
        ((lambda "x" $ record (ref TestTypes.testTypeBuddyListANameDef) [
          "head">: var "x",
          "tail">: optional $ just $ record (ref TestTypes.testTypeBuddyListBNameDef) [
            "head">: var "x",
            "tail">: optional nothing]]) @@ int32 42)
        (T.apply (Core.typeVariable $ ref TestTypes.testTypeBuddyListANameDef) T.int32),
      expectPoly 2 [tag_disabledForMinimalInference]
        (lambda "x" $ record (ref TestTypes.testTypeBuddyListANameDef) [
          "head">: var "x",
          "tail">: optional $ just $ record (ref TestTypes.testTypeBuddyListBNameDef) [
            "head">: var "x",
            "tail">: optional nothing]])
        ["t0"] (T.function (T.var "t0") (T.apply (Core.typeVariable $ ref TestTypes.testTypeBuddyListANameDef) (T.var "t0")))]]

testGroupForVariantsDef :: TBinding TestGroup
testGroupForVariantsDef = define "testGroupForVariants" $
  supergroup "Variant terms" [

    subgroup "Variants" [
      expectMono 1 [tag_disabledForMinimalInference]
        (inject (ref TestTypes.testTypeTimestampNameDef) "unixTimeMillis" $ uint64 1638200308368)
        (Core.typeVariable (ref TestTypes.testTypeTimestampNameDef)),
      expectMono 2 [tag_disabledForMinimalInference]
        (inject (ref TestTypes.testTypeUnionMonomorphicNameDef) "string" $ string "bar")
        (Core.typeVariable (ref TestTypes.testTypeUnionMonomorphicNameDef))],
--    TODO: inference failure test cases
--      H.it "test #3" $
--        expectFailure
--          (inject TestTypes.testTypeUnionMonomorphicName $ Field (Name "string") $ int32 42)

    subgroup "Polymorphic and recursive variants" [
      expectPoly 1 [tag_disabledForMinimalInference]
        (inject (ref TestTypes.testTypeUnionPolymorphicRecursiveNameDef) "bool" true)
        ["t0"] (T.apply (Core.typeVariable (ref TestTypes.testTypeUnionPolymorphicRecursiveNameDef)) (T.var "t0")),
      expectMono 2 [tag_disabledForMinimalInference]
        (inject (ref TestTypes.testTypeUnionPolymorphicRecursiveNameDef) "value" $ string "foo")
        (T.apply (Core.typeVariable (ref TestTypes.testTypeUnionPolymorphicRecursiveNameDef)) T.string),
      expectMono 3 [tag_disabledForMinimalInference]
        (lets [
          "other">: inject (ref TestTypes.testTypeUnionPolymorphicRecursiveNameDef) "value" $ int32 42]
          $ inject (ref TestTypes.testTypeUnionPolymorphicRecursiveNameDef) "other" $ var "other")
        (T.apply (Core.typeVariable (ref TestTypes.testTypeUnionPolymorphicRecursiveNameDef)) T.int32)]]

testGroupForWrappersDef :: TBinding TestGroup
testGroupForWrappersDef = define "testGroupForWrappers" $
  supergroup "Wrapper introductions and eliminations" [

    subgroup "Wrapper introductions" [
      expectMono 1 [tag_disabledForMinimalInference]
        (wrap (ref TestTypes.testTypeStringAliasNameDef) $ string "foo")
        (Core.typeVariable $ ref TestTypes.testTypeStringAliasNameDef),
      expectMono 2 [tag_disabledForMinimalInference]
        (lambda "v" $ wrap (ref TestTypes.testTypeStringAliasNameDef) $ var "v")
        (T.function T.string (Core.typeVariable $ ref TestTypes.testTypeStringAliasNameDef))],

    subgroup "Wrapper eliminations" [
      expectMono 1 [tag_disabledForMinimalInference]
        (unwrap (ref TestTypes.testTypeStringAliasNameDef))
        (T.function (Core.typeVariable $ ref TestTypes.testTypeStringAliasNameDef) T.string),
      expectMono 2 [tag_disabledForMinimalInference]
        (unwrap (ref TestTypes.testTypeStringAliasNameDef) @@ (wrap (ref TestTypes.testTypeStringAliasNameDef) $ string "foo"))
        T.string]]
