module Hydra.Sources.Tier3.Test.Inference.NominalTypes (nominalTypesTests) where

import Hydra.Kernel
import Hydra.Testing
import qualified Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Core as Core
import Hydra.Dsl.Testing as Testing
import Hydra.Dsl.ShorthandTypes
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Lib.Lists as Lists
import qualified Hydra.Dsl.Lib.Maps as Maps
import qualified Hydra.Dsl.Lib.Math as Math
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Tier3.Test.TestGraph
import Hydra.Dsl.TTerms as TTerms
import qualified Hydra.Dsl.TTypes as T
import Hydra.Sources.Tier3.Test.Inference.Fundamentals

import qualified Data.Map as M
import Prelude hiding (map, product, sum)


nominalTypesTests :: TTerm TestGroup
nominalTypesTests = supergroup "Nominal terms" [
  testGroupForCaseStatements,
  testGroupForProjections,
  testGroupForRecords,
  testGroupForVariants,
  testGroupForWrappers]

testGroupForCaseStatements :: TTerm TestGroup
testGroupForCaseStatements = subgroup "Case statements" [
    expectMono 1 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
      (match (ref testTypeSimpleNumberNameDef) nothing [
        "int">: lambda "x" $ var "x",
        "float">: lambda "x" $ int32 42])
      (T.function (Core.typeVariable $ ref testTypeSimpleNumberNameDef) T.int32),
    expectMono 2 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
      (match (ref testTypeUnionMonomorphicNameDef) nothing [
        "bool">: constant true,
        "string">: constant false,
        "unit">: constant false])
      (T.function (Core.typeVariable $ ref testTypeUnionMonomorphicNameDef) T.boolean)]

testGroupForProjections :: TTerm TestGroup
testGroupForProjections = supergroup "Projections" [
    subgroup "Record eliminations" [
      expectMono 1 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (project (ref testTypePersonNameDef) (name "firstName"))
        (T.function (Core.typeVariable $ ref testTypePersonNameDef) T.string)]]

testGroupForRecords :: TTerm TestGroup
testGroupForRecords = supergroup "Records" [

    subgroup "Simple records" [
      expectMono 1 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (record (ref testTypeLatLonNameDef) [
          "lat">: float32 37.7749,
          "lon">: float32 $ negate 122.4194])
        (Core.typeVariable (ref testTypeLatLonNameDef)),
      expectMono 2 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (record (ref testTypeLatLonPolyNameDef) [
          "lat">: float32 37.7749,
          "lon">: float32 $ negate 122.4194])
        (T.apply (Core.typeVariable (ref testTypeLatLonPolyNameDef)) T.float32),
      expectMono 3 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (lambda "lon" (record (ref testTypeLatLonPolyNameDef) [
          "lat">: float32 37.7749,
          "lon">: var "lon"]))
        (T.function T.float32 (T.apply (Core.typeVariable (ref testTypeLatLonPolyNameDef)) T.float32)),
      expectPoly 4 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (lambda "latlon" (record (ref testTypeLatLonPolyNameDef) [
          "lat">: var "latlon",
          "lon">: var "latlon"]))
        ["t0"] (T.function (T.var "t0") (T.apply (Core.typeVariable (ref testTypeLatLonPolyNameDef)) (T.var "t0"))),
      expectMono 5 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (ref testDataArthurDef)
        (Core.typeVariable (ref testTypePersonNameDef))],

    subgroup "Record instances of simply recursive record types" [
      expectMono 1 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (record (ref testTypeIntListNameDef) [
          "head">: int32 42,
          "tail">: optional $ just (record (ref testTypeIntListNameDef) [
            "head">: int32 43,
            "tail">: optional nothing])])
        (Core.typeVariable (ref testTypeIntListNameDef)),
      expectMono 2 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        ((lambda "x" $ record (ref testTypeIntListNameDef) [
          "head">: var "x",
          "tail">: optional $ just (record (ref testTypeIntListNameDef) [
            "head">: var "x",
            "tail">: optional nothing])]) @@ int32 42)
        (Core.typeVariable (ref testTypeIntListNameDef)),
      expectMono 3 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (record (ref testTypeListNameDef) [
          "head">: int32 42,
          "tail">: optional $ just (record (ref testTypeListNameDef) [
            "head">: int32 43,
            "tail">: optional nothing])])
        (T.apply (Core.typeVariable (ref testTypeListNameDef)) T.int32),
      expectMono 4 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        ((lambda "x" $ record (ref testTypeListNameDef) [
          "head">: var "x",
          "tail">: optional $ just (record (ref testTypeListNameDef) [
            "head">: var "x",
            "tail">: optional nothing])]) @@ int32 42)
        (T.apply (Core.typeVariable (ref testTypeListNameDef)) T.int32),
      expectPoly 5 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (lambda "x" $ record (ref testTypeListNameDef) [
          "head">: var "x",
          "tail">: optional $ just (record (ref testTypeListNameDef) [
            "head">: var "x",
            "tail">: optional nothing])])
        ["t0"] (T.function (T.var "t0") (T.apply (Core.typeVariable (ref testTypeListNameDef)) (T.var "t0")))],

    subgroup "Record instances of mutually recursive record types" [
      expectMono 1 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        ((lambda "x" $ record (ref testTypeBuddyListANameDef) [
          "head">: var "x",
          "tail">: optional $ just $ record (ref testTypeBuddyListBNameDef) [
            "head">: var "x",
            "tail">: optional nothing]]) @@ int32 42)
        (T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) T.int32),
      expectPoly 2 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (lambda "x" $ record (ref testTypeBuddyListANameDef) [
          "head">: var "x",
          "tail">: optional $ just $ record (ref testTypeBuddyListBNameDef) [
            "head">: var "x",
            "tail">: optional nothing]])
        ["t0"] (T.function (T.var "t0") (T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) (T.var "t0")))]]

testGroupForVariants :: TTerm TestGroup
testGroupForVariants = supergroup "Variant terms" [

    subgroup "Variants" [
      expectMono 1 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (inject (ref testTypeTimestampNameDef) "unixTimeMillis" $ uint64 1638200308368)
        (Core.typeVariable (ref testTypeTimestampNameDef)),
      expectMono 2 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (inject (ref testTypeUnionMonomorphicNameDef) "string" $ string "bar")
        (Core.typeVariable (ref testTypeUnionMonomorphicNameDef))],
--    TODO: inference failure test cases
--      H.it "test #3" $
--        expectFailure
--          (inject testTypeUnionMonomorphicName $ Field (Name "string") $ int32 42)

    subgroup "Polymorphic and recursive variants" [
      expectPoly 1 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "bool" true)
        ["t0"] (T.apply (Core.typeVariable (ref testTypeUnionPolymorphicRecursiveNameDef)) (T.var "t0")),
      expectMono 2 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" $ string "foo")
        (T.apply (Core.typeVariable (ref testTypeUnionPolymorphicRecursiveNameDef)) T.string),
      expectMono 3 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (lets [
          "other">: inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" $ int32 42]
          $ inject (ref testTypeUnionPolymorphicRecursiveNameDef) "other" $ var "other")
        (T.apply (Core.typeVariable (ref testTypeUnionPolymorphicRecursiveNameDef)) T.int32)]]

testGroupForWrappers :: TTerm TestGroup
testGroupForWrappers = supergroup "Wrapper introductions and eliminations" [

    subgroup "Wrapper introductions" [
      expectMono 1 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (wrap (ref testTypeStringAliasNameDef) $ string "foo")
        (Core.typeVariable $ ref testTypeStringAliasNameDef),
      expectMono 2 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (lambda "v" $ wrap (ref testTypeStringAliasNameDef) $ var "v")
        (T.function T.string (Core.typeVariable $ ref testTypeStringAliasNameDef))],

    subgroup "Wrapper eliminations" [
      expectMono 1 [tag_disabledForDefaultInference, tag_disabledForAlgorithmWInference]
        (unwrap (ref testTypeStringAliasNameDef))
        (T.function (Core.typeVariable $ ref testTypeStringAliasNameDef) T.string),
      expectMono 2 [tag_disabledForAlgorithmWInference]
        (unwrap (ref testTypeStringAliasNameDef) @@ (wrap (ref testTypeStringAliasNameDef) $ string "foo"))
        T.string]]
