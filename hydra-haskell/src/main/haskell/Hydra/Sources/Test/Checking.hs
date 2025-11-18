{-# LANGUAGE OverloadedStrings #-}

-- | Type checking test cases for the common test suite
module Hydra.Sources.Test.Checking where

import Hydra.Kernel hiding (map)
import Hydra.Testing
import Hydra.Dsl.Testing
import Hydra.Dsl.TTerms as TTerms
import qualified Hydra.Dsl.TTypes as T
import qualified Hydra.Dsl.Phantoms as Phantoms
import qualified Hydra.Dsl.Core as Core
import Hydra.Sources.Test.TestGraph

import Prelude hiding (map, product, sum)
import qualified Data.List as L
import qualified Data.Map as M


checkingTests :: TTerm TestGroup
checkingTests = supergroup "Type checking tests" [
  annotatedTermsTests,
  applicationsTests,
  eithersTests,
  flowsTests,
  functionsTests,
  letTermsTests,
  listsTests,
  literalsTests,
  mapsTests,
  optionalsTests,
  pairsTests,
  productsTests,
  recordsTests,
  setsTests,
  sumsTests,
  unionsTests,
  unitTests,
  variablesTests,
  wrappedTermsTests,
  failOnUntypedTests]

------ Annotated terms ------

annotatedTermsTests :: TTerm TestGroup
annotatedTermsTests = supergroup "Annotated terms" [
  topLevelAnnotationsTests,
  nestedAnnotationsTests,
  annotationsInComplexContextsTests]

topLevelAnnotationsTests :: TTerm TestGroup
topLevelAnnotationsTests = subgroup "Top-level annotations" [
  noChange "annotated literal"
    (annotated (int32 42) mapTermEmpty)
    T.int32,
  noChange "annotated list"
    (annotated (list [string "a", string "b"]) mapTermEmpty)
    (T.list T.string),
  noChange "annotated record"
    (annotated (record (ref testTypePersonNameDef) [
      "firstName">: string "John",
      "lastName">: string "Doe",
      "age">: int32 25]) mapTermEmpty)
    (Core.typeVariable $ ref testTypePersonNameDef),
  checkTest "annotated lambda" []
    (annotated (lambda "x" $ var "x") mapTermEmpty)
    (annotated (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ var "x") mapTermEmpty)
    (T.forAlls ["t0"] $ T.function (T.var "t0") (T.var "t0"))]

nestedAnnotationsTests :: TTerm TestGroup
nestedAnnotationsTests = subgroup "Nested annotations" [
  noChange "annotation within annotation"
    (annotated (annotated (int32 100) mapTermEmpty) mapTermEmpty)
    T.int32,
  noChange "annotated terms in tuple"
    (tuple [annotated (int32 1) mapTermEmpty,
            annotated (string "hello") mapTermEmpty])
    (T.product [T.int32, T.string]),
  checkTest "annotated term in function application" []
    (annotated (lambda "x" $ var "x") mapTermEmpty @@ annotated (int32 42) mapTermEmpty)
    (annotated (lambdaTyped "x" T.int32 $ var "x") mapTermEmpty @@ annotated (int32 42) mapTermEmpty)
    T.int32]

annotationsInComplexContextsTests :: TTerm TestGroup
annotationsInComplexContextsTests = subgroup "Annotations in complex contexts" [
  checkTest "annotated let binding" []
    (lets ["x">: annotated (int32 5) mapTermEmpty,
           "y">: annotated (string "world") mapTermEmpty] $
      annotated (tuple [var "x", var "y"]) mapTermEmpty)
    (letsTyped [("x", annotated (int32 5) mapTermEmpty, T.mono T.int32),
                ("y", annotated (string "world") mapTermEmpty, T.mono T.string)] $
      annotated (tuple [var "x", var "y"]) mapTermEmpty)
    (T.product [T.int32, T.string]),
  noChange "annotated record fields"
    (record (ref testTypePersonNameDef) [
      "firstName">: annotated (string "Alice") mapTermEmpty,
      "lastName">: annotated (string "Smith") mapTermEmpty,
      "age">: annotated (int32 30) mapTermEmpty])
    (Core.typeVariable $ ref testTypePersonNameDef),
  checkTest "annotated function in application" []
    (lets ["add">: annotated (primitive _math_add) mapTermEmpty] $
      var "add" @@ annotated (int32 10) mapTermEmpty @@ annotated (int32 20) mapTermEmpty)
    (letsTyped [("add", annotated (primitive _math_add) mapTermEmpty, T.mono $ T.function T.int32 (T.function T.int32 T.int32))] $
      var "add" @@ (annotated (int32 10) mapTermEmpty) @@ (annotated (int32 20) mapTermEmpty))
    T.int32]

--    expectTermWithType "annotated function in application"
--      (lets ["add">: annotated (primitive _math_add) M.empty] $
--            var "add" @@ annotated (int32 10) M.empty @@ annotated (int32 20) M.empty)
--      (letsTyped [("add", annotated (primitive _math_add) M.empty, Types.mono $ Types.function Types.int32 (Types.function Types.int32 Types.int32))] $
--        var "add" @@ annotated (int32 10) M.empty @@ annotated (int32 20) M.empty)
--      Types.int32



------ Applications ------

applicationsTests :: TTerm TestGroup
applicationsTests = supergroup "Applications" [
  simpleFunctionApplicationsTests,
  partialApplicationsTests,
  higherOrderApplicationsTests,
  polymorphicApplicationsTests,
  applicationsInComplexContextsTests,
  applicationsWithComplexArgumentsTests]

simpleFunctionApplicationsTests :: TTerm TestGroup
simpleFunctionApplicationsTests = subgroup "Simple function applications" [
  checkTest "identity application" []
    (lambda "x" (var "x") @@ int32 42)
    (lambdaTyped "x" T.int32 (var "x") @@ int32 42)
    T.int32,
  noChange "primitive application"
    (primitive _math_add @@ int32 10 @@ int32 20)
    T.int32,
  noChange "string concatenation"
    (primitive _strings_cat2 @@ string "hello" @@ string "world")
    T.string]

partialApplicationsTests :: TTerm TestGroup
partialApplicationsTests = subgroup "Partial applications" [
  noChange "partially applied add"
    (primitive _math_add @@ int32 5)
    (T.function T.int32 T.int32),
  noChange "partially applied string cat"
    (primitive _strings_cat2 @@ string "prefix")
    (T.function T.string T.string)]

higherOrderApplicationsTests :: TTerm TestGroup
higherOrderApplicationsTests = subgroup "Higher-order applications" [
  checkTest "apply function to function" []
    (lets ["apply">: lambda "f" $ lambda "x" $ var "f" @@ var "x",
           "double">: lambda "n" $ primitive _math_mul @@ var "n" @@ int32 2] $
      var "apply" @@ var "double" @@ int32 5)
    (letsTyped [
      ("apply", tylams ["t0", "t1"] $ lambdaTyped "f" (T.function (T.var "t0") (T.var "t1")) $ lambdaTyped "x" (T.var "t0") $ var "f" @@ var "x",
        T.poly ["t0", "t1"] $ T.function (T.function (T.var "t0") (T.var "t1")) (T.function (T.var "t0") (T.var "t1"))),
      ("double", lambdaTyped "n" T.int32 $ primitive _math_mul @@ var "n" @@ int32 2,
        T.mono $ T.function T.int32 T.int32)] $
      tyapps (var "apply") [T.int32, T.int32] @@ var "double" @@ int32 5)
    T.int32,
  checkTest "function composition" []
    (lets ["compose">: lambda "f" $ lambda "g" $ lambda "x" $ var "f" @@ (var "g" @@ var "x"),
           "add1">: lambda "n" $ primitive _math_add @@ var "n" @@ int32 1,
           "mul2">: lambda "n" $ primitive _math_mul @@ var "n" @@ int32 2] $
      var "compose" @@ var "add1" @@ var "mul2" @@ int32 3)
    (letsTyped [
      ("compose", tylams ["t0", "t1", "t2"] $ lambdaTyped "f" (T.function (T.var "t0") (T.var "t1")) $ lambdaTyped "g" (T.function (T.var "t2") (T.var "t0")) $ lambdaTyped "x" (T.var "t2") $ var "f" @@ (var "g" @@ var "x"),
        T.poly ["t0", "t1", "t2"] $ T.function (T.function (T.var "t0") (T.var "t1")) (T.function (T.function (T.var "t2") (T.var "t0")) (T.function (T.var "t2") (T.var "t1")))),
      ("add1", lambdaTyped "n" T.int32 $ primitive _math_add @@ var "n" @@ int32 1,
        T.mono $ T.function T.int32 T.int32),
      ("mul2", lambdaTyped "n" T.int32 $ primitive _math_mul @@ var "n" @@ int32 2,
        T.mono $ T.function T.int32 T.int32)] $
      tyapps (var "compose") [T.int32, T.int32, T.int32] @@ var "add1" @@ var "mul2" @@ int32 3)
    T.int32]

polymorphicApplicationsTests :: TTerm TestGroup
polymorphicApplicationsTests = subgroup "Polymorphic applications" [
  checkTest "polymorphic identity" []
    (lets ["id">: lambda "x" $ var "x"] $
      tuple [var "id" @@ int32 42, var "id" @@ string "hello"])
    (letsTyped [
      ("id", tylam "t0" $ lambdaTyped "x" (T.var "t0") $ var "x",
        T.poly ["t0"] $ T.function (T.var "t0") (T.var "t0"))] $
      tuple [tyapp (var "id") T.int32 @@ int32 42, tyapp (var "id") T.string @@ string "hello"])
    (T.product [T.int32, T.string]),
  checkTest "polymorphic const" []
    (lets ["const">: lambdas ["x", "y"] $ var "x"] $
      var "const" @@ string "keep" @@ int32 999)
    (letsTyped [
      ("const", tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t1") $ var "x",
        T.poly ["t0", "t1"] $ T.function (T.var "t0") (T.function (T.var "t1") (T.var "t0")))] $
      tyapps (var "const") [T.string, T.int32] @@ string "keep" @@ int32 999)
    T.string,
  checkTest "polymorphic flip" []
    (lets ["flip">: lambda "f" $ lambda "x" $ lambda "y" $ var "f" @@ var "y" @@ var "x"] $
      var "flip" @@ primitive _strings_cat2 @@ string "world" @@ string "hello")
    (letsTyped [
      ("flip", tylams ["t0", "t1", "t2"] $ lambdaTyped "f" (T.function (T.var "t0") (T.function (T.var "t1") (T.var "t2"))) $ lambdaTyped "x" (T.var "t1") $ lambdaTyped "y" (T.var "t0") $ var "f" @@ var "y" @@ var "x",
        T.poly ["t0", "t1", "t2"] $ T.function (T.function (T.var "t0") (T.function (T.var "t1") (T.var "t2"))) (T.function (T.var "t1") (T.function (T.var "t0") (T.var "t2"))))] $
      tyapps (var "flip") [T.string, T.string, T.string] @@ primitive _strings_cat2 @@ string "world" @@ string "hello")
    T.string]

applicationsInComplexContextsTests :: TTerm TestGroup
applicationsInComplexContextsTests = subgroup "Applications in complex contexts" [
  noChange "application in tuple"
    (tuple [primitive _math_add @@ int32 1 @@ int32 2,
            primitive _strings_cat2 @@ string "a" @@ string "b"])
    (T.product [T.int32, T.string]),
  noChange "application in record"
    (record (ref testTypePersonNameDef) [
      "firstName">: primitive _strings_cat2 @@ string "John" @@ string "ny",
      "lastName">: string "Doe",
      "age">: primitive _math_add @@ int32 20 @@ int32 5])
    (Core.typeVariable $ ref testTypePersonNameDef),
  checkTest "application in let binding" []
    (lets ["result">: primitive _math_mul @@ int32 6 @@ int32 7] $
      var "result")
    (letsTyped [
      ("result", primitive _math_mul @@ int32 6 @@ int32 7,
        T.mono T.int32)] $
      var "result")
    T.int32,
  noChange "nested applications"
    (primitive _math_add @@ (primitive _math_mul @@ int32 3 @@ int32 4) @@ (primitive _math_add @@ int32 1 @@ int32 2))
    T.int32]

applicationsWithComplexArgumentsTests :: TTerm TestGroup
applicationsWithComplexArgumentsTests = subgroup "Applications with complex arguments" [
  checkTest "application with record argument" []
    (lets ["getName">: lambda "person" $ project (ref testTypePersonNameDef) (name "firstName") @@ var "person"] $
      var "getName" @@ record (ref testTypePersonNameDef) [
        "firstName">: string "Alice",
        "lastName">: string "Smith",
        "age">: int32 25])
    (letsTyped [
      ("getName", lambdaTyped "person" (Core.typeVariable $ ref testTypePersonNameDef) $ project (ref testTypePersonNameDef) (name "firstName") @@ var "person",
        T.mono $ T.function (Core.typeVariable $ ref testTypePersonNameDef) T.string)] $
      var "getName" @@ record (ref testTypePersonNameDef) [
        "firstName">: string "Alice",
        "lastName">: string "Smith",
        "age">: int32 25])
    T.string,
  checkTest "application with list argument" []
    (lets ["head">: lambda "xs" $ primitive _lists_head @@ var "xs"] $
      var "head" @@ list [string "first", string "second"])
    (letsTyped [
      ("head", tylam "t0" $ lambdaTyped "xs" (T.list (T.var "t0")) $ tyapp (primitive _lists_head) (T.var "t0") @@ var "xs",
        T.poly ["t0"] $ T.function (T.list (T.var "t0")) (T.var "t0"))] $
      tyapp (var "head") T.string @@ list [string "first", string "second"])
    T.string]

------ Eithers ------

eithersTests :: TTerm TestGroup
eithersTests = supergroup "Eithers" [
  leftValuesTests,
  rightValuesTests,
  polymorphicEithersTests,
  eithersInComplexContextsTests,
  nestedEithersTests,
  eithersWithComplexTypesTests]

leftValuesTests :: TTerm TestGroup
leftValuesTests = subgroup "Left values" [
  checkTest "left int" []
    (left $ int32 42)
    (tylam "t0" $ tyapps (left $ int32 42) [T.int32, T.var "t0"])
    (T.forAlls ["t0"] $ T.either_ T.int32 (T.var "t0")),
  checkTest "left string" []
    (left $ string "error")
    (tylam "t0" $ tyapps (left $ string "error") [T.string, T.var "t0"])
    (T.forAlls ["t0"] $ T.either_ T.string (T.var "t0")),
  checkTest "left boolean" []
    (left $ boolean False)
    (tylam "t0" $ tyapps (left $ boolean False) [T.boolean, T.var "t0"])
    (T.forAlls ["t0"] $ T.either_ T.boolean (T.var "t0"))]

rightValuesTests :: TTerm TestGroup
rightValuesTests = subgroup "Right values" [
  checkTest "right int" []
    (right $ int32 42)
    (tylam "t0" $ tyapps (right $ int32 42) [T.var "t0", T.int32])
    (T.forAlls ["t0"] $ T.either_ (T.var "t0") T.int32),
  checkTest "right string" []
    (right $ string "success")
    (tylam "t0" $ tyapps (right $ string "success") [T.var "t0", T.string])
    (T.forAlls ["t0"] $ T.either_ (T.var "t0") T.string),
  checkTest "right boolean" []
    (right $ boolean True)
    (tylam "t0" $ tyapps (right $ boolean True) [T.var "t0", T.boolean])
    (T.forAlls ["t0"] $ T.either_ (T.var "t0") T.boolean)]

polymorphicEithersTests :: TTerm TestGroup
polymorphicEithersTests = subgroup "Polymorphic eithers" [
  checkTest "left from lambda" []
    (lambda "x" $ left $ var "x")
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ tyapps (left $ var "x") [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.either_ (T.var "t0") (T.var "t1"))),
  checkTest "right from lambda" []
    (lambda "x" $ right $ var "x")
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ tyapps (right $ var "x") [T.var "t1", T.var "t0"])
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.either_ (T.var "t1") (T.var "t0"))),
  checkTest "either from two lambdas" []
    (lambda "flag" $ lambda "x" $
      primitive _logic_ifElse @@ var "flag" @@
        (left $ var "x") @@
        (right $ var "x"))
    (tylam "t0" $ lambdaTyped "flag" T.boolean $ lambdaTyped "x" (T.var "t0") $
      tyapp (primitive _logic_ifElse) (T.either_ (T.var "t0") (T.var "t0")) @@ var "flag" @@
        tyapps (left $ var "x") [T.var "t0", T.var "t0"] @@
        tyapps (right $ var "x") [T.var "t0", T.var "t0"])
    (T.forAlls ["t0"] $ T.function T.boolean (T.function (T.var "t0") (T.either_ (T.var "t0") (T.var "t0"))))]

eithersInComplexContextsTests :: TTerm TestGroup
eithersInComplexContextsTests = subgroup "Eithers in complex contexts" [
  checkTest "either in tuple" []
    (tuple [left $ string "error", int32 100])
    (tylam "t0" $ tuple [tyapps (left $ string "error") [T.string, T.var "t0"], int32 100])
    (T.forAlls ["t0"] $ T.product [T.either_ T.string (T.var "t0"), T.int32]),
  checkTest "either in list" []
    (list [left $ string "error", right $ int32 42])
    (list [tyapps (left $ string "error") [T.string, T.int32], tyapps (right $ int32 42) [T.string, T.int32]])
    (T.list $ T.either_ T.string T.int32),
  checkTest "either in let binding" []
    (lets ["result">: right $ int32 42] $
      var "result")
    (tylam "t0" $ letsTyped [("result", tylam "t1" $ tyapps (right $ int32 42) [T.var "t1", T.int32], T.poly ["t1"] $ T.either_ (T.var "t1") T.int32)] $
      tyapp (var "result") (T.var "t0"))
    (T.forAlls ["t0"] $ T.either_ (T.var "t0") T.int32)]

nestedEithersTests :: TTerm TestGroup
nestedEithersTests = subgroup "Nested eithers" [
  checkTest "either of either (left left)" []
    (left $ left $ int32 1)
    (tylams ["t0", "t1"] $ tyapps (left $ tyapps (left $ int32 1) [T.int32, T.var "t0"]) [T.either_ T.int32 (T.var "t0"), T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.either_ (T.either_ T.int32 (T.var "t0")) (T.var "t1")),
  checkTest "either of either (left right)" []
    (left $ right $ string "nested")
    (tylams ["t0", "t1"] $ tyapps (left $ tyapps (right $ string "nested") [T.var "t0", T.string]) [T.either_ (T.var "t0") T.string, T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.either_ (T.either_ (T.var "t0") T.string) (T.var "t1")),
  checkTest "either of either (right)" []
    (right $ boolean True)
    (tylam "t0" $ tyapps (right $ boolean True) [T.var "t0", T.boolean])
    (T.forAlls ["t0"] $ T.either_ (T.var "t0") T.boolean),
  checkTest "either of list" []
    (left $ list [int32 1, int32 2])
    (tylam "t0" $ tyapps (left $ list [int32 1, int32 2]) [T.list T.int32, T.var "t0"])
    (T.forAlls ["t0"] $ T.either_ (T.list T.int32) (T.var "t0")),
  checkTest "list of eithers" []
    (list [left $ string "a", right $ int32 1, left $ string "b"])
    (list [tyapps (left $ string "a") [T.string, T.int32], tyapps (right $ int32 1) [T.string, T.int32], tyapps (left $ string "b") [T.string, T.int32]])
    (T.list $ T.either_ T.string T.int32)]

eithersWithComplexTypesTests :: TTerm TestGroup
eithersWithComplexTypesTests = subgroup "Eithers with complex types" [
  checkTest "either with record on left" []
    (left $ record (ref testTypePersonNameDef) [
      "firstName">: string "Alice",
      "lastName">: string "Smith",
      "age">: int32 30])
    (tylam "t0" $ tyapps (left $ record (ref testTypePersonNameDef) [
      "firstName">: string "Alice",
      "lastName">: string "Smith",
      "age">: int32 30]) [Core.typeVariable $ ref testTypePersonNameDef, T.var "t0"])
    (T.forAlls ["t0"] $ T.either_ (Core.typeVariable $ ref testTypePersonNameDef) (T.var "t0")),
  checkTest "either with record on right" []
    (right $ record (ref testTypePersonNameDef) [
      "firstName">: string "Bob",
      "lastName">: string "Jones",
      "age">: int32 25])
    (tylam "t0" $ tyapps (right $ record (ref testTypePersonNameDef) [
      "firstName">: string "Bob",
      "lastName">: string "Jones",
      "age">: int32 25]) [T.var "t0", Core.typeVariable $ ref testTypePersonNameDef])
    (T.forAlls ["t0"] $ T.either_ (T.var "t0") (Core.typeVariable $ ref testTypePersonNameDef)),
  checkTest "either with tuple" []
    (left $ tuple [string "error", int32 404])
    (tylam "t0" $ tyapps (left $ tuple [string "error", int32 404]) [T.product [T.string, T.int32], T.var "t0"])
    (T.forAlls ["t0"] $ T.either_ (T.product [T.string, T.int32]) (T.var "t0"))]

------ Eliminations ------

eliminationsTests :: TTerm TestGroup
eliminationsTests = supergroup "Eliminations" [
  productEliminationsTests,
  recordEliminationsTests,
  unionEliminationsTests,
  wrapEliminationsTests]

------ Flows ------

flowsTests :: TTerm TestGroup
flowsTests = supergroup "Flows" [
  flowsWithFailureAcrossLetBindingsTests]

flowsWithFailureAcrossLetBindingsTests :: TTerm TestGroup
flowsWithFailureAcrossLetBindingsTests = subgroup "Flows with failure across let bindings" [
  checkTest "mutually referential failure functions with Flow monad" []
    (lets ["conditionalUnexpected">: lambda "s" $ lambda "b" $ lambda "ignored" $
             primitive _logic_ifElse @@ var "b" @@
               (var "unexpected" @@ string "oops") @@
               (var "unexpected" @@ var "s"),
           "unexpected">: lambda "s" $ primitive _flows_fail @@ var "s"] $
      var "conditionalUnexpected")
    (tylams ["t0", "t1", "t2"] $
      letsTyped [
        ("conditionalUnexpected",
         tylams ["t3", "t4", "t5"] $
         lambdaTyped "s" T.string $
         lambdaTyped "b" T.boolean $
         lambdaTyped "ignored" (T.var "t3") $
         tyapp (primitive _logic_ifElse) (T.applys (Core.typeVariable $ name "hydra.compute.Flow") [T.var "t4", T.var "t5"]) @@ var "b" @@
           (tyapps (var "unexpected") [T.var "t4", T.var "t5"] @@ string "oops") @@
           (tyapps (var "unexpected") [T.var "t4", T.var "t5"] @@ var "s"),
         T.poly ["t3", "t4", "t5"] $
           T.function T.string $
           T.function T.boolean $
           T.function (T.var "t3") $
           T.applys (Core.typeVariable $ name "hydra.compute.Flow") [T.var "t4", T.var "t5"]),
        ("unexpected",
         tylams ["t3", "t4"] $
         lambdaTyped "s" T.string $
           tyapps (primitive _flows_fail) [T.var "t3", T.var "t4"] @@ var "s",
         T.poly ["t3", "t4"] $
           T.function T.string $
           T.applys (Core.typeVariable $ name "hydra.compute.Flow") [T.var "t3", T.var "t4"])] $
      tyapps (var "conditionalUnexpected") [T.var "t0", T.var "t1", T.var "t2"])
    (T.forAlls ["t0", "t1", "t2"] $
      T.function T.string $
      T.function T.boolean $
      T.function (T.var "t0") $
      T.applys (Core.typeVariable $ name "hydra.compute.Flow") [T.var "t1", T.var "t2"])]

------ Functions ------

functionsTests :: TTerm TestGroup
functionsTests = supergroup "Functions" [
  eliminationsTests,
  lambdasTests,
  primitivesTests]

------ Lambdas ------

lambdasTests :: TTerm TestGroup
lambdasTests = supergroup "Lambdas" [
  simpleLambdasTests,
  multiParameterLambdasTests,
  lambdasWithOperationsTests,
  nestedLambdasTests,
  lambdasInComplexContextsTests,
  higherOrderLambdasTests]

simpleLambdasTests :: TTerm TestGroup
simpleLambdasTests = subgroup "Simple lambdas" [
  checkTest "identity function" []
    (lambda "x" $ var "x")
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ var "x")
    (T.forAlls ["t0"] $ T.function (T.var "t0") (T.var "t0")),
  checkTest "constant function" []
    (lambda "x" $ int32 42)
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ int32 42)
    (T.forAlls ["t0"] $ T.function (T.var "t0") T.int32)]

multiParameterLambdasTests :: TTerm TestGroup
multiParameterLambdasTests = subgroup "Multi-parameter lambdas" [
  checkTest "two parameters" []
    (lambda "x" $ lambda "y" $ var "x")
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t1") $ var "x")
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.function (T.var "t1") (T.var "t0"))),
  checkTest "three parameters" []
    (lambda "x" $ lambda "y" $ lambda "z" $ var "y")
    (tylams ["t0", "t1", "t2"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t1") $ lambdaTyped "z" (T.var "t2") $ var "y")
    (T.forAlls ["t0", "t1", "t2"] $ T.function
      (T.var "t0")
      (T.function (T.var "t1") (T.function (T.var "t2") (T.var "t1")))),
  checkTest "parameter reuse" []
    (lambda "x" $ lambda "y" $ tuple [var "x", var "x", var "y"])
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t1") $ tuple [var "x", var "x", var "y"])
    (T.forAlls ["t0", "t1"] $ T.function
      (T.var "t0") (T.function (T.var "t1")
      (T.product [T.var "t0", T.var "t0", T.var "t1"])))]

lambdasWithOperationsTests :: TTerm TestGroup
lambdasWithOperationsTests = subgroup "Lambdas with operations" [
  checkTest "lambda with primitive" []
    (lambda "x" $ primitive _math_add @@ var "x" @@ int32 1)
    (lambdaTyped "x" T.int32 $ primitive _math_add @@ var "x" @@ int32 1)
    (T.function T.int32 T.int32),
  checkTest "lambda with application" []
    (lambda "f" $ lambda "x" $ var "f" @@ var "x")
    (tylams ["t0", "t1"] $ lambdaTyped "f" (T.function (T.var "t0") (T.var "t1")) $ lambdaTyped "x" (T.var "t0") $ var "f" @@ var "x")
    (T.forAlls ["t0", "t1"] $ T.function (T.function (T.var "t0") (T.var "t1")) (T.function (T.var "t0") (T.var "t1"))),
  checkTest "lambda with construction" []
    (lambda "x" $ lambda "y" $ tuple [var "x", var "y"])
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t1") $ tuple [var "x", var "y"])
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.function (T.var "t1") (T.product [T.var "t0", T.var "t1"])))]

nestedLambdasTests :: TTerm TestGroup
nestedLambdasTests = subgroup "Nested lambdas" [
  checkTest "lambda returning lambda" []
    (lambda "x" $ lambda "y" $ lambda "z" $ var "x")
    (tylams ["t0", "t1", "t2"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t1") $ lambdaTyped "z" (T.var "t2") $ var "x")
    (T.forAlls ["t0", "t1", "t2"] $ T.function (T.var "t0") (T.function (T.var "t1") (T.function (T.var "t2") (T.var "t0")))),
  checkTest "lambda with let binding" []
    (lambda "x" $ lets ["y">: var "x"] $ var "y")
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ letsTyped [("y", var "x", T.mono (T.var "t0"))] $ var "y")
    (T.forAlls ["t0"] $ T.function (T.var "t0") (T.var "t0")),
  checkTest "lambda with inner lambda" []
    (lambda "outer" $ lets ["inner">: lambda "x" $ var "x"] $ var "inner" @@ var "outer")
    (tylam "t0" $ lambdaTyped "outer" (T.var "t0") $ letsTyped [("inner", tylam "t1" $ lambdaTyped "x" (T.var "t1") $ var "x", T.poly ["t1"] $ T.function (T.var "t1") (T.var "t1"))] $ tyapp (var "inner") (T.var "t0") @@ var "outer")
    (T.forAlls ["t0"] $ T.function (T.var "t0") (T.var "t0"))]

lambdasInComplexContextsTests :: TTerm TestGroup
lambdasInComplexContextsTests = subgroup "Lambdas in complex contexts" [
  checkTest "lambda in tuple" []
    (tuple [lambda "x" $ var "x", int32 42])
    (tylam "t0" $ tuple [lambdaTyped "x" (T.var "t0") $ var "x", int32 42])
    (T.forAlls ["t0"] $ T.product [T.function (T.var "t0") (T.var "t0"), T.int32]),
  checkTest "lambda in list" []
    (list [lambda "x" $ primitive _math_add @@ var "x" @@ int32 1,
           lambda "y" $ primitive _math_mul @@ var "y" @@ int32 2])
    (list [lambdaTyped "x" T.int32 $ primitive _math_add @@ var "x" @@ int32 1,
           lambdaTyped "y" T.int32 $ primitive _math_mul @@ var "y" @@ int32 2])
    (T.list $ T.function T.int32 T.int32),
  checkTest "lambda in record" []
    (lambda "name" $ record (ref testTypePersonNameDef) [
      "firstName">: var "name",
      "lastName">: string "Doe",
      "age">: int32 30])
    (lambdaTyped "name" T.string $ record (ref testTypePersonNameDef) [
      "firstName">: var "name",
      "lastName">: string "Doe",
      "age">: int32 30])
    (T.function T.string (Core.typeVariable $ ref testTypePersonNameDef))]

higherOrderLambdasTests :: TTerm TestGroup
higherOrderLambdasTests = subgroup "Higher-order lambdas" [
  checkTest "function composition" []
    (lambda "f" $ lambda "g" $ lambda "x" $ var "f" @@ (var "g" @@ var "x"))
    (tylams ["t0", "t1", "t2"] $ lambdaTyped "f" (T.function (T.var "t0") (T.var "t1")) $ lambdaTyped "g" (T.function (T.var "t2") (T.var "t0")) $ lambdaTyped "x" (T.var "t2") $ var "f" @@ (var "g" @@ var "x"))
    (T.forAlls ["t0", "t1", "t2"] $ T.function
      (T.function (T.var "t0") (T.var "t1"))
      (T.function
        (T.function (T.var "t2") (T.var "t0"))
        (T.function (T.var "t2") (T.var "t1")))),
  checkTest "function application" []
    (lambda "f" $ lambda "x" $ var "f" @@ var "x")
    (tylams ["t0", "t1"] $ lambdaTyped "f" (T.function (T.var "t0") (T.var "t1")) $ lambdaTyped "x" (T.var "t0") $ var "f" @@ var "x")
    (T.forAlls ["t0", "t1"] $ T.function (T.function (T.var "t0") (T.var "t1")) (T.function (T.var "t0") (T.var "t1"))),
  checkTest "curried function" []
    (lambda "x" $ lambda "y" $ lambda "z" $ primitive _logic_ifElse @@ var "x" @@ var "y" @@ var "z")
    (tylam "t0" $ lambdaTyped "x" T.boolean $ lambdaTyped "y" (T.var "t0") $ lambdaTyped "z" (T.var "t0") $ tyapp (primitive _logic_ifElse) (T.var "t0") @@ var "x" @@ var "y" @@ var "z")
    (T.forAlls ["t0"] $ T.function T.boolean (T.function (T.var "t0") (T.function (T.var "t0") (T.var "t0"))))]

------ Let terms ------

letTermsTests :: TTerm TestGroup
letTermsTests = supergroup "Let terms" [
  simpleLetBindingsTests,
  letTermsWithShadowingTests,
  recursiveBindingsTests,
  mutualRecursionTests,
  nestedLetTermsTests,
  letWithComplexExpressionsTests]

simpleLetBindingsTests :: TTerm TestGroup
simpleLetBindingsTests = subgroup "Simple let bindings" [
  checkTest "single binding" []
    (lets ["x">: int32 42] $
          var "x")
    (letsTyped [("x", int32 42, T.mono T.int32)] $
      var "x")
    T.int32,
  checkTest "multiple bindings" []
    (lets ["x">: int32 42,
           "y">: string "hello"] $
          tuple [var "x", var "y"])
    (letsTyped [("x", int32 42, T.mono T.int32),
                ("y", string "hello", T.mono T.string)] $
      tuple [var "x", var "y"])
    (T.product [T.int32, T.string])]

letTermsWithShadowingTests :: TTerm TestGroup
letTermsWithShadowingTests = subgroup "Let terms with shadowing" [
  checkTest "lambda parameter shadowing let binding" []
    (lets ["x">: int32 42] $
      lambda "x" $ var "x")
    (tylam "t0" $
      letsTyped [("x", int32 42, T.mono T.int32)] $
        lambdaTyped "x" (T.var "t0") $ var "x")
    (T.forAlls ["t0"] $ T.function (T.var "t0") (T.var "t0")),
  checkTest "nested lambda shadowing" []
    (lambda "x" $
      lets ["y">: var "x"] $
        lambda "x" $
          tuple [var "x", var "y"])
    (tylams ["t0", "t1"] $
      lambdaTyped "x" (T.var "t0") $
        letsTyped [("y", var "x", T.mono $ T.var "t0")] $
          lambdaTyped "x" (T.var "t1") $
            tuple [var "x", var "y"])
    (T.forAlls ["t0", "t1"] $
      T.function (T.var "t0") $
        T.function (T.var "t1") $
          T.product [T.var "t1", T.var "t0"]),
  checkTest "multiple levels of let shadowing" []
    (lets ["x">: int32 1] $
      lets ["x">: string "second"] $
        lets ["x">: boolean True] $
          var "x")
    (letsTyped [("x", int32 1, T.mono T.int32)] $
      letsTyped [("x", string "second", T.mono T.string)] $
        letsTyped [("x", boolean True, T.mono T.boolean)] $
          var "x")
    T.boolean,
  checkTest "let shadowing with lambda and reference to outer binding" []
    (lets ["x">: int32 10, "y">: int32 20] $
      lambda "x" $
        lets ["z">: var "y"] $
          tuple [var "x", var "z"])
    (tylam "t0" $
      letsTyped [("x", int32 10, T.mono T.int32),
                 ("y", int32 20, T.mono T.int32)] $
        lambdaTyped "x" (T.var "t0") $
          letsTyped [("z", var "y", T.mono T.int32)] $
            tuple [var "x", var "z"])
    (T.forAlls ["t0"] $
      T.function (T.var "t0") $
        T.product [T.var "t0", T.int32])]

recursiveBindingsTests :: TTerm TestGroup
recursiveBindingsTests = subgroup "Recursive bindings" [
  checkTest "simple arithmetic recursion" []
    (lets ["double">: lambda "n" $ primitive _math_add @@ var "n" @@ var "n"] $
          var "double" @@ int32 5)
    (letsTyped [("double", lambdaTyped "n" T.int32 $ primitive _math_add @@ var "n" @@ var "n",
                 T.mono $ T.function T.int32 T.int32)] $
      var "double" @@ int32 5)
    T.int32]

mutualRecursionTests :: TTerm TestGroup
mutualRecursionTests = subgroup "Mutual recursion" [
  checkTest "mutually recursive data" []
    (lets ["listA">: record (ref testTypeBuddyListANameDef) [
             "head">: int32 1,
             "tail">: optional $ just $ var "listB"],
           "listB">: record (ref testTypeBuddyListBNameDef) [
             "head">: int32 2,
             "tail">: optional nothing]] $
          var "listA")
    (letsTyped [("listA", tyapp (record (ref testTypeBuddyListANameDef) [
                   "head">: int32 1,
                   "tail">: optional $ just $ var "listB"]) T.int32,
                 T.mono $ T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) T.int32),
                ("listB", tyapp (record (ref testTypeBuddyListBNameDef) [
                   "head">: int32 2,
                   "tail">: tyapp (optional nothing) (T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) T.int32)]) T.int32,
                 T.mono $ T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) T.int32)] $
      var "listA")
    (T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) T.int32),
  checkTest "(monomorphic) mutually recursive functions" []
    (lets ["f">: lambda "x" $ var "g" @@ var "x",
           "g">: lambda "y" $ primitive _math_add @@ var "y" @@ int32 1] $
          var "f" @@ int32 5)
    (letsTyped [("f", lambdaTyped "x" T.int32 $ var "g" @@ var "x",
                 T.mono $ T.function T.int32 T.int32),
                ("g", lambdaTyped "y" T.int32 $ primitive _math_add @@ var "y" @@ int32 1,
                 T.mono $ T.function T.int32 T.int32)] $
      var "f" @@ int32 5)
    T.int32]

nestedLetTermsTests :: TTerm TestGroup
nestedLetTermsTests = subgroup "Nested let terms" [
  checkTest "monomorphic nesting" []
    (lets ["x">: int32 1] $
     lets ["y">: primitive _math_add @@ var "x" @@ int32 2] $
     lets ["z">: primitive _math_mul @@ var "y" @@ int32 3] $
      var "z")
    (letsTyped [("x", int32 1, T.mono T.int32)] $
     letsTyped [("y", primitive _math_add @@ var "x" @@ int32 2, T.mono T.int32)] $
     letsTyped [("z", primitive _math_mul @@ var "y" @@ int32 3, T.mono T.int32)] $
      var "z")
    T.int32,
  checkTest "polymorphic nesting" []
    (lets ["id">: lambda "x" $ var "x"] $
     lets ["apply">: lambda "f" $ lambda "x" $ var "f" @@ var "x"] $
      var "apply" @@ var "id" @@ string "test")
    (letsTyped [("id", tylam "t0" $ lambdaTyped "x" (T.var "t0") $ var "x",
                 T.poly ["t0"] $ T.function (T.var "t0") (T.var "t0"))] $
     letsTyped [("apply", tylams ["t0", "t1"] $ lambdaTyped "f" (T.function (T.var "t0") (T.var "t1")) $ lambdaTyped "x" (T.var "t0") $ var "f" @@ var "x",
                 T.poly ["t0", "t1"] $ T.function (T.function (T.var "t0") (T.var "t1")) (T.function (T.var "t0") (T.var "t1")))] $
      tyapps (var "apply") [T.string, T.string] @@ tyapp (var "id") T.string @@ string "test")
    T.string,
  checkTest "variable capture avoidance" []
    (lets ["x">: int32 1] $
      lambda "x" $ lets ["y">: var "x"] $ var "y")
    (tylam "t0" $ letsTyped [("x", int32 1, T.mono T.int32)] $
      lambdaTyped "x" (T.var "t0") $ letsTyped [("y", var "x", T.mono (T.var "t0"))] $ var "y")
    (T.forAlls ["t0"] $ T.function (T.var "t0") (T.var "t0")),
  checkTest "simple let in lambda" []
    (lambda "z" $ lets ["y">: var "z"] $ var "y")
    (tylam "t0" $ lambdaTyped "z" (T.var "t0") $ letsTyped [("y", var "z", T.mono (T.var "t0"))] $ var "y")
    (T.forAlls ["t0"] $ T.function (T.var "t0") (T.var "t0"))]

letWithComplexExpressionsTests :: TTerm TestGroup
letWithComplexExpressionsTests = subgroup "Let with complex expressions" [
  checkTest "let in record" []
    (record (ref testTypePersonNameDef) [
      "firstName">: lets ["first">: string "John",
                              "middle">: string "Q"] $
                             primitive _strings_cat2 @@ var "first" @@ var "middle",
      "lastName">: string "Doe",
      "age">: int32 30])
    (record (ref testTypePersonNameDef) [
      "firstName">: letsTyped [("first", string "John", T.mono T.string),
                                   ("middle", string "Q", T.mono T.string)] $
                         primitive _strings_cat2 @@ var "first" @@ var "middle",
      "lastName">: string "Doe",
      "age">: int32 30])
    (Core.typeVariable $ ref testTypePersonNameDef),
  checkTest "let in function application" []
    (lets ["x">: int32 5,
           "y">: int32 3] $
      primitive _math_add @@ var "x" @@ var "y")
    (letsTyped [("x", int32 5, T.mono T.int32),
                ("y", int32 3, T.mono T.int32)] $
      primitive _math_add @@ var "x" @@ var "y")
    T.int32,
  checkTest "polymorphic let binding" []
    (lets ["id">: lambda "x" $ var "x"] $
      tuple [var "id" @@ int32 42, var "id" @@ string "hello"])
    (letsTyped [("id", tylam "t0" $ lambdaTyped "x" (T.var "t0") $ var "x",
                 T.poly ["t0"] $ T.function (T.var "t0") (T.var "t0"))] $
      tuple [tyapp (var "id") T.int32 @@ int32 42, tyapp (var "id") T.string @@ string "hello"])
    (T.product [T.int32, T.string]),
  checkTest "composition" []
    (lets ["compose">: lambda "f" $ lambda "g" $ lambda "x" $ var "f" @@ (var "g" @@ var "x"),
           "add1">: lambda "n" $ primitive _math_add @@ var "n" @@ int32 1,
           "double">: lambda "n" $ primitive _math_mul @@ var "n" @@ int32 2] $
      (var "compose" @@ var "add1" @@ var "double") @@ int32 5)
    (letsTyped [
      ("compose", tylams ["t0", "t1", "t2"] $
        lambdaTyped "f" (T.function (T.var "t0") (T.var "t1")) $
          lambdaTyped "g" (T.function (T.var "t2") (T.var "t0")) $
            lambdaTyped "x" (T.var "t2") $ var "f" @@ (var "g" @@ var "x"),
        T.poly ["t0", "t1", "t2"] $
          T.function
            (T.function (T.var "t0") (T.var "t1"))
            (T.function (T.function (T.var "t2") (T.var "t0"))
              (T.function (T.var "t2") (T.var "t1")))),
      ("add1", lambdaTyped "n" T.int32 $ primitive _math_add @@ var "n" @@ int32 1,
       T.mono $ T.function T.int32 T.int32),
      ("double", lambdaTyped "n" T.int32 $ primitive _math_mul @@ var "n" @@ int32 2,
       T.mono $ T.function T.int32 T.int32)] $
      (tyapps (var "compose") [T.int32, T.int32, T.int32] @@ var "add1" @@ var "double") @@ int32 5)
    T.int32]

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

------ Literals ------

literalsTests :: TTerm TestGroup
literalsTests = supergroup "Literals" [
  booleanLiteralsTests,
  stringLiteralsTests,
  integerLiteralsTests,
  floatLiteralsTests,
--  binaryLiteralsTests, -- TODO: restore this group
  literalsInComplexContextsTests]

booleanLiteralsTests :: TTerm TestGroup
booleanLiteralsTests = subgroup "Boolean literals" [
  noChange "true" (boolean True) T.boolean,
  noChange "false" (boolean False) T.boolean]

stringLiteralsTests :: TTerm TestGroup
stringLiteralsTests = subgroup "String literals" [
  noChange "simple string" (string "hello") T.string,
  noChange "empty string" (string "") T.string,
  noChange "unicode string" (string "café") T.string]

integerLiteralsTests :: TTerm TestGroup
integerLiteralsTests = subgroup "Integer literals" [
  noChange "bigint" (bigint 42) T.bigint,
  noChange "int8" (int8 127) T.int8,
  noChange "int16" (int16 32767) T.int16,
  noChange "int32" (int32 2147483647) T.int32,
  noChange "int64" (int64 9223372036854775807) T.int64,
  noChange "uint8" (uint8 255) T.uint8,
  noChange "uint16" (uint16 65535) T.uint16,
  noChange "uint32" (uint32 4294967295) T.uint32,
  noChange "uint64" (uint64 18446744073709551615) T.uint64]

floatLiteralsTests :: TTerm TestGroup
floatLiteralsTests = subgroup "Float literals" [
  noChange "bigfloat" (bigfloat 3.14159) T.bigfloat,
  noChange "float32" (float32 2.71828) T.float32,
  noChange "float64" (float64 1.41421) T.float64]

binaryLiteralsTests :: TTerm TestGroup
binaryLiteralsTests = subgroup "Binary literals" [
  noChange "binary" (binary "SGVsbG8gV29ybGQ=") T.binary]

literalsInComplexContextsTests :: TTerm TestGroup
literalsInComplexContextsTests = subgroup "Literals in complex contexts" [
  noChange "literals in tuple"
    (tuple [boolean True, string "test", int32 42, float32 3.14])
    (T.product [T.boolean, T.string, T.int32, T.float32]),
  noChange "literals in list"
    (list [string "one", string "two", string "three"])
    (T.list T.string)]

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

------ Optionals ------

optionalsTests :: TTerm TestGroup
optionalsTests = supergroup "Optionals" [
  monomorphicOptionalsTests,
  polymorphicOptionalsTests,
  optionalsInComplexContextsTests,
  nestedOptionalsTests,
  optionalsWithComplexTypesTests]

monomorphicOptionalsTests :: TTerm TestGroup
monomorphicOptionalsTests = subgroup "Monomorphic optionals" [
  checkTest "nothing" []
    (optional nothing)
    (tylam "t0" $ tyapp (optional nothing) (T.var "t0"))
    (T.forAll "t0" $ T.optional $ T.var "t0"),
  noChange "just int"
    (optional $ just $ int32 42)
    (T.optional T.int32),
  noChange "just string"
    (optional $ just $ string "hello")
    (T.optional T.string),
  noChange "just boolean"
    (optional $ just $ boolean True)
    (T.optional T.boolean)]

polymorphicOptionalsTests :: TTerm TestGroup
polymorphicOptionalsTests = subgroup "Polymorphic optionals" [
  checkTest "optional from lambda" []
    (lambda "x" $ optional $ just $ var "x")
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ optional $ just $ var "x")
    (T.forAll "t0" $ T.function (T.var "t0") (T.optional $ T.var "t0")),
  checkTest "nothing from lambda" []
    (lambda "x" $ optional nothing)
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ tyapp (optional nothing) (T.var "t1"))
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.optional $ T.var "t1")),
  checkTest "conditional optional" []
    (lambda "x" $ lambda "flag" $
      primitive _logic_ifElse @@ var "flag" @@
        (optional $ just $ var "x") @@
        (optional nothing))
    (tylams ["t0"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "flag" T.boolean $
      tyapp (primitive _logic_ifElse) (T.optional $ T.var "t0") @@ var "flag" @@
        (optional $ just $ var "x") @@
        (tyapp (optional nothing) (T.var "t0")))
    (T.forAlls ["t0"] $ T.function (T.var "t0") (T.function T.boolean (T.optional $ T.var "t0")))]

optionalsInComplexContextsTests :: TTerm TestGroup
optionalsInComplexContextsTests = subgroup "Optionals in complex contexts" [
  noChange "optional in tuple"
    (tuple [optional $ just $ int32 100, string "context"])
    (T.product [T.optional T.int32, T.string]),
  checkTest "optional in record" []
    (record (ref testTypeBuddyListANameDef) [
      "head">: string "first",
      "tail">: optional $ just $ record (ref testTypeBuddyListBNameDef) [
        "head">: string "second",
        "tail">: optional nothing]])
    (tyapp (record (ref testTypeBuddyListANameDef) [
      "head">: string "first",
      "tail">: optional $ just $ tyapp (record (ref testTypeBuddyListBNameDef) [
        "head">: string "second",
        "tail">: tyapp (optional nothing) (T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) T.string)]) T.string]) T.string)
    (T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) T.string),
  checkTest "optional in let binding" []
    (lets ["maybeValue" >: optional $ just $ int32 42] $
      var "maybeValue")
    (letsTyped [("maybeValue", optional $ just $ int32 42, T.mono $ T.optional T.int32)] $
      var "maybeValue")
    (T.optional T.int32)]

nestedOptionalsTests :: TTerm TestGroup
nestedOptionalsTests = subgroup "Nested optionals" [
  noChange "optional of optional"
    (optional $ just $ optional $ just $ string "nested")
    (T.optional $ T.optional T.string),
  noChange "optional of list"
    (optional $ just $ list [int32 1, int32 2, int32 3])
    (T.optional $ T.list T.int32),
  checkTest "list of optionals" []
    (list [optional $ just $ string "a", optional nothing, optional $ just $ string "b"])
    (list [optional $ just $ string "a", tyapp (optional nothing) T.string, optional $ just $ string "b"])
    (T.list $ T.optional T.string)]

optionalsWithComplexTypesTests :: TTerm TestGroup
optionalsWithComplexTypesTests = subgroup "Optionals with complex types" [
  noChange "optional tuple"
    (optional $ just $ tuple [int32 10, string "test"])
    (T.optional $ T.product [T.int32, T.string]),
  noChange "optional map"
    (optional $ just $ mapTerm [(string "key", int32 42)])
    (T.optional $ T.map T.string T.int32)]

------ Pairs ------

pairsTests :: TTerm TestGroup
pairsTests = supergroup "Pairs" [
  basicPairsTests,
  polymorphicPairsTests,
  pairsInComplexContextsTests,
  nestedPairsTests,
  pairsWithComplexTypesTests]

basicPairsTests :: TTerm TestGroup
basicPairsTests = subgroup "Basic pairs" [
  checkTest "pair of int and string" []
    (pair (int32 42) (string "hello"))
    (tyapps (pair (int32 42) (string "hello")) [T.int32, T.string])
    (T.pair T.int32 T.string),
  checkTest "pair of string and boolean" []
    (pair (string "test") (boolean True))
    (tyapps (pair (string "test") (boolean True)) [T.string, T.boolean])
    (T.pair T.string T.boolean),
  checkTest "pair of boolean and int" []
    (pair (boolean False) (int32 100))
    (tyapps (pair (boolean False) (int32 100)) [T.boolean, T.int32])
    (T.pair T.boolean T.int32)]

polymorphicPairsTests :: TTerm TestGroup
polymorphicPairsTests = subgroup "Polymorphic pairs" [
  checkTest "pair from lambda (first element)" []
    (lambda "x" $ pair (var "x") (string "constant"))
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapps (pair (var "x") (string "constant")) [T.var "t0", T.string])
    (T.forAll "t0" $ T.function (T.var "t0") (T.pair (T.var "t0") T.string)),
  checkTest "pair from lambda (second element)" []
    (lambda "x" $ pair (string "constant") (var "x"))
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapps (pair (string "constant") (var "x")) [T.string, T.var "t0"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.pair T.string (T.var "t0"))),
  checkTest "pair from two lambdas" []
    (lambda "x" $ lambda "y" $ pair (var "x") (var "y"))
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t1") $ tyapps (pair (var "x") (var "y")) [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.function (T.var "t1") (T.pair (T.var "t0") (T.var "t1")))),
  checkTest "pair with repeated variable" []
    (lambda "x" $ pair (var "x") (var "x"))
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapps (pair (var "x") (var "x")) [T.var "t0", T.var "t0"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.pair (T.var "t0") (T.var "t0")))]

pairsInComplexContextsTests :: TTerm TestGroup
pairsInComplexContextsTests = subgroup "Pairs in complex contexts" [
  checkTest "pair in tuple" []
    (tuple [pair (int32 1) (string "one"), boolean True])
    (tuple [tyapps (pair (int32 1) (string "one")) [T.int32, T.string], boolean True])
    (T.product [T.pair T.int32 T.string, T.boolean]),
  checkTest "pair in list" []
    (list [pair (int32 1) (string "one"), pair (int32 2) (string "two")])
    (list [tyapps (pair (int32 1) (string "one")) [T.int32, T.string], tyapps (pair (int32 2) (string "two")) [T.int32, T.string]])
    (T.list $ T.pair T.int32 T.string),
  checkTest "pair in let binding" []
    (lets ["result" >: pair (int32 42) (string "answer")] $
      var "result")
    (letsTyped [("result", tyapps (pair (int32 42) (string "answer")) [T.int32, T.string], T.mono $ T.pair T.int32 T.string)] $
      var "result")
    (T.pair T.int32 T.string)]

nestedPairsTests :: TTerm TestGroup
nestedPairsTests = subgroup "Nested pairs" [
  checkTest "pair of pairs" []
    (pair (pair (int32 1) (string "one")) (pair (boolean True) (int32 2)))
    (tyapps (pair (tyapps (pair (int32 1) (string "one")) [T.int32, T.string]) (tyapps (pair (boolean True) (int32 2)) [T.boolean, T.int32])) [T.pair T.int32 T.string, T.pair T.boolean T.int32])
    (T.pair (T.pair T.int32 T.string) (T.pair T.boolean T.int32)),
  checkTest "pair with list" []
    (pair (list [int32 1, int32 2]) (string "numbers"))
    (tyapps (pair (list [int32 1, int32 2]) (string "numbers")) [T.list T.int32, T.string])
    (T.pair (T.list T.int32) T.string),
  checkTest "list of pairs" []
    (list [pair (int32 1) (string "a"), pair (int32 2) (string "b")])
    (list [tyapps (pair (int32 1) (string "a")) [T.int32, T.string], tyapps (pair (int32 2) (string "b")) [T.int32, T.string]])
    (T.list $ T.pair T.int32 T.string)]

personRecord :: String -> String -> Int -> TTerm Term
personRecord fName lName age' =
  record (name "Person") [
    "firstName" >: string fName,
    "lastName" >: string lName,
    "age" >: int32 age']

pairsWithComplexTypesTests :: TTerm TestGroup
pairsWithComplexTypesTests = subgroup "Pairs with complex types" [
  checkTest "pair with record on first" []
    (pair (personRecord "Alice" "Smith" 30) (int32 1))
    (tyapps (pair (personRecord "Alice" "Smith" 30) (int32 1)) [T.var "Person", T.int32])
    (T.pair (T.var "Person") T.int32),
  checkTest "pair with record on second" []
    (pair (string "name") (personRecord "Bob" "Jones" 25))
    (tyapps (pair (string "name") (personRecord "Bob" "Jones" 25)) [T.string, T.var "Person"])
    (T.pair T.string (T.var "Person")),
  checkTest "pair of tuple and list" []
    (pair (tuple [int32 1, int32 2]) (list [string "a", string "b"]))
    (tyapps (pair (tuple [int32 1, int32 2]) (list [string "a", string "b"])) [T.product [T.int32, T.int32], T.list T.string])
    (T.pair (T.product [T.int32, T.int32]) (T.list T.string))]

------ Products ------

productsTests :: TTerm TestGroup
productsTests = supergroup "Products" [
  monomorphicProductsTests,
  polymorphicProductsTests,
  nestedProductsTests]

monomorphicProductsTests :: TTerm TestGroup
monomorphicProductsTests = subgroup "Monomorphic products" [
  noChange "empty tuple"
    (tuple [])
    (T.product []),
  noChange "singleton tuple"
    (tuple [int32 42])
    (T.product [T.int32]),
  noChange "pair tuple"
    (tuple [int32 42, string "foo"])
    (T.product [T.int32, T.string]),
  noChange "triple tuple"
    (tuple [int32 1, int32 2, int32 3])
    (T.product [T.int32, T.int32, T.int32]),
  noChange "mixed types"
    (tuple [unit, string "test", bigint 100])
    (T.product [T.unit, T.string, T.bigint])]

polymorphicProductsTests :: TTerm TestGroup
polymorphicProductsTests = subgroup "Polymorphic products" [
  checkTest "lambda with var" []
    (lambda "x" $ tuple [var "x", string "foo"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tuple [var "x", string "foo"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.product [T.var "t0", T.string])),
  checkTest "two variables" []
    (lambda "x" $ lambda "y" $ tuple [var "x", var "y"])
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t1") $ tuple [var "x", var "y"])
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.function (T.var "t1") (T.product [T.var "t0", T.var "t1"]))),
  checkTest "repeated variable" []
    (lambda "x" $ tuple [var "x", var "x"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tuple [var "x", var "x"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.product [T.var "t0", T.var "t0"]))]

nestedProductsTests :: TTerm TestGroup
nestedProductsTests = subgroup "Nested products" [
  noChange "tuple in tuple"
    (tuple [tuple [int32 1], string "foo"])
    (T.product [T.product [T.int32], T.string]),
  checkTest "nested polymorphic" []
    (lambda "x" $ tuple [tuple [var "x"], tuple [string "test"]])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tuple [tuple [var "x"], tuple [string "test"]])
    (T.forAll "t0" $ T.function (T.var "t0") (T.product [T.product [T.var "t0"], T.product [T.string]]))]

------ Primitives ------

primitivesTests :: TTerm TestGroup
primitivesTests = supergroup "Primitives" [
  nullaryPrimitivesTests,
  unaryPrimitivesTests,
  binaryPrimitivesTests,
  ternaryPrimitivesTests,
  monomorphicVsPolymorphicTests,
  higherOrderPrimitivesTests,
  primitivesInComplexContextsTests]

nullaryPrimitivesTests :: TTerm TestGroup
nullaryPrimitivesTests = subgroup "Nullary primitives" [
  checkTest "empty map" []
    (primitive _maps_empty)
    (tylams ["t0", "t1"] $ tyapps (primitive _maps_empty) [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.map (T.var "t0") (T.var "t1")),
  checkTest "empty set" []
    (primitive _sets_empty)
    (tylam "t0" $ tyapp (primitive _sets_empty) (T.var "t0"))
    (T.forAll "t0" $ T.set $ T.var "t0")]

unaryPrimitivesTests :: TTerm TestGroup
unaryPrimitivesTests = subgroup "Unary primitives" [
  checkTest "lists head" []
    (primitive _lists_head)
    (tylam "t0" $ tyapp (primitive _lists_head) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.list $ T.var "t0") (T.var "t0")),
  noChange "math neg"
    (primitive _math_negate)
    (T.function T.int32 T.int32),
  noChange "logic not"
    (primitive _logic_not)
    (T.function T.boolean T.boolean)]

binaryPrimitivesTests :: TTerm TestGroup
binaryPrimitivesTests = subgroup "Binary primitives" [
  noChange "math add"
    (primitive _math_add)
    (T.function T.int32 (T.function T.int32 T.int32)),
  checkTest "lists cons" []
    (primitive _lists_cons)
    (tylam "t0" $ tyapp (primitive _lists_cons) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.function (T.list $ T.var "t0") (T.list $ T.var "t0"))),
  checkTest "maps insert" []
    (primitive _maps_insert)
    (tylams ["t0", "t1"] $ tyapps (primitive _maps_insert) [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.function
      (T.var "t0")
      (T.function (T.var "t1") (T.function (T.map (T.var "t0") (T.var "t1")) (T.map (T.var "t0") (T.var "t1")))))]

ternaryPrimitivesTests :: TTerm TestGroup
ternaryPrimitivesTests = subgroup "Ternary primitives" [
  checkTest "logic ifElse" []
    (primitive _logic_ifElse)
    (tylam "t0" $ tyapp (primitive _logic_ifElse) (T.var "t0"))
    (T.forAll "t0" $ T.function T.boolean (T.function (T.var "t0") (T.function (T.var "t0") (T.var "t0")))),
  checkTest "lists foldl" []
    (primitive _lists_foldl)
    (tylams ["t0", "t1"] $ tyapps (primitive _lists_foldl) [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.function
      (T.function (T.var "t0") (T.function (T.var "t1") (T.var "t0")))
      (T.function (T.var "t0") (T.function (T.list $ T.var "t1") (T.var "t0"))))]

monomorphicVsPolymorphicTests :: TTerm TestGroup
monomorphicVsPolymorphicTests = subgroup "Monomorphic vs polymorphic" [
  noChange "monomorphic math"
    (primitive _math_add)
    (T.function T.int32 (T.function T.int32 T.int32)),
  checkTest "polymorphic identity" []
    (primitive _equality_identity)
    (tylam "t0" $ tyapp (primitive _equality_identity) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.var "t0")),
  checkTest "polymorphic map" []
    (primitive _lists_map)
    (tylams ["t0", "t1"] $ tyapps (primitive _lists_map) [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.function
      (T.function (T.var "t0") (T.var "t1"))
      (T.function (T.list $ T.var "t0") (T.list $ T.var "t1")))]

higherOrderPrimitivesTests :: TTerm TestGroup
higherOrderPrimitivesTests = subgroup "Higher-order primitives" [
  checkTest "lists map function" []
    (primitive _lists_map @@ (lambda "x" $ primitive _math_add @@ var "x" @@ int32 1))
    (tyapps (primitive _lists_map) [T.int32, T.int32] @@ (lambdaTyped "x" T.int32 $ primitive _math_add @@ var "x" @@ int32 1))
    (T.function (T.list T.int32) (T.list T.int32)),
  checkTest "lists filter" []
    (primitive _lists_filter)
    (tylam "t0" $ tyapp (primitive _lists_filter) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.function (T.var "t0") T.boolean) (T.function (T.list $ T.var "t0") (T.list $ T.var "t0"))),
  checkTest "optionals maybe" []
    (primitive _maybes_maybe)
    (tylams ["t0", "t1"] $ tyapps (primitive _maybes_maybe) [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $
      T.function (T.var "t0") (T.function (T.function (T.var "t1") (T.var "t0")) (T.function (T.optional $ T.var "t1") (T.var "t0"))))]

primitivesInComplexContextsTests :: TTerm TestGroup
primitivesInComplexContextsTests = subgroup "Primitives in complex contexts" [
  checkTest "primitive composition" []
    (lets ["double">: lambda "x" $ primitive _math_mul @@ var "x" @@ int32 2,
           "increment">: lambda "x" $ primitive _math_add @@ var "x" @@ int32 1] $
      primitive _lists_map @@ var "double" @@ (primitive _lists_map @@ var "increment" @@ list [int32 1, int32 2, int32 3]))
    (letsTyped [("double", lambdaTyped "x" T.int32 $ primitive _math_mul @@ var "x" @@ int32 2,
                 T.mono $ T.function T.int32 T.int32),
                ("increment", lambdaTyped "x" T.int32 $ primitive _math_add @@ var "x" @@ int32 1,
                 T.mono $ T.function T.int32 T.int32)] $
      tyapps (primitive _lists_map) [T.int32, T.int32] @@ var "double" @@ (tyapps (primitive _lists_map) [T.int32, T.int32] @@ var "increment" @@ list [int32 1, int32 2, int32 3]))
    (T.list T.int32),
  checkTest "nested higher-order" []
    (primitive _lists_map @@ (primitive _lists_map @@ (primitive _math_add @@ int32 1)) @@
     list [list [int32 1, int32 2], list [int32 3, int32 4]])
    (tyapps (primitive _lists_map) [T.list T.int32, T.list T.int32] @@ (tyapps (primitive _lists_map) [T.int32, T.int32] @@ (primitive _math_add @@ int32 1)) @@
     list [list [int32 1, int32 2], list [int32 3, int32 4]])
    (T.list $ T.list T.int32)]

------ Product eliminations ------

productEliminationsTests :: TTerm TestGroup
productEliminationsTests = supergroup "Product eliminations" [
  simpleTupleProjectionsTests,
  polymorphicTupleProjectionsTests,
  projectionsWithVariablesTests,
  projectionsInComplexContextsTests,
  projectionsWithMixedTypesTests,
  projectionsWithPrimitiveFunctionsTests]

simpleTupleProjectionsTests :: TTerm TestGroup
simpleTupleProjectionsTests = subgroup "Simple tuple projections" [
  noChange "projection from pair"
    (untuple 2 0 @@ tuple2 (int32 42) (string "hello"))
    T.int32,
  noChange "second projection from pair"
    (untuple 2 1 @@ tuple2 (int32 42) (string "hello"))
    T.string,
  noChange "projection from triple"
    (untuple 3 1 @@ triple (int32 1) (string "middle") (boolean True))
    T.string,
  noChange "first element of triple"
    (untuple 3 0 @@ triple (boolean False) (int32 100) (string "last"))
    T.boolean,
  noChange "last element of triple"
    (untuple 3 2 @@ triple (boolean False) (int32 100) (string "last"))
    T.string]

polymorphicTupleProjectionsTests :: TTerm TestGroup
polymorphicTupleProjectionsTests = subgroup "Polymorphic tuple projections" [
  checkTest "projection function" []
    (untuple 2 0)
    (tylams ["t0", "t1"] $ untuple 2 0)
    (T.forAlls ["t0", "t1"] $ T.function
      (T.product [T.var "t0", T.var "t1"])
      (T.var "t0")),
  checkTest "second projection function" []
    (untuple 2 1)
    (tylams ["t0", "t1"] $ untuple 2 1)
    (T.forAlls ["t0", "t1"] $ T.function
      (T.product [T.var "t0", T.var "t1"])
      (T.var "t1")),
  checkTest "triple projection function" []
    (untuple 3 1)
    (tylams ["t0", "t1", "t2"] $ untuple 3 1)
    (T.forAlls ["t0", "t1", "t2"] $ T.function
      (T.product [T.var "t0", T.var "t1", T.var "t2"])
      (T.var "t1")),
  checkTest "projection from lambda" []
    (lambda "pair" $ untuple 2 0 @@ var "pair")
    (tylams ["t0", "t1"] $ lambdaTyped "pair" (T.product [T.var "t0", T.var "t1"]) $ untuple 2 0 @@ var "pair")
    (T.forAlls ["t0", "t1"] $ T.function
      (T.product [T.var "t0", T.var "t1"])
      (T.var "t0"))]

projectionsWithVariablesTests :: TTerm TestGroup
projectionsWithVariablesTests = subgroup "Projections with variables" [
  checkTest "projection with variable tuple" []
    (lambda "x" $ lambda "y" $ untuple 2 0 @@ tuple2 (var "x") (var "y"))
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t1") $ untuple 2 0 @@ tuple2 (var "x") (var "y"))
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0")
      (T.function (T.var "t1") (T.var "t0"))),
  checkTest "projection preserves polymorphism" []
    (lambda "pair" $ tuple2 (untuple 2 0 @@ var "pair") (untuple 2 1 @@ var "pair"))
    (tylams ["t0", "t1"] $ lambdaTyped "pair" (T.product [T.var "t0", T.var "t1"]) $ tuple2 (untuple 2 0 @@ var "pair") (untuple 2 1 @@ var "pair"))
    (T.forAlls ["t0", "t1"] $ T.function
      (T.product [T.var "t0", T.var "t1"])
      (T.product [T.var "t0", T.var "t1"])),
  checkTest "nested projection" []
    (lambda "nested" $ untuple 2 0 @@ (untuple 2 1 @@ var "nested"))
    (tylams ["t0", "t1", "t2"] $ lambdaTyped "nested" (T.product [T.var "t0", T.product [T.var "t1", T.var "t2"]]) $ untuple 2 0 @@ (untuple 2 1 @@ var "nested"))
    (T.forAlls ["t0", "t1", "t2"] $ T.function
      (T.product [T.var "t0", T.product [T.var "t1", T.var "t2"]])
      (T.var "t1"))]

projectionsInComplexContextsTests :: TTerm TestGroup
projectionsInComplexContextsTests = subgroup "Projections in complex contexts" [
  checkTest "projection in let binding" []
    (lets ["pair">: tuple2 (int32 10) (string "test")] $
      untuple 2 0 @@ var "pair")
    (letsTyped [("pair", tuple2 (int32 10) (string "test"), T.mono $ T.product [T.int32, T.string])] $
      untuple 2 0 @@ var "pair")
    T.int32,
  noChange "projection in tuple"
    (tuple2 (untuple 2 0 @@ tuple2 (int32 1) (string "a"))
      (untuple 2 1 @@ tuple2 (int32 2) (string "b")))
    (T.product [T.int32, T.string]),
  noChange "projection in list"
    (list [untuple 2 0 @@ tuple2 (int32 1) (string "a"),
           untuple 2 0 @@ tuple2 (int32 2) (string "b")])
    (T.list T.int32)]

projectionsWithMixedTypesTests :: TTerm TestGroup
projectionsWithMixedTypesTests = subgroup "Projections with mixed types" [
  noChange "projection from mixed tuple"
    (untuple 4 2 @@ tuple4 (int32 1) (string "test") (boolean True) (float32 3.14))
    T.boolean,
  checkTest "projection chain" []
    (lets ["quadruple">: tuple4 (int32 1) (string "test") (boolean True) (float32 3.14)] $
      tuple4 (untuple 4 0 @@ var "quadruple")
             (untuple 4 1 @@ var "quadruple")
             (untuple 4 2 @@ var "quadruple")
             (untuple 4 3 @@ var "quadruple"))
    (letsTyped [("quadruple", tuple4 (int32 1) (string "test") (boolean True) (float32 3.14),
                 T.mono $ T.product [T.int32, T.string, T.boolean, T.float32])] $
      tuple4 (untuple 4 0 @@ var "quadruple")
             (untuple 4 1 @@ var "quadruple")
             (untuple 4 2 @@ var "quadruple")
             (untuple 4 3 @@ var "quadruple"))
    (T.product [T.int32, T.string, T.boolean, T.float32]),
  checkTest "projection with function result" []
    (untuple 2 1 @@ tuple2 (int32 42) (lambda "x" $ var "x"))
    (tylam "t0" $ untuple 2 1 @@ tuple2 (int32 42) (lambdaTyped "x" (T.var "t0") $ var "x"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.var "t0"))]

projectionsWithPrimitiveFunctionsTests :: TTerm TestGroup
projectionsWithPrimitiveFunctionsTests = subgroup "Projections with primitive functions" [
  checkTest "lists.map with untuple" []
    (primitive _lists_map @@ (untuple 2 0))
    (tylams ["t0", "t1"] $ tyapps (primitive _lists_map) [T.product [T.var "t0", T.var "t1"], T.var "t0"] @@ (untuple 2 0))
    (T.forAlls ["t0", "t1"] $
      T.function (T.list (T.product [T.var "t0", T.var "t1"])) (T.list (T.var "t0")))]

------ Records ------

recordsTests :: TTerm TestGroup
recordsTests = supergroup "Records" [
  monomorphicRecordsTests,
  polymorphicRecordsTests,
  recordsInComplexContextsTests,
  multiParameterPolymorphicRecordsTests]

monomorphicRecordsTests :: TTerm TestGroup
monomorphicRecordsTests = subgroup "Monomorphic records" [
  noChange "latlon record"
    (record (name "LatLon") [
      "lat" >: float32 19.5429,
      "lon" >: float32 (-155.6659)])
    (T.var "LatLon"),
  checkTest "latlon with variable" []
    (lambda "x" $ record (name "LatLon") [
      "lat" >: float32 19.5429,
      "lon" >: var "x"])
    (lambdaTyped "x" T.float32 $ record (name "LatLon") [
      "lat" >: float32 19.5429,
      "lon" >: var "x"])
    (T.function T.float32 (T.var "LatLon")),
  noChange "person record"
    (record (name "Person") [
      "firstName" >: string "Alice",
      "lastName" >: string "Smith",
      "age" >: int32 30])
    (T.var "Person"),
  noChange "empty record"
    (record (name "Unit") [])
    (T.var "Unit"),
  checkTest "person with variables" []
    (lambda "name" $ lambda "age" $ record (name "Person") [
      "firstName" >: var "name",
      "lastName" >: string "Doe",
      "age" >: var "age"])
    (lambdaTyped "name" T.string $ lambdaTyped "age" T.int32 $ record (name "Person") [
      "firstName" >: var "name",
      "lastName" >: string "Doe",
      "age" >: var "age"])
    (T.function T.string (T.function T.int32 (T.var "Person")))]

polymorphicRecordsTests :: TTerm TestGroup
polymorphicRecordsTests = subgroup "Polymorphic records" [
  checkTest "latlon poly float" []
    (record (name "LatLonPoly") [
      "lat" >: float32 19.5429,
      "lon" >: float32 (-155.6659)])
    (tyapp (record (name "LatLonPoly") [
      "lat" >: float32 19.5429,
      "lon" >: float32 (-155.6659)]) T.float32)
    (T.apply (T.var "LatLonPoly") T.float32),
  checkTest "latlon poly int64" []
    (record (name "LatLonPoly") [
      "lat" >: int64 195429,
      "lon" >: int64 (-1556659)])
    (tyapp (record (name "LatLonPoly") [
      "lat" >: int64 195429,
      "lon" >: int64 (-1556659)]) T.int64)
    (T.apply (T.var "LatLonPoly") T.int64),
  checkTest "latlon poly variable" []
    (lambda "x" $ record (name "LatLonPoly") [
      "lat" >: var "x",
      "lon" >: var "x"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapp (record (name "LatLonPoly") [
      "lat" >: var "x",
      "lon" >: var "x"]) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.apply (T.var "LatLonPoly") (T.var "t0"))),
  checkTest "buddylist string" []
    (record (name "BuddyListA") [
      "head" >: string "first",
      "tail" >: optional nothing])
    (tyapp (record (name "BuddyListA") [
      "head" >: string "first",
      "tail" >: tyapp (optional nothing) (T.apply (T.var "BuddyListB") T.string)]) T.string)
    (T.apply (T.var "BuddyListA") T.string),
  checkTest "buddylist variable" []
    (lambda "x" $ record (name "BuddyListA") [
      "head" >: var "x",
      "tail" >: optional nothing])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapp (record (name "BuddyListA") [
      "head" >: var "x",
      "tail" >: tyapp (optional nothing) (T.apply (T.var "BuddyListB") (T.var "t0"))]) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.apply (T.var "BuddyListA") (T.var "t0")))]

recordsInComplexContextsTests :: TTerm TestGroup
recordsInComplexContextsTests = subgroup "Records in complex contexts" [
  noChange "records in tuple"
    (tuple [
      record (name "Person") [
        "firstName" >: string "Bob",
        "lastName" >: string "Jones",
        "age" >: int32 25],
      record (name "LatLon") [
        "lat" >: float32 1.0,
        "lon" >: float32 2.0]])
    (T.product [T.var "Person", T.var "LatLon"]),
  checkTest "poly records in tuple" []
    (tuple [
      record (name "LatLonPoly") [
        "lat" >: int32 1,
        "lon" >: int32 2],
      record (name "BuddyListA") [
        "head" >: string "test",
        "tail" >: optional nothing]])
    (tuple [
      tyapp (record (name "LatLonPoly") [
        "lat" >: int32 1,
        "lon" >: int32 2]) T.int32,
      tyapp (record (name "BuddyListA") [
        "head" >: string "test",
        "tail" >: tyapp (optional nothing) (T.apply (T.var "BuddyListB") T.string)]) T.string])
    (T.product [
      T.apply (T.var "LatLonPoly") T.int32,
      T.apply (T.var "BuddyListA") T.string]),
  checkTest "recursive record" []
    (record (name "IntList") [
      "head" >: int32 42,
      "tail" >: optional (Phantoms.just (
        record (name "IntList") [
          "head" >: int32 43,
          "tail" >: optional nothing]))])
    (record (name "IntList") [
      "head" >: int32 42,
      "tail" >: optional (Phantoms.just (
        record (name "IntList") [
          "head" >: int32 43,
          "tail" >: tyapp (optional nothing) (T.var "IntList")]))])
    (T.var "IntList")]

multiParameterPolymorphicRecordsTests :: TTerm TestGroup
multiParameterPolymorphicRecordsTests = subgroup "Multi-parameter polymorphic records" [
  checkTest "triple with three monomorphic types" []
    (record (name "Triple") [
      "first" >: int32 1,
      "second" >: string "middle",
      "third" >: boolean True])
    (tyapps (record (name "Triple") [
      "first" >: int32 1,
      "second" >: string "middle",
      "third" >: boolean True]) [T.int32, T.string, T.boolean])
    (T.applys (T.var "Triple") [T.int32, T.string, T.boolean]),
  checkTest "triple with PersonOrSomething containing map" []
    (lambda "k" $ lambda "v" $
      record (name "Triple") [
        "first" >: string "prefix",
        "second" >: inject (ref testTypePersonOrSomethingNameDef) "other"
          (mapTerm [(var "k", var "v")]),
        "third" >: int32 999])
    (tylams ["t0", "t1"] $
      lambdaTyped "k" (T.var "t0") $
      lambdaTyped "v" (T.var "t1") $
      tyapps (record (name "Triple") [
        "first" >: string "prefix",
        "second" >: tyapp (inject (ref testTypePersonOrSomethingNameDef) "other"
          (mapTerm [(var "k", var "v")])) (T.map (T.var "t0") (T.var "t1")),
        "third" >: int32 999])
        [T.string,
         T.apply (T.var "PersonOrSomething") (T.map (T.var "t0") (T.var "t1")),
         T.int32])
    (T.forAlls ["t0", "t1"] $
      T.function (T.var "t0") $
      T.function (T.var "t1") $
      T.applys (T.var "Triple")
        [T.string,
         T.apply (T.var "PersonOrSomething") (T.map (T.var "t0") (T.var "t1")),
         T.int32])]

------ Record eliminations ------

recordEliminationsTests :: TTerm TestGroup
recordEliminationsTests = supergroup "Record eliminations" [
  simpleRecordProjectionsTests,
  recordProjectionsAppliedToRecordsTests,
  polymorphicRecordProjectionsTests,
  polymorphicRecordProjectionsAppliedTests,
  recordProjectionsWithVariablesTests,
  recordProjectionsInComplexContextsTests,
  multiParameterPolymorphicProjectionsTests,
  higherOrderRecordProjectionsTests,
  recursiveRecordProjectionsTests,
  recordProjectionsWithMutualRecursionTests]

simpleRecordProjectionsTests :: TTerm TestGroup
simpleRecordProjectionsTests = subgroup "Simple record projections" [
  noChange "project firstName from Person"
    (project (ref testTypePersonNameDef) (name "firstName"))
    (T.function (T.var "Person") T.string),
  noChange "project lastName from Person"
    (project (ref testTypePersonNameDef) (name "lastName"))
    (T.function (T.var "Person") T.string),
  noChange "project age from Person"
    (project (ref testTypePersonNameDef) (name "age"))
    (T.function (T.var "Person") T.int32),
  noChange "project lat from LatLon"
    (project (ref testTypeLatLonNameDef) (name "lat"))
    (T.function (T.var "LatLon") T.float32),
  noChange "project lon from LatLon"
    (project (ref testTypeLatLonNameDef) (name "lon"))
    (T.function (T.var "LatLon") T.float32)]

recordProjectionsAppliedToRecordsTests :: TTerm TestGroup
recordProjectionsAppliedToRecordsTests = subgroup "Record projections applied to records" [
  noChange "project firstName applied to person record"
    (project (ref testTypePersonNameDef) (name "firstName") @@
     record (name "Person") [
       "firstName" >: string "Alice",
       "lastName" >: string "Smith",
       "age" >: int32 30])
    T.string,
  noChange "project age applied to person record"
    (project (ref testTypePersonNameDef) (name "age") @@
     record (name "Person") [
       "firstName" >: string "Bob",
       "lastName" >: string "Jones",
       "age" >: int32 25])
    T.int32,
  noChange "project lat applied to LatLon record"
    (project (ref testTypeLatLonNameDef) (name "lat") @@
     record (name "LatLon") [
       "lat" >: float32 40.7128,
       "lon" >: float32 (-74.0060)])
    T.float32]

polymorphicRecordProjectionsTests :: TTerm TestGroup
polymorphicRecordProjectionsTests = subgroup "Polymorphic record projections" [
  checkTest "project lat from polymorphic LatLonPoly" []
    (project (ref testTypeLatLonPolyNameDef) (name "lat"))
    (tylam "t0" $ tyapp (project (ref testTypeLatLonPolyNameDef) (name "lat")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.apply (T.var "LatLonPoly") (T.var "t0")) (T.var "t0")),
  checkTest "project lon from polymorphic LatLonPoly" []
    (project (ref testTypeLatLonPolyNameDef) (name "lon"))
    (tylam "t0" $ tyapp (project (ref testTypeLatLonPolyNameDef) (name "lon")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.apply (T.var "LatLonPoly") (T.var "t0")) (T.var "t0")),
  checkTest "project head from BuddyListA" []
    (project (ref testTypeBuddyListANameDef) (name "head"))
    (tylam "t0" $ tyapp (project (ref testTypeBuddyListANameDef) (name "head")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.apply (T.var "BuddyListA") (T.var "t0")) (T.var "t0")),
  checkTest "project tail from BuddyListA" []
    (project (ref testTypeBuddyListANameDef) (name "tail"))
    (tylam "t0" $ tyapp (project (ref testTypeBuddyListANameDef) (name "tail")) (T.var "t0"))
    (T.forAll "t0" $ T.function
      (T.apply (T.var "BuddyListA") (T.var "t0"))
      (T.optional (T.apply (T.var "BuddyListB") (T.var "t0"))))]

polymorphicRecordProjectionsAppliedTests :: TTerm TestGroup
polymorphicRecordProjectionsAppliedTests = subgroup "Polymorphic record projections applied" [
  checkTest "project lat from LatLonPoly with int32" []
    (project (ref testTypeLatLonPolyNameDef) (name "lat") @@
     record (name "LatLonPoly") [
       "lat" >: int32 40,
       "lon" >: int32 (-74)])
    (tyapp (project (ref testTypeLatLonPolyNameDef) (name "lat")) T.int32 @@
     tyapp (record (name "LatLonPoly") [
       "lat" >: int32 40,
       "lon" >: int32 (-74)]) T.int32)
    T.int32,
  checkTest "project lon from LatLonPoly with float64" []
    (project (ref testTypeLatLonPolyNameDef) (name "lon") @@
     record (name "LatLonPoly") [
       "lat" >: float64 40.7128,
       "lon" >: float64 (-74.0060)])
    (tyapp (project (ref testTypeLatLonPolyNameDef) (name "lon")) T.float64 @@
     tyapp (record (name "LatLonPoly") [
       "lat" >: float64 40.7128,
       "lon" >: float64 (-74.0060)]) T.float64)
    T.float64,
  checkTest "project head from BuddyListA with string" []
    (project (ref testTypeBuddyListANameDef) (name "head") @@
     record (name "BuddyListA") [
       "head" >: string "Alice",
       "tail" >: optional nothing])
    (tyapp (project (ref testTypeBuddyListANameDef) (name "head")) T.string @@
     tyapp (record (name "BuddyListA") [
       "head" >: string "Alice",
       "tail" >: tyapp (optional nothing) (T.apply (T.var "BuddyListB") T.string)]) T.string)
    T.string]

recordProjectionsWithVariablesTests :: TTerm TestGroup
recordProjectionsWithVariablesTests = subgroup "Record projections with variables" [
  checkTest "project from lambda parameter" []
    (lambda "person" $ project (ref testTypePersonNameDef) (name "firstName") @@ var "person")
    (lambdaTyped "person" (T.var "Person") $ project (ref testTypePersonNameDef) (name "firstName") @@ var "person")
    (T.function (T.var "Person") T.string),
  checkTest "project from polymorphic lambda parameter" []
    (lambda "coords" $ project (ref testTypeLatLonPolyNameDef) (name "lat") @@ var "coords")
    (tylam "t0" $ lambdaTyped "coords" (T.apply (T.var "LatLonPoly") (T.var "t0")) $ tyapp (project (ref testTypeLatLonPolyNameDef) (name "lat")) (T.var "t0") @@ var "coords")
    (T.forAll "t0" $ T.function (T.apply (T.var "LatLonPoly") (T.var "t0")) (T.var "t0")),
  checkTest "multiple projections from same record" []
    (lambda "person" $
     tuple [project (ref testTypePersonNameDef) (name "firstName") @@ var "person",
            project (ref testTypePersonNameDef) (name "lastName") @@ var "person"])
    (lambdaTyped "person" (T.var "Person") $
     tuple [project (ref testTypePersonNameDef) (name "firstName") @@ var "person",
            project (ref testTypePersonNameDef) (name "lastName") @@ var "person"])
    (T.function (T.var "Person") (T.product [T.string, T.string]))]

recordProjectionsInComplexContextsTests :: TTerm TestGroup
recordProjectionsInComplexContextsTests = subgroup "Record projections in complex contexts" [
  checkTest "projection in let binding" []
    (lets ["person">: record (name "Person") [
             "firstName" >: string "Charlie",
             "lastName" >: string "Brown",
             "age" >: int32 35],
           "getName">: project (ref testTypePersonNameDef) (name "firstName")] $
          var "getName" @@ var "person")
    (letsTyped [("person", record (name "Person") [
                   "firstName" >: string "Charlie",
                   "lastName" >: string "Brown",
                   "age" >: int32 35],
                 T.mono $ T.var "Person"),
                ("getName", project (ref testTypePersonNameDef) (name "firstName"),
                 T.mono $ T.function (T.var "Person") T.string)] $
      var "getName" @@ var "person")
    T.string,
  noChange "projection in tuple"
    (tuple [project (ref testTypePersonNameDef) (name "firstName"),
            project (ref testTypePersonNameDef) (name "age")])
    (T.product [T.function (T.var "Person") T.string,
                T.function (T.var "Person") T.int32]),
  noChange "projection in list"
    (list [project (ref testTypePersonNameDef) (name "firstName"),
           project (ref testTypePersonNameDef) (name "lastName")])
    (T.list (T.function (T.var "Person") T.string))]

multiParameterPolymorphicProjectionsTests :: TTerm TestGroup
multiParameterPolymorphicProjectionsTests = subgroup "Multi-parameter polymorphic projections" [
  checkTest "project first from Triple" []
    (project (ref testTypeTripleNameDef) (name "first"))
    (tylams ["t0", "t1", "t2"] $ tyapps (project (ref testTypeTripleNameDef) (name "first")) [T.var "t0", T.var "t1", T.var "t2"])
    (T.forAlls ["t0", "t1", "t2"] $
      T.function
        (T.applys (Core.typeVariable $ ref testTypeTripleNameDef) [T.var "t0", T.var "t1", T.var "t2"])
        (T.var "t0")),
  checkTest "project second from Triple applied" []
    (project (ref testTypeTripleNameDef) (name "second") @@
      record (ref testTypeTripleNameDef) [
        "first">: int32 1,
        "second">: string "middle",
        "third">: boolean True])
    (tyapps (project (ref testTypeTripleNameDef) (name "second")) [T.int32, T.string, T.boolean] @@
      tyapps (record (ref testTypeTripleNameDef) [
        "first">: int32 1,
        "second">: string "middle",
        "third">: boolean True]) [T.int32, T.string, T.boolean])
    T.string,
  checkTest "project from Triple and use second field, which is another polymorphic record" []
    (lambda "triple" $ lambda "key" $
      match (ref testTypePersonOrSomethingNameDef) nothing [
        "person">: lambda "p" $ Core.termMaybe nothing,
        "other">: lambda "m" $ primitive _maps_lookup @@ var "key" @@ var "m"] @@
      (project (ref testTypeTripleNameDef) (name "second") @@ var "triple"))
    (tylams ["t0", "t1", "t2", "t3"] $
      lambdaTyped "triple"
        (T.applys (Core.typeVariable $ ref testTypeTripleNameDef)
          [T.var "t0",
           T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) (T.map (T.var "t1") (T.var "t2")),
           T.var "t3"]) $
      lambdaTyped "key" (T.var "t1") $
      tyapp (match (ref testTypePersonOrSomethingNameDef) nothing [
        "person">: lambdaTyped "p" (Core.typeVariable $ ref testTypePersonNameDef) (tyapp (Core.termMaybe nothing) (T.var "t2")),
        "other">: lambdaTyped "m" (T.map (T.var "t1") (T.var "t2")) $
          tyapps (primitive _maps_lookup) [T.var "t1", T.var "t2"] @@ var "key" @@ var "m"]) (T.map (T.var "t1") (T.var "t2")) @@
      (tyapps (project (ref testTypeTripleNameDef) (name "second"))
        [T.var "t0",
         T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) (T.map (T.var "t1") (T.var "t2")),
         T.var "t3"] @@ var "triple"))
    (T.forAlls ["t0", "t1", "t2", "t3"] $
      T.function
        (T.applys (Core.typeVariable $ ref testTypeTripleNameDef)
          [T.var "t0",
           T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) (T.map (T.var "t1") (T.var "t2")),
           T.var "t3"])
        (T.function (T.var "t1") (T.optional (T.var "t2"))))]

higherOrderRecordProjectionsTests :: TTerm TestGroup
higherOrderRecordProjectionsTests = subgroup "Higher-order record projections" [
  checkTest "map projection over list of records" []
    (primitive _lists_map @@ (project (ref testTypePersonNameDef) (name "firstName")) @@
     list [record (ref testTypePersonNameDef) [
             "firstName">: (string "Alice"),
             "lastName">: (string "Smith"),
             "age">: (int32 30)],
           record (ref testTypePersonNameDef) [
             "firstName">: (string "Bob"),
             "lastName">: (string "Jones"),
             "age">: (int32 25)]])
    (tyapps (primitive _lists_map) [Core.typeVariable $ ref testTypePersonNameDef, T.string] @@ (project (ref testTypePersonNameDef) (name "firstName")) @@
     list [record (ref testTypePersonNameDef) [
             "firstName">: (string "Alice"),
             "lastName">: (string "Smith"),
             "age">: (int32 30)],
           record (ref testTypePersonNameDef) [
             "firstName">: (string "Bob"),
             "lastName">: (string "Jones"),
             "age">: (int32 25)]])
    (T.list T.string),
  checkTest "map polymorphic projection" []
    (primitive _lists_map @@ (project (ref testTypeLatLonPolyNameDef) (name "lat")) @@
     list [record (ref testTypeLatLonPolyNameDef) [
             "lat">: (int32 40),
             "lon">: (int32 (-74))],
           record (ref testTypeLatLonPolyNameDef) [
             "lat">: (int32 34),
             "lon">: (int32 (-118))]])
    (tyapps (primitive _lists_map) [T.apply (Core.typeVariable $ ref testTypeLatLonPolyNameDef) T.int32, T.int32]
      @@ (tyapp (project (ref testTypeLatLonPolyNameDef) (name "lat")) T.int32) @@
     list [tyapp (record (ref testTypeLatLonPolyNameDef) [
             "lat">: (int32 40),
             "lon">: (int32 (-74))]) T.int32,
           tyapp (record (ref testTypeLatLonPolyNameDef) [
             "lat">: (int32 34),
             "lon">: (int32 (-118))]) T.int32])
    (T.list T.int32),
  checkTest "filter using projection" []
    (primitive _lists_filter @@
     (lambda "person" $
      primitive _equality_gt @@
      (project (ref testTypePersonNameDef) (name "age") @@ var "person") @@
      int32 30) @@
     list [record (ref testTypePersonNameDef) [
             "firstName">: (string "Alice"),
             "lastName">: (string "Smith"),
             "age">: (int32 35)],
           record (ref testTypePersonNameDef) [
             "firstName">: (string "Bob"),
             "lastName">: (string "Jones"),
             "age">: (int32 25)]])
    (tyapp (primitive _lists_filter) (Core.typeVariable $ ref testTypePersonNameDef) @@
     (lambdaTyped "person" (Core.typeVariable $ ref testTypePersonNameDef) $
      tyapp (primitive _equality_gt) T.int32 @@
      (project (ref testTypePersonNameDef) (name "age") @@ var "person") @@
      int32 30) @@
     list [record (ref testTypePersonNameDef) [
             "firstName">: (string "Alice"),
             "lastName">: (string "Smith"),
             "age">: (int32 35)],
           record (ref testTypePersonNameDef) [
             "firstName">: (string "Bob"),
             "lastName">: (string "Jones"),
             "age">: (int32 25)]])
    (T.list (Core.typeVariable $ ref testTypePersonNameDef))]

recursiveRecordProjectionsTests :: TTerm TestGroup
recursiveRecordProjectionsTests = subgroup "Recursive record projections" [
  checkTest "nested projection from recursive record" []
    (lambda "intList" $
     primitive _maybes_maybe @@
     int32 0 @@
     (project (ref testTypeIntListNameDef) (name "head")) @@
     (project (ref testTypeIntListNameDef) (name "tail") @@ var "intList"))
    (lambdaTyped "intList" (Core.typeVariable $ ref testTypeIntListNameDef) $
     tyapps (primitive _maybes_maybe) [T.int32, Core.typeVariable $ ref testTypeIntListNameDef] @@
     int32 0 @@
     (project (ref testTypeIntListNameDef) (name "head")) @@
     (project (ref testTypeIntListNameDef) (name "tail") @@ var "intList"))
    (T.function (Core.typeVariable $ ref testTypeIntListNameDef) T.int32)]

recordProjectionsWithMutualRecursionTests :: TTerm TestGroup
recordProjectionsWithMutualRecursionTests = subgroup "Record projections with mutual recursion" [
  checkTest "project head from BuddyListA" []
    (project (ref testTypeBuddyListANameDef) (name "head"))
    (tylam "t0" $ tyapp (project (ref testTypeBuddyListANameDef) (name "head")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) (T.var "t0")) (T.var "t0")),
  checkTest "project tail from BuddyListB" []
    (project (ref testTypeBuddyListBNameDef) (name "tail"))
    (tylam "t0" $ tyapp (project (ref testTypeBuddyListBNameDef) (name "tail")) (T.var "t0"))
    (T.forAll "t0" $ T.function
      (T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "t0"))
      (T.optional (T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) (T.var "t0")))),
  checkTest "chained projections across mutual recursion" []
    (lambda "listA" $
      primitive _maybes_maybe @@
      Core.termMaybe nothing @@
      (lambda "listB" $
        primitive _maybes_maybe @@
        Core.termMaybe nothing @@
        (project (ref testTypeBuddyListANameDef) (name "tail")) @@
        (project (ref testTypeBuddyListBNameDef) (name "tail") @@ var "listB")) @@
      (project (ref testTypeBuddyListANameDef) (name "tail") @@ var "listA"))
    (tylam "t0" $ lambdaTyped "listA" (T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) (T.var "t0")) $
      tyapps (primitive _maybes_maybe) [T.optional (T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "t0")), T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "t0")] @@
      tyapp (Core.termMaybe nothing) (T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "t0")) @@
      (lambdaTyped "listB" (T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "t0")) $
        tyapps (primitive _maybes_maybe) [T.optional (T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "t0")), T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) (T.var "t0")] @@
        tyapp (Core.termMaybe nothing) (T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "t0")) @@
        (tyapp (project (ref testTypeBuddyListANameDef) (name "tail")) (T.var "t0")) @@
        (tyapp (project (ref testTypeBuddyListBNameDef) (name "tail")) (T.var "t0") @@ var "listB")) @@
      (tyapp (project (ref testTypeBuddyListANameDef) (name "tail")) (T.var "t0") @@ var "listA"))
    (T.forAll "t0" $ T.function
      (T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) (T.var "t0"))
      (T.optional (T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "t0"))))]

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
    (TTerms.set [])
    (tylam "t0" $ tyapp (TTerms.set []) (T.var "t0"))
    (T.forAll "t0" $ T.set $ T.var "t0"),
  noChange "int set"
    (TTerms.set [int32 1, int32 2, int32 3])
    (T.set T.int32),
  noChange "string set"
    (TTerms.set [string "apple", string "banana", string "cherry"])
    (T.set T.string),
  noChange "single element set"
    (TTerms.set [boolean True])
    (T.set T.boolean)]

polymorphicSetsTests :: TTerm TestGroup
polymorphicSetsTests = subgroup "Polymorphic sets" [
  checkTest "set from lambda" []
    (lambda "x" $ TTerms.set [var "x"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ TTerms.set [var "x"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.set $ T.var "t0")),
  checkTest "set with repeated variable" []
    (lambda "x" $ TTerms.set [var "x", var "x"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ TTerms.set [var "x", var "x"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.set $ T.var "t0")),
  checkTest "set from two variables" []
    (lambda "x" $ lambda "y" $ TTerms.set [var "x", var "y"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t0") $ TTerms.set [var "x", var "y"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.function (T.var "t0") (T.set $ T.var "t0")))]

setsInComplexContextsTests :: TTerm TestGroup
setsInComplexContextsTests = subgroup "Sets in complex contexts" [
  noChange "set in tuple"
    (tuple [TTerms.set [int32 1, int32 2], string "context"])
    (T.product [T.set T.int32, T.string]),
  checkTest "set in let binding" []
    (lets ["numbers">: TTerms.set [int32 10, int32 20, int32 30]] $
      var "numbers")
    (letsTyped [("numbers", TTerms.set [int32 10, int32 20, int32 30],
      T.mono $ T.set T.int32)] $
      var "numbers")
    (T.set T.int32)]

nestedSetsTests :: TTerm TestGroup
nestedSetsTests = subgroup "Nested sets" [
  noChange "set of lists"
    (TTerms.set [
      list [string "a", string "b"],
      list [string "c", string "d"]])
    (T.set $ T.list T.string),
  noChange "set of tuples"
    (TTerms.set [
      tuple [int32 1, int32 2],
      tuple [int32 3, int32 4]])
    (T.set $ T.product [T.int32, T.int32]),
  noChange "set of sets"
    (TTerms.set [TTerms.set [string "nested"]])
    (T.set $ T.set T.string)]

setsWithComplexTypesTests :: TTerm TestGroup
setsWithComplexTypesTests = subgroup "Sets with complex types" [
  noChange "set of records"
    (TTerms.set [record (ref testTypePersonNameDef) [
      "firstName">: string "Alice",
      "lastName">: string "Smith",
      "age">: int32 30]])
    (T.set $ Core.typeVariable $ ref testTypePersonNameDef),
  checkTest "set of optionals" []
    (TTerms.set [
      optional $ just $ int32 42,
      optional nothing])
    (TTerms.set [
      optional $ just $ int32 42,
      tyapp (optional nothing) T.int32])
    (T.set $ T.optional T.int32),
  noChange "set of maps"
    (TTerms.set [TTerms.map $ Phantoms.map $ M.singleton (string "key") (int32 42)])
    (T.set $ T.map T.string T.int32)]

------ Sums ------

sumsTests :: TTerm TestGroup
sumsTests = subgroup "Sums" []

------ Unions ------

unionsTests :: TTerm TestGroup
unionsTests = supergroup "Unions" [
  simpleUnionInjectionsTests,
  unionInjectionsWithDataTests,
  polymorphicUnionInjectionsTests,
  polymorphicRecursiveUnionInjectionsTests,
  polymorphicUnionsFromLambdaTests,
  unionsInComplexContextsTests,
  multiParameterPolymorphicInjectionsTests]

simpleUnionInjectionsTests :: TTerm TestGroup
simpleUnionInjectionsTests = subgroup "Simple union injections" [
  noChange "inject into Comparison lessThan variant"
    (injectUnit (ref testTypeComparisonNameDef) "lessThan")
    (Core.typeVariable $ ref testTypeComparisonNameDef),
  noChange "inject into Comparison equalTo variant"
    (injectUnit (ref testTypeComparisonNameDef) "equalTo")
    (Core.typeVariable $ ref testTypeComparisonNameDef),
  noChange "inject into Comparison greaterThan variant"
    (injectUnit (ref testTypeComparisonNameDef) "greaterThan")
    (Core.typeVariable $ ref testTypeComparisonNameDef)]

unionInjectionsWithDataTests :: TTerm TestGroup
unionInjectionsWithDataTests = subgroup "Union injections with data" [
  noChange "inject into Number int variant"
    (inject (ref testTypeNumberNameDef) "int" (int32 42))
    (Core.typeVariable $ ref testTypeNumberNameDef),
  noChange "inject into Number float variant"
    (inject (ref testTypeNumberNameDef) "float" (float32 3.14))
    (Core.typeVariable $ ref testTypeNumberNameDef),
  noChange "inject into Timestamp unixTimeMillis variant"
    (inject (ref testTypeTimestampNameDef) "unixTimeMillis" (uint64 1609459200000))
    (Core.typeVariable $ ref testTypeTimestampNameDef),
  noChange "inject into Timestamp date variant"
    (inject (ref testTypeTimestampNameDef) "date" (string "2021-01-01"))
    (Core.typeVariable $ ref testTypeTimestampNameDef)]

polymorphicUnionInjectionsTests :: TTerm TestGroup
polymorphicUnionInjectionsTests = subgroup "Polymorphic union injections" [
  checkTest "inject person into PersonOrSomething" []
    (inject (ref testTypePersonOrSomethingNameDef) "person"
      (record (ref testTypePersonNameDef) [
        "firstName">: string "Alice",
        "lastName">: string "Smith",
        "age">: int32 30]))
    (tylam "t0" $ tyapp (inject (ref testTypePersonOrSomethingNameDef) "person"
      (record (ref testTypePersonNameDef) [
        "firstName">: string "Alice",
        "lastName">: string "Smith",
        "age">: int32 30])) (T.var "t0"))
    (T.forAll "t0" $ T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) (T.var "t0")),
  checkTest "inject string into PersonOrSomething other variant" []
    (inject (ref testTypePersonOrSomethingNameDef) "other" (string "something else"))
    (tyapp (inject (ref testTypePersonOrSomethingNameDef) "other" (string "something else")) T.string)
    (T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) T.string),
  checkTest "inject int into PersonOrSomething other variant" []
    (inject (ref testTypePersonOrSomethingNameDef) "other" (int32 42))
    (tyapp (inject (ref testTypePersonOrSomethingNameDef) "other" (int32 42)) T.int32)
    (T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) T.int32)]

polymorphicRecursiveUnionInjectionsTests :: TTerm TestGroup
polymorphicRecursiveUnionInjectionsTests = subgroup "Polymorphic recursive union injections" [
  checkTest "inject boolean into UnionPolymorphicRecursive" []
    (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "bool" (boolean True))
    (tylam "t0" $ tyapp (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "bool" (boolean True)) (T.var "t0"))
    (T.forAll "t0" $ T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) (T.var "t0")),
  checkTest "inject string value into UnionPolymorphicRecursive" []
    (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" (string "test"))
    (tyapp (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" (string "test")) T.string)
    (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) T.string),
  checkTest "inject int value into UnionPolymorphicRecursive" []
    (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" (int32 123))
    (tyapp (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" (int32 123)) T.int32)
    (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) T.int32)]

polymorphicUnionsFromLambdaTests :: TTerm TestGroup
polymorphicUnionsFromLambdaTests = subgroup "Polymorphic unions from lambda" [
  checkTest "lambda creating PersonOrSomething other variant" []
    (lambda "x" $ inject (ref testTypePersonOrSomethingNameDef) "other" (var "x"))
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapp (inject (ref testTypePersonOrSomethingNameDef) "other" (var "x")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) (T.var "t0"))),
  checkTest "lambda creating UnionPolymorphicRecursive value variant" []
    (lambda "x" $ inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" (var "x"))
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapp (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" (var "x")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) (T.var "t0")))]

unionsInComplexContextsTests :: TTerm TestGroup
unionsInComplexContextsTests = subgroup "Unions in complex contexts" [
  noChange "union in tuple"
    (tuple [inject (ref testTypeNumberNameDef) "int" (int32 42),
            string "context"])
    (T.product [Core.typeVariable $ ref testTypeNumberNameDef, T.string]),
  noChange "union in list"
    (list [inject (ref testTypeNumberNameDef) "int" (int32 1),
           inject (ref testTypeNumberNameDef) "float" (float32 2.5)])
    (T.list $ Core.typeVariable $ ref testTypeNumberNameDef),
  checkTest "polymorphic union in let binding" []
    (lets ["value">: inject (ref testTypePersonOrSomethingNameDef) "other" (string "test")] $
          var "value")
    (letsTyped [("value", tyapp (inject (ref testTypePersonOrSomethingNameDef) "other" (string "test")) T.string,
                 T.mono $ T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) T.string)] $
      var "value")
    (T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) T.string)]

multiParameterPolymorphicInjectionsTests :: TTerm TestGroup
multiParameterPolymorphicInjectionsTests = subgroup "Multi-parameter polymorphic injections" [
  checkTest "either left with int" []
    (inject (ref testTypeEitherNameDef) "left" (int32 42))
    (tylam "t0" $ tyapps (inject (ref testTypeEitherNameDef) "left" (int32 42)) [T.int32, T.var "t0"])
    (T.forAll "t0" $ T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.int32, T.var "t0"]),
  checkTest "either right with string" []
    (inject (ref testTypeEitherNameDef) "right" (string "hello"))
    (tylam "t0" $ tyapps (inject (ref testTypeEitherNameDef) "right" (string "hello")) [T.var "t0", T.string])
    (T.forAll "t0" $ T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t0", T.string]),
  checkTest "either containing LatLonPoly in list" []
    (inject (ref testTypeEitherNameDef) "right"
      (list [record (ref testTypeLatLonPolyNameDef) [
        "lat">: int32 40,
        "lon">: int32 (-74)]]))
    (tylam "t0" $ tyapps (inject (ref testTypeEitherNameDef) "right"
      (list [tyapp (record (ref testTypeLatLonPolyNameDef) [
        "lat">: int32 40,
        "lon">: int32 (-74)]) T.int32]))
      [T.var "t0", T.list (T.apply (Core.typeVariable $ ref testTypeLatLonPolyNameDef) T.int32)])
    (T.forAll "t0" $ T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t0", T.list (T.apply (Core.typeVariable $ ref testTypeLatLonPolyNameDef) T.int32)]),
  checkTest "either in triple in map with shared type variables" []
    (lambda "x0" $ lambda "x1" $ lambda "x2" $
      TTerms.map $ Phantoms.map $ M.singleton (string "key") $
        record (ref testTypeTripleNameDef) [
          "first">: inject (ref testTypeEitherNameDef) "left" (var "x0"),
          "second">: inject (ref testTypeEitherNameDef) "left" (var "x0"),
          "third">: inject (ref testTypeEitherNameDef) "right" (var "x1")])
    (tylams ["t0", "t1", "t2", "t3", "t4", "t5"] $
      lambdaTyped "x0" (T.var "t0") $
      lambdaTyped "x1" (T.var "t1") $
      lambdaTyped "x2" (T.var "t2") $
      TTerms.map $ Phantoms.map $ M.singleton (string "key") $
        tyapps (record (ref testTypeTripleNameDef) [
          "first">: tyapps (inject (ref testTypeEitherNameDef) "left" (var "x0")) [T.var "t0", T.var "t3"],
          "second">: tyapps (inject (ref testTypeEitherNameDef) "left" (var "x0")) [T.var "t0", T.var "t4"],
          "third">: tyapps (inject (ref testTypeEitherNameDef) "right" (var "x1")) [T.var "t5", T.var "t1"]])
        [T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t0", T.var "t3"],
         T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t0", T.var "t4"],
         T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t5", T.var "t1"]])
    (T.forAlls ["t0", "t1", "t2", "t3", "t4", "t5"] $
      T.function (T.var "t0") $
      T.function (T.var "t1") $
      T.function (T.var "t2") $
      T.map T.string $
        T.applys (Core.typeVariable $ ref testTypeTripleNameDef)
          [T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t0", T.var "t3"],
           T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t0", T.var "t4"],
           T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t5", T.var "t1"]])]

------ Union eliminations ------

unionEliminationsTests :: TTerm TestGroup
unionEliminationsTests = supergroup "Union eliminations" [
  simpleUnitVariantEliminationsTests,
  unionEliminationsWithDataTests,
  polymorphicUnionEliminationsTests,
  unionEliminationsWithDefaultsTests,
  nestedUnionEliminationsTests,
  unionEliminationsInComplexContextsTests,
  multiParameterPolymorphicCaseStatementsTests,
  higherOrderUnionEliminationsTests,
  recursiveUnionEliminationsTests]

simpleUnitVariantEliminationsTests :: TTerm TestGroup
simpleUnitVariantEliminationsTests = subgroup "Simple unit variant eliminations" [
  checkTest "match Comparison with all cases" []
    (match (ref testTypeComparisonNameDef) nothing [
      "lessThan">: lambda "x" (string "less"),
      "equalTo">: lambda "x" (string "equal"),
      "greaterThan">: lambda "x" (string "greater")])
    (match (ref testTypeComparisonNameDef) nothing [
      "lessThan">: lambdaTyped "x" T.unit (string "less"),
      "equalTo">: lambdaTyped "x" T.unit (string "equal"),
      "greaterThan">: lambdaTyped "x" T.unit (string "greater")])
    (T.function (Core.typeVariable $ ref testTypeComparisonNameDef) T.string),
  checkTest "match Comparison returning int32" []
    (match (ref testTypeComparisonNameDef) nothing [
      "lessThan">: lambda "x" (int32 (-1)),
      "equalTo">: lambda "x" (int32 0),
      "greaterThan">: lambda "x" (int32 1)])
    (match (ref testTypeComparisonNameDef) nothing [
      "lessThan">: lambdaTyped "x" T.unit (int32 (-1)),
      "equalTo">: lambdaTyped "x" T.unit (int32 0),
      "greaterThan">: lambdaTyped "x" T.unit (int32 1)])
    (T.function (Core.typeVariable $ ref testTypeComparisonNameDef) T.int32),
  checkTest "match applied to Comparison variant" []
    (match (ref testTypeComparisonNameDef) nothing [
      "lessThan">: lambda "x" (string "less"),
      "equalTo">: lambda "x" (string "equal"),
      "greaterThan">: lambda "x" (string "greater")] @@
     injectUnit (ref testTypeComparisonNameDef) "equalTo")
    (match (ref testTypeComparisonNameDef) nothing [
      "lessThan">: lambdaTyped "x" T.unit (string "less"),
      "equalTo">: lambdaTyped "x" T.unit (string "equal"),
      "greaterThan">: lambdaTyped "x" T.unit (string "greater")] @@
     injectUnit (ref testTypeComparisonNameDef) "equalTo")
    T.string]

unionEliminationsWithDataTests :: TTerm TestGroup
unionEliminationsWithDataTests = subgroup "Union eliminations with data" [
  checkTest "match Number extracting int values" []
    (match (ref testTypeNumberNameDef) nothing [
      "int">: lambda "i" (var "i"),
      "float">: lambda "f" (int32 0)])
    (match (ref testTypeNumberNameDef) nothing [
      "int">: lambdaTyped "i" T.int32 (var "i"),
      "float">: lambdaTyped "f" T.float32 (int32 0)])
    (T.function (Core.typeVariable $ ref testTypeNumberNameDef) T.int32),
  checkTest "match Number converting to string" []
    (match (ref testTypeNumberNameDef) nothing [
      "int">: lambda "i" (primitive _literals_showInt32 @@ var "i"),
      "float">: lambda "f" (primitive _literals_showFloat32 @@ var "f")])
    (match (ref testTypeNumberNameDef) nothing [
      "int">: lambdaTyped "i" T.int32 (primitive _literals_showInt32 @@ var "i"),
      "float">: lambdaTyped "f" T.float32 (primitive _literals_showFloat32 @@ var "f")])
    (T.function (Core.typeVariable $ ref testTypeNumberNameDef) T.string),
  checkTest "match Number applied to int variant" []
    (match (ref testTypeNumberNameDef) nothing [
      "int">: lambda "i" (primitive _math_add @@ var "i" @@ int32 10),
      "float">: lambda "f" (int32 0)] @@
     inject (ref testTypeNumberNameDef) "int" (int32 42))
    (match (ref testTypeNumberNameDef) nothing [
      "int">: lambdaTyped "i" T.int32 (primitive _math_add @@ var "i" @@ int32 10),
      "float">: lambdaTyped "f" T.float32 (int32 0)] @@
     inject (ref testTypeNumberNameDef) "int" (int32 42))
    T.int32,
  checkTest "match Timestamp with mixed data types" []
    (match (ref testTypeTimestampNameDef) nothing [
      "unixTimeMillis">: lambda "millis" (primitive _literals_showUint64 @@ var "millis"),
      "date">: lambda "dateStr" (var "dateStr")])
    (match (ref testTypeTimestampNameDef) nothing [
      "unixTimeMillis">: lambdaTyped "millis" T.uint64 (primitive _literals_showUint64 @@ var "millis"),
      "date">: lambdaTyped "dateStr" T.string (var "dateStr")])
    (T.function (Core.typeVariable $ ref testTypeTimestampNameDef) T.string)]

polymorphicUnionEliminationsTests :: TTerm TestGroup
polymorphicUnionEliminationsTests = supergroup "Polymorphic union eliminations" [
  simplePolymorphicUnionTests,
  usingUnionPolymorphicRecursiveTests,
  usingKernelTypesTests]

simplePolymorphicUnionTests :: TTerm TestGroup
simplePolymorphicUnionTests = subgroup "Simple polymorphic unions" [
  checkTest "match PersonOrSomething with string" []
    (match (ref testTypePersonOrSomethingNameDef) nothing [
      "person">: lambda "p" (project (ref testTypePersonNameDef) (name "firstName") @@ var "p"),
      "other">: lambda "x" (var "x")])
    (tyapp (match (ref testTypePersonOrSomethingNameDef) nothing [
      "person">: lambdaTyped "p" (Core.typeVariable $ ref testTypePersonNameDef) (project (ref testTypePersonNameDef) (name "firstName") @@ var "p"),
      "other">: lambdaTyped "x" T.string (var "x")]) T.string)
    (T.function (T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) T.string) T.string),
  checkTest "match PersonOrSomething instantiated with string" []
    (match (ref testTypePersonOrSomethingNameDef) nothing [
      "person">: lambda "p" (project (ref testTypePersonNameDef) (name "firstName") @@ var "p"),
      "other">: lambda "x" (var "x")] @@
     inject (ref testTypePersonOrSomethingNameDef) "other" (string "test"))
    (tyapp (match (ref testTypePersonOrSomethingNameDef) nothing [
      "person">: lambdaTyped "p" (Core.typeVariable $ ref testTypePersonNameDef) (project (ref testTypePersonNameDef) (name "firstName") @@ var "p"),
      "other">: lambdaTyped "x" T.string (var "x")]) T.string @@
     tyapp (inject (ref testTypePersonOrSomethingNameDef) "other" (string "test")) T.string)
    T.string]

usingUnionPolymorphicRecursiveTests :: TTerm TestGroup
usingUnionPolymorphicRecursiveTests = subgroup "using UnionPolymorphicRecursive" [
  checkTest "non-applied UnionPolymorphicRecursive" []
    (lets [
      "test">: (match (ref testTypeUnionPolymorphicRecursiveNameDef)
        (just $ string "other") [
        "value">: lambda "i" $ primitive _literals_showInt32 @@ var "i"])] $
      var "test")
    (letsTyped [
        ("test",
         tyapp (match (ref testTypeUnionPolymorphicRecursiveNameDef)
           (just $ string "other") [
           "value">: lambdaTyped "i" T.int32 $ primitive _literals_showInt32 @@ var "i"]) T.int32,
         T.mono $ T.function (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) T.int32) T.string)] $
      var "test")
    (T.function (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) T.int32) T.string),
  checkTest "applied UnionPolymorphicRecursive with int32" []
    (lets [
      "test">: (match (ref testTypeUnionPolymorphicRecursiveNameDef)
          (just $ string "other") [
          "value">: lambda "i" $ primitive _literals_showInt32 @@ var "i"])
        @@ (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" $ int32 42)] $
      var "test")
    (letsTyped [
      ("test",
       tyapp (match (ref testTypeUnionPolymorphicRecursiveNameDef)
           (just $ string "other") [
           "value">: lambdaTyped "i" T.int32 $ primitive _literals_showInt32 @@ var "i"]) T.int32
         @@ tyapp (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" $ int32 42) T.int32,
       T.mono T.string)] $
      var "test")
    T.string,
  checkTest "applied UnionPolymorphicRecursive with int32 in lambda" []
    (lets [
      "test">: lambda "x" $ match (ref testTypeUnionPolymorphicRecursiveNameDef)
          (just $ string "other") [
          "value">: lambda "i" $ primitive _literals_showInt32 @@ var "i"]
        @@ var "x"] $
      var "test")
    (letsTyped [
      ("test",
       lambdaTyped "x" (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) T.int32) $
         tyapp (match (ref testTypeUnionPolymorphicRecursiveNameDef)
             (just $ string "other") [
             "value">: lambdaTyped "i" T.int32 $ primitive _literals_showInt32 @@ var "i"]) T.int32
           @@ var "x",
       T.mono $ T.function (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) T.int32) T.string)] $
      var "test")
    (T.function (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) T.int32) T.string),
  checkTest "applied generic UnionPolymorphicRecursive in lambda" []
    (lets [
      "test">: lambda "x" $ match (ref testTypeUnionPolymorphicRecursiveNameDef)
          (just $ string "other") [
          "value">: lambda "ignored" $ string "foo"]
        @@ var "x"] $
      var "test")
    (tylam "t0" $ letsTyped [
      ("test",
       tylam "t1" $ lambdaTyped "x" (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) (T.var "t1")) $
         tyapp (match (ref testTypeUnionPolymorphicRecursiveNameDef)
             (just $ string "other") [
             "value">: lambdaTyped "ignored" (T.var "t1") $ string "foo"]) (T.var "t1")
           @@ var "x",
       T.poly ["t1"] $ T.function (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) (T.var "t1")) T.string)] $
      tyapp (var "test") $ T.var "t0")
    (T.forAll "t0" $ T.function (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) (T.var "t0")) T.string)]

usingKernelTypesTests :: TTerm TestGroup
usingKernelTypesTests = subgroup "Using kernel types" [
  checkTest "case statement on CoderDirection applied to argument" []
    (lambda "dir" $
      lambda "coder" $
        match (name "hydra.coders.CoderDirection")
          nothing [
          "encode">: lambda "_" $
            lambda "v12" $
              project (name "hydra.compute.Coder") (name "encode")
                @@ var "coder" @@ var "v12",
          "decode">: lambda "_" $
            lambda "v12" $
              project (name "hydra.compute.Coder") (name "decode")
                @@ var "coder" @@ var "v12"]
          @@ var "dir")
    (tylams ["t0", "t1"] $
      lambdaTyped "dir" (T.var "hydra.coders.CoderDirection") $
        lambdaTyped "coder" (T.applys (T.var "hydra.compute.Coder") (T.var <$> ["t0", "t0", "t1", "t1"])) $
          match (name "hydra.coders.CoderDirection")
            nothing [
            "encode">: lambdaTyped "_" T.unit $
              lambdaTyped "v12" (T.var "t1") $
                tyapps (project (name "hydra.compute.Coder") (name "encode")) (T.var <$> ["t0", "t0", "t1", "t1"])
                  @@ var "coder" @@ var "v12",
            "decode">: lambdaTyped "_" T.unit $
              lambdaTyped "v12" (T.var "t1") $
                tyapps (project (name "hydra.compute.Coder") (name "decode")) (T.var <$> ["t0", "t0", "t1", "t1"])
                  @@ var "coder" @@ var "v12"]
          @@ var "dir")
    (T.forAlls ["t0", "t1"] $
      T.functionMany [
        T.var "hydra.coders.CoderDirection",
        T.applys (T.var "hydra.compute.Coder") (T.var <$> ["t0", "t0", "t1", "t1"]),
        T.var "t1",
        T.applys (Core.typeVariable $ name "hydra.compute.Flow") [T.var "t0", T.var "t1"]])]

unionEliminationsWithDefaultsTests :: TTerm TestGroup
unionEliminationsWithDefaultsTests = subgroup "Union eliminations with defaults" [
  checkTest "match Comparison with default case" []
    (match (ref testTypeComparisonNameDef) (just (string "unknown")) [
      "lessThan">: lambda "x" (string "less"),
      "equalTo">: lambda "x" (string "equal")])
    (match (ref testTypeComparisonNameDef) (just (string "unknown")) [
      "lessThan">: lambdaTyped "x" T.unit (string "less"),
      "equalTo">: lambdaTyped "x" T.unit (string "equal")])
    (T.function (Core.typeVariable $ ref testTypeComparisonNameDef) T.string),
  checkTest "match Number with default case" []
    (match (ref testTypeNumberNameDef) (just (int32 (-1))) [
      "int">: lambda "i" (var "i")])
    (match (ref testTypeNumberNameDef) (just (int32 (-1))) [
      "int">: lambdaTyped "i" T.int32 (var "i")])
    (T.function (Core.typeVariable $ ref testTypeNumberNameDef) T.int32),
  checkTest "match UnionMonomorphic with default" []
    (match (ref testTypeUnionMonomorphicNameDef) (just (string "fallback")) [
      "bool">: lambda "b" (primitive _literals_showBoolean @@ var "b"),
      "string">: lambda "s" (var "s")])
    (match (ref testTypeUnionMonomorphicNameDef) (just (string "fallback")) [
      "bool">: lambdaTyped "b" T.boolean (primitive _literals_showBoolean @@ var "b"),
      "string">: lambdaTyped "s" T.string (var "s")])
    (T.function (Core.typeVariable $ ref testTypeUnionMonomorphicNameDef) T.string)]

nestedUnionEliminationsTests :: TTerm TestGroup
nestedUnionEliminationsTests = subgroup "Nested union eliminations" [
  checkTest "nested match statements" []
    (match (ref testTypePersonOrSomethingNameDef) nothing [
      "person">: lambda "p" (project (ref testTypePersonNameDef) (name "firstName") @@ var "p"),
      "other">: lambda "x" (
        match (ref testTypeNumberNameDef) nothing [
          "int">: lambda "i" (primitive _literals_showInt32 @@ var "i"),
          "float">: lambda "f" (primitive _literals_showFloat32 @@ var "f")] @@
        var "x")])
    (tyapp (match (ref testTypePersonOrSomethingNameDef) nothing [
      "person">: lambdaTyped "p" (Core.typeVariable $ ref testTypePersonNameDef) (project (ref testTypePersonNameDef) (name "firstName") @@ var "p"),
      "other">: lambdaTyped "x" (Core.typeVariable $ ref testTypeNumberNameDef) (
        match (ref testTypeNumberNameDef) nothing [
          "int">: lambdaTyped "i" T.int32 (primitive _literals_showInt32 @@ var "i"),
          "float">: lambdaTyped "f" T.float32 (primitive _literals_showFloat32 @@ var "f")] @@
        var "x")]) (Core.typeVariable $ ref testTypeNumberNameDef))
    (T.function (T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) (Core.typeVariable $ ref testTypeNumberNameDef)) T.string),
  checkTest "match in tuple" []
    (tuple [
      match (ref testTypeComparisonNameDef) nothing [
        "lessThan">: lambda "x" (int32 1),
        "equalTo">: lambda "x" (int32 0),
        "greaterThan">: lambda "x" (int32 (-1))],
      string "context"])
    (tuple [
      match (ref testTypeComparisonNameDef) nothing [
        "lessThan">: lambdaTyped "x" T.unit (int32 1),
        "equalTo">: lambdaTyped "x" T.unit (int32 0),
        "greaterThan">: lambdaTyped "x" T.unit (int32 (-1))],
      string "context"])
    (T.product [T.function (Core.typeVariable $ ref testTypeComparisonNameDef) T.int32, T.string])]

unionEliminationsInComplexContextsTests :: TTerm TestGroup
unionEliminationsInComplexContextsTests = subgroup "Union eliminations in complex contexts" [
  checkTest "match in let binding" []
    (lets ["matcher">: match (ref testTypeComparisonNameDef) nothing [
             "lessThan">: lambda "x" (string "less"),
             "equalTo">: lambda "x" (string "equal"),
             "greaterThan">: lambda "x" (string "greater")]] $
          var "matcher")
    (letsTyped [("matcher", match (ref testTypeComparisonNameDef) nothing [
                   "lessThan">: lambdaTyped "x" T.unit (string "less"),
                   "equalTo">: lambdaTyped "x" T.unit (string "equal"),
                   "greaterThan">: lambdaTyped "x" T.unit (string "greater")],
                 T.mono $ T.function (Core.typeVariable $ ref testTypeComparisonNameDef) T.string)] $
      var "matcher")
    (T.function (Core.typeVariable $ ref testTypeComparisonNameDef) T.string),
  checkTest "match in record" []
    (record (ref testTypePersonNameDef) [
      "firstName">: (match (ref testTypePersonOrSomethingNameDef) nothing [
        "person">: lambda "p" (project (ref testTypePersonNameDef) (name "firstName") @@ var "p"),
        "other">: lambda "x" (var "x")] @@
       inject (ref testTypePersonOrSomethingNameDef) "other" (string "John")),
      "lastName">: (string "Doe"),
      "age">: (int32 30)])
    (record (ref testTypePersonNameDef) [
      "firstName">: (tyapp (match (ref testTypePersonOrSomethingNameDef) nothing [
        "person">: lambdaTyped "p" (Core.typeVariable $ ref testTypePersonNameDef) (project (ref testTypePersonNameDef) (name "firstName") @@ var "p"),
        "other">: lambdaTyped "x" T.string (var "x")]) T.string @@
       tyapp (inject (ref testTypePersonOrSomethingNameDef) "other" (string "John")) T.string),
      "lastName">: (string "Doe"),
      "age">: (int32 30)])
    (Core.typeVariable $ ref testTypePersonNameDef),
  checkTest "match with polymorphic result in list" []
    (list [
      match (ref testTypePersonOrSomethingNameDef) nothing [
        "person">: lambda "p" (project (ref testTypePersonNameDef) (name "age") @@ var "p"),
        "other">: lambda "x" (var "x")] @@
      inject (ref testTypePersonOrSomethingNameDef) "other" (int32 25),
      int32 30])
    (list [
      tyapp (match (ref testTypePersonOrSomethingNameDef) nothing [
        "person">: lambdaTyped "p" (Core.typeVariable $ ref testTypePersonNameDef) (project (ref testTypePersonNameDef) (name "age") @@ var "p"),
        "other">: lambdaTyped "x" T.int32 (var "x")]) T.int32 @@
      tyapp (inject (ref testTypePersonOrSomethingNameDef) "other" (int32 25)) T.int32,
      int32 30])
    (T.list T.int32)]

multiParameterPolymorphicCaseStatementsTests :: TTerm TestGroup
multiParameterPolymorphicCaseStatementsTests = subgroup "Multi-parameter polymorphic case statements" [
  checkTest "case Either converting both to string" []
    (match (ref testTypeEitherNameDef) nothing [
      "left">: lambda "x" $ primitive _literals_showInt32 @@ var "x",
      "right">: lambda "y" $ primitive _literals_showFloat32 @@ var "y"])
    (tyapps (match (ref testTypeEitherNameDef) nothing [
      "left">: lambdaTyped "x" T.int32 (primitive _literals_showInt32 @@ var "x"),
      "right">: lambdaTyped "y" T.float32 (primitive _literals_showFloat32 @@ var "y")]) [T.int32, T.float32])
    (T.function
      (T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.int32, T.float32])
      T.string),
  checkTest "case Either applied to injection" []
    (match (ref testTypeEitherNameDef) nothing [
      "left">: lambda "n" $ primitive _math_add @@ var "n" @@ int32 1,
      "right">: lambda "s" $ primitive _strings_length @@ var "s"] @@
     inject (ref testTypeEitherNameDef) "left" (int32 42))
    (tyapps (match (ref testTypeEitherNameDef) nothing [
      "left">: lambdaTyped "n" T.int32 (primitive _math_add @@ var "n" @@ int32 1),
      "right">: lambdaTyped "s" T.string (primitive _strings_length @@ var "s")]) [T.int32, T.string] @@
     tyapps (inject (ref testTypeEitherNameDef) "left" (int32 42)) [T.int32, T.string])
    T.int32,
  checkTest "case Either with Triple and nested projections" []
    (lambda "triple" $
      match (ref testTypeEitherNameDef) nothing [
        "left">: lambda "coords" $
          project (ref testTypeLatLonPolyNameDef) (name "lat") @@ var "coords",
        "right">: lambda "t" $
          project (ref testTypeTripleNameDef) (name "first") @@ var "t"] @@
      (project (ref testTypeTripleNameDef) (name "second") @@ var "triple"))
    (tylams ["t0", "t1", "t2", "t3", "t4"] $
      lambdaTyped "triple"
        (T.applys (Core.typeVariable $ ref testTypeTripleNameDef)
          [T.var "t0",
           T.applys (Core.typeVariable $ ref testTypeEitherNameDef)
             [T.apply (Core.typeVariable $ ref testTypeLatLonPolyNameDef) (T.var "t1"),
              T.applys (Core.typeVariable $ ref testTypeTripleNameDef) [T.var "t1", T.var "t2", T.var "t3"]],
           T.var "t4"]) $
      tyapps (match (ref testTypeEitherNameDef) nothing [
        "left">: lambdaTyped "coords" (T.apply (Core.typeVariable $ ref testTypeLatLonPolyNameDef) (T.var "t1")) $
          tyapp (project (ref testTypeLatLonPolyNameDef) (name "lat")) (T.var "t1") @@ var "coords",
        "right">: lambdaTyped "t" (T.applys (Core.typeVariable $ ref testTypeTripleNameDef) [T.var "t1", T.var "t2", T.var "t3"]) $
          tyapps (project (ref testTypeTripleNameDef) (name "first")) [T.var "t1", T.var "t2", T.var "t3"] @@ var "t"])
        [T.apply (Core.typeVariable $ ref testTypeLatLonPolyNameDef) (T.var "t1"),
         T.applys (Core.typeVariable $ ref testTypeTripleNameDef) [T.var "t1", T.var "t2", T.var "t3"]] @@
      (tyapps (project (ref testTypeTripleNameDef) (name "second"))
        [T.var "t0",
         T.applys (Core.typeVariable $ ref testTypeEitherNameDef)
           [T.apply (Core.typeVariable $ ref testTypeLatLonPolyNameDef) (T.var "t1"),
            T.applys (Core.typeVariable $ ref testTypeTripleNameDef) [T.var "t1", T.var "t2", T.var "t3"]],
         T.var "t4"] @@ var "triple"))
    (T.forAlls ["t0", "t1", "t2", "t3", "t4"] $
      T.function
        (T.applys (Core.typeVariable $ ref testTypeTripleNameDef)
          [T.var "t0",
           T.applys (Core.typeVariable $ ref testTypeEitherNameDef)
             [T.apply (Core.typeVariable $ ref testTypeLatLonPolyNameDef) (T.var "t1"),
              T.applys (Core.typeVariable $ ref testTypeTripleNameDef) [T.var "t1", T.var "t2", T.var "t3"]],
           T.var "t4"])
        (T.var "t1")),
  checkTest "case Either with polymorphic let bindings" []
    (lets ["makeLeft">: lambda "x" $ inject (ref testTypeEitherNameDef) "left" (var "x"),
           "makeRight">: lambda "y" $ inject (ref testTypeEitherNameDef) "right" (var "y")] $
      lambda "flag" $
        match (ref testTypeEitherNameDef) nothing [
          "left">: lambda "n" $ var "makeRight" @@ (primitive _math_add @@ var "n" @@ int32 10),
          "right">: lambda "s" $ var "makeLeft" @@ (primitive _strings_length @@ var "s")] @@
        var "flag")
    (letsTyped [("makeLeft", tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ tyapps (inject (ref testTypeEitherNameDef) "left" (var "x")) [T.var "t0", T.var "t1"],
                 T.poly ["t0", "t1"] $ T.function (T.var "t0") (T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t0", T.var "t1"])),
                ("makeRight", tylams ["t0", "t1"] $ lambdaTyped "y" (T.var "t0") $ tyapps (inject (ref testTypeEitherNameDef) "right" (var "y")) [T.var "t1", T.var "t0"],
                 T.poly ["t0", "t1"] $ T.function (T.var "t0") (T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t1", T.var "t0"]))] $
      lambdaTyped "flag" (T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.int32, T.string]) $
        tyapps (match (ref testTypeEitherNameDef) nothing [
          "left">: lambdaTyped "n" T.int32 $ tyapps (var "makeRight") [T.int32, T.int32] @@ (primitive _math_add @@ var "n" @@ int32 10),
          "right">: lambdaTyped "s" T.string $ tyapps (var "makeLeft") [T.int32, T.int32] @@ (primitive _strings_length @@ var "s")]) [T.int32, T.string] @@
        var "flag")
    (T.function (T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.int32, T.string]) (T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.int32, T.int32]))]

higherOrderUnionEliminationsTests :: TTerm TestGroup
higherOrderUnionEliminationsTests = subgroup "Higher-order union eliminations" [
  checkTest "map match over list" []
    (primitive _lists_map @@
     (match (ref testTypeComparisonNameDef) nothing [
       "lessThan">: lambda "x" (string "less"),
       "equalTo">: lambda "x" (string "equal"),
       "greaterThan">: lambda "x" (string "greater")]) @@
     list [injectUnit (ref testTypeComparisonNameDef) "lessThan",
           injectUnit (ref testTypeComparisonNameDef) "equalTo"])
    (tyapps (primitive _lists_map) [Core.typeVariable $ ref testTypeComparisonNameDef, T.string] @@
     (match (ref testTypeComparisonNameDef) nothing [
       "lessThan">: lambdaTyped "x" T.unit (string "less"),
       "equalTo">: lambdaTyped "x" T.unit (string "equal"),
       "greaterThan">: lambdaTyped "x" T.unit (string "greater")]) @@
     list [injectUnit (ref testTypeComparisonNameDef) "lessThan",
           injectUnit (ref testTypeComparisonNameDef) "equalTo"])
    (T.list T.string),
  checkTest "compose match with other functions" []
    (lambda "comp" $
     primitive _strings_length @@
     (match (ref testTypeComparisonNameDef) nothing [
       "lessThan">: lambda "x" (string "less"),
       "equalTo">: lambda "x" (string "equal"),
       "greaterThan">: lambda "x" (string "greater")] @@
      var "comp"))
    (lambdaTyped "comp" (Core.typeVariable $ ref testTypeComparisonNameDef) $
     primitive _strings_length @@
     (match (ref testTypeComparisonNameDef) nothing [
       "lessThan">: lambdaTyped "x" T.unit (string "less"),
       "equalTo">: lambdaTyped "x" T.unit (string "equal"),
       "greaterThan">: lambdaTyped "x" T.unit (string "greater")] @@
      var "comp"))
    (T.function (Core.typeVariable $ ref testTypeComparisonNameDef) T.int32),
  checkTest "match in lambda body" []
    (lambda "unionValue" $
     match (ref testTypeNumberNameDef) nothing [
       "int">: lambda "i" (primitive _math_add @@ var "i" @@ int32 1),
       "float">: lambda "f" (int32 0)] @@
     var "unionValue")
    (lambdaTyped "unionValue" (Core.typeVariable $ ref testTypeNumberNameDef) $
     match (ref testTypeNumberNameDef) nothing [
       "int">: lambdaTyped "i" T.int32 (primitive _math_add @@ var "i" @@ int32 1),
       "float">: lambdaTyped "f" T.float32 (int32 0)] @@
     var "unionValue")
    (T.function (Core.typeVariable $ ref testTypeNumberNameDef) T.int32)]

recursiveUnionEliminationsTests :: TTerm TestGroup
recursiveUnionEliminationsTests = subgroup "Recursive union eliminations" [
  checkTest "match HydraType recursively" []
    (match (ref testTypeHydraTypeNameDef) nothing [
      "literal">: lambda "lit" (
        match (ref testTypeHydraLiteralTypeNameDef) nothing [
          "boolean">: lambda "b" (primitive _literals_showBoolean @@ var "b"),
          "string">: lambda "s" (var "s")] @@
        var "lit"),
      "list">: lambda "nested" (string "list")])
    (match (ref testTypeHydraTypeNameDef) nothing [
      "literal">: lambdaTyped "lit" (Core.typeVariable $ ref testTypeHydraLiteralTypeNameDef) (
        match (ref testTypeHydraLiteralTypeNameDef) nothing [
          "boolean">: lambdaTyped "b" T.boolean (primitive _literals_showBoolean @@ var "b"),
          "string">: lambdaTyped "s" T.string (var "s")] @@
        var "lit"),
      "list">: lambdaTyped "nested" (Core.typeVariable $ ref testTypeHydraTypeNameDef) (string "list")])
    (T.function (Core.typeVariable $ ref testTypeHydraTypeNameDef) T.string)]

------ Unit ------

unitTests :: TTerm TestGroup
unitTests = supergroup "Unit" [
  unitTermTests,
  unitTermInPolymorphicContextTests]

unitTermTests :: TTerm TestGroup
unitTermTests = subgroup "Unit term" [
  noChange "unit literal"
    unit
    T.unit]

unitTermInPolymorphicContextTests :: TTerm TestGroup
unitTermInPolymorphicContextTests = subgroup "Unit term in polymorphic context" [
  checkTest "unit from lambda" []
    (lambda "x" unit)
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") unit)
    (T.forAlls ["t0"] $ T.function (T.var "t0") T.unit),
  noChange "unit in tuple"
    (tuple [unit, string "foo"])
    (T.product [T.unit, T.string])]

------ Variables ------

variablesTests :: TTerm TestGroup
variablesTests = supergroup "Variables" [
  simpleVariableLookupTests,
  variableScopingTests,
  polymorphicVariablesTests,
  variablesInComplexContextsTests,
  recursiveVariablesTests]

simpleVariableLookupTests :: TTerm TestGroup
simpleVariableLookupTests = subgroup "Simple variable lookup" [
  checkTest "int variable" []
    (lambda "x" $ var "x")
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ var "x")
    (T.forAll "t0" $ T.function (T.var "t0") (T.var "t0")),
  checkTest "variable in let binding" []
    (lets ["x">: int32 42] $ var "x")
    (letsTyped [("x", int32 42, T.mono T.int32)] $ var "x")
    T.int32,
  checkTest "multiple variables" []
    (lets ["x">: string "hello",
           "y">: int32 42] $
          tuple [var "x", var "y"])
    (letsTyped [("x", string "hello", T.mono T.string),
                ("y", int32 42, T.mono T.int32)] $
      tuple [var "x", var "y"])
    (T.product [T.string, T.int32])]

variableScopingTests :: TTerm TestGroup
variableScopingTests = subgroup "Variable scoping" [
  checkTest "lambda parameter" []
    (lambda "x" $ lambda "y" $ var "x")
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t1") $ var "x")
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.function (T.var "t1") (T.var "t0"))),
  checkTest "let binding scope" []
    (lets ["x">: int32 1] $
     lets ["y">: string "hello"] $
          var "x")
    (letsTyped [("x", int32 1, T.mono T.int32)] $
     letsTyped [("y", string "hello", T.mono T.string)] $
      var "x")
    T.int32,
  checkTest "variable shadowing" []
    (lets ["x">: int32 1] $
     lambda "x" $ var "x")
    (tylam "t0" $ letsTyped [("x", int32 1, T.mono T.int32)] $
      lambdaTyped "x" (T.var "t0") $ var "x")
    (T.forAll "t0" $ T.function (T.var "t0") (T.var "t0")),
  checkTest "nested scoping" []
    (lambda "x" $
     lets ["y">: var "x"] $
          lambda "z" $
          tuple [var "x", var "y", var "z"])
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $
     letsTyped [("y", var "x", T.mono (T.var "t0"))] $
      lambdaTyped "z" (T.var "t1") $
      tuple [var "x", var "y", var "z"])
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.function (T.var "t1") (T.product [T.var "t0", T.var "t0", T.var "t1"])))]

polymorphicVariablesTests :: TTerm TestGroup
polymorphicVariablesTests = subgroup "Polymorphic variables" [
  checkTest "polymorphic function" []
    (lets ["id">: lambda "x" $ var "x"] $
          var "id")
    (tylam "t0" $ letsTyped [("id", tylam "t1" $ lambdaTyped "x" (T.var "t1") $ var "x",
                              T.poly ["t1"] $ T.function (T.var "t1") (T.var "t1"))] $
      tyapp (var "id") (T.var "t0"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.var "t0")),
  checkTest "polymorphic application" []
    (lets ["id">: lambda "x" $ var "x"] $
          tuple [var "id" @@ int32 42, var "id" @@ string "test"])
    (letsTyped [("id", tylam "t0" $ lambdaTyped "x" (T.var "t0") $ var "x",
                 T.poly ["t0"] $ T.function (T.var "t0") (T.var "t0"))] $
      tuple [tyapp (var "id") T.int32 @@ int32 42, tyapp (var "id") T.string @@ string "test"])
    (T.product [T.int32, T.string]),
  checkTest "higher order polymorphic" []
    (lets ["apply">: lambda "f" $ lambda "x" $ var "f" @@ var "x"] $
          var "apply")
    (tylams ["t0", "t1"] $ letsTyped [("apply", tylams ["t2", "t3"] $ lambdaTyped "f" (T.function (T.var "t2") (T.var "t3")) $ lambdaTyped "x" (T.var "t2") $ var "f" @@ var "x",
                                       T.poly ["t2", "t3"] $ T.function (T.function (T.var "t2") (T.var "t3")) (T.function (T.var "t2") (T.var "t3")))] $
      tyapps (var "apply") [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $
      T.function (T.function (T.var "t0") (T.var "t1")) (T.function (T.var "t0") (T.var "t1")))]

variablesInComplexContextsTests :: TTerm TestGroup
variablesInComplexContextsTests = subgroup "Variables in complex contexts" [
  checkTest "variable in record" []
    (lambda "name" $
     record (ref testTypePersonNameDef) [
       "firstName">: (var "name"),
       "lastName">: (string "Doe"),
       "age">: (int32 25)])
    (lambdaTyped "name" T.string $
     record (ref testTypePersonNameDef) [
       "firstName">: (var "name"),
       "lastName">: (string "Doe"),
       "age">: (int32 25)])
    (T.function T.string (Core.typeVariable $ ref testTypePersonNameDef)),
  checkTest "variable in list" []
    (lambda "x" $ list [var "x", var "x"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ list [var "x", var "x"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.list $ T.var "t0")),
  checkTest "variable in map" []
    (lambda "key" $ lambda "value" $
      mapTerm [(var "key", var "value")])
    (tylams ["t0", "t1"] $ lambdaTyped "key" (T.var "t0") $ lambdaTyped "value" (T.var "t1") $
      mapTerm [(var "key", var "value")])
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.function (T.var "t1") (T.map (T.var "t0") (T.var "t1")))),
  checkTest "variable in optional" []
    (lambda "x" $ Core.termMaybe $ just $ var "x")
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ Core.termMaybe $ just $ var "x")
    (T.forAll "t0" $ T.function (T.var "t0") (T.optional $ T.var "t0"))]

recursiveVariablesTests :: TTerm TestGroup
recursiveVariablesTests = subgroup "Recursive variables" [
  checkTest "simple recursion" []
    (lets ["f">: lambda "x" $ primitive _math_add @@ var "x" @@ int32 1] $
          var "f")
    (letsTyped [("f", lambdaTyped "x" T.int32 $ primitive _math_add @@ var "x" @@ int32 1,
                 T.mono $ T.function T.int32 T.int32)] $
      var "f")
    (T.function T.int32 T.int32),
  checkTest "mutual recursion" []
    (lets ["f">: lambda "x" $ var "g" @@ var "x",
           "g">: lambda "y" $ primitive _math_add @@ var "y" @@ int32 1] $
          var "f")
    (letsTyped [("f", lambdaTyped "x" T.int32 $ var "g" @@ var "x",
                 T.mono $ T.function T.int32 T.int32),
                ("g", lambdaTyped "y" T.int32 $ primitive _math_add @@ var "y" @@ int32 1,
                 T.mono $ T.function T.int32 T.int32)] $
      var "f")
    (T.function T.int32 T.int32)]

------ Wrapped terms ------

wrappedTermsTests :: TTerm TestGroup
wrappedTermsTests = supergroup "Wrapped terms" [
  monomorphicWrappedTermsTests,
  polymorphicWrappedTermsTests,
  wrappedTermsInComplexContextsTests,
  nestedWrappedTermsTests,
  multipleWrappingLevelsTests,
  multiParameterPolymorphicWrappersTests]

monomorphicWrappedTermsTests :: TTerm TestGroup
monomorphicWrappedTermsTests = subgroup "Monomorphic wrapped terms" [
  noChange "string alias"
    (wrap (ref testTypeStringAliasNameDef) (string "hello"))
    (Core.typeVariable $ ref testTypeStringAliasNameDef),
  noChange "wrapped integer"
    (wrap (ref testTypeStringAliasNameDef) (string "wrapped"))
    (Core.typeVariable $ ref testTypeStringAliasNameDef),
  noChange "wrapped in tuple"
    (tuple [wrap (ref testTypeStringAliasNameDef) (string "first"),
            string "second"])
    (T.product [Core.typeVariable $ ref testTypeStringAliasNameDef, T.string])]

polymorphicWrappedTermsTests :: TTerm TestGroup
polymorphicWrappedTermsTests = subgroup "Polymorphic wrapped terms" [
  checkTest "polymorphic wrapper with int" []
    (wrap (ref testTypePolymorphicWrapperNameDef) (list [int32 1, int32 2]))
    (tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [int32 1, int32 2])) T.int32)
    (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) T.int32),
  checkTest "polymorphic wrapper with string" []
    (wrap (ref testTypePolymorphicWrapperNameDef) (list [string "a", string "b"]))
    (tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [string "a", string "b"])) T.string)
    (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) T.string),
  checkTest "polymorphic wrapper from lambda" []
    (lambda "x" $ wrap (ref testTypePolymorphicWrapperNameDef) (list [var "x"]))
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [var "x"])) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) (T.var "t0")))]

wrappedTermsInComplexContextsTests :: TTerm TestGroup
wrappedTermsInComplexContextsTests = subgroup "Wrapped terms in complex contexts" [
  noChange "wrapped in record"
    (record (ref testTypePersonNameDef) [
      "firstName">: (string "John"),
      "lastName">: (string "Doe"),
      "age">: (int32 30)])
    (Core.typeVariable $ ref testTypePersonNameDef),
  checkTest "wrapped in let binding" []
    (lets ["alias">: wrap (ref testTypeStringAliasNameDef) (string "test")] $
          var "alias")
    (letsTyped [("alias", wrap (ref testTypeStringAliasNameDef) (string "test"),
                 T.mono $ Core.typeVariable $ ref testTypeStringAliasNameDef)] $
      var "alias")
    (Core.typeVariable $ ref testTypeStringAliasNameDef),
  noChange "wrapped in list"
    (list [wrap (ref testTypeStringAliasNameDef) (string "first"),
           wrap (ref testTypeStringAliasNameDef) (string "second")])
    (T.list $ Core.typeVariable $ ref testTypeStringAliasNameDef)]

nestedWrappedTermsTests :: TTerm TestGroup
nestedWrappedTermsTests = subgroup "Nested wrapped terms" [
  checkTest "wrapped tuple" []
    (wrap (ref testTypePolymorphicWrapperNameDef) (list [tuple [int32 1, string "a"]]))
    (tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [tuple [int32 1, string "a"]])) (T.product [T.int32, T.string]))
    (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) (T.product [T.int32, T.string])),
  checkTest "wrapped optional" []
    (wrap (ref testTypePolymorphicWrapperNameDef) (list [Core.termMaybe $ just $ int32 42]))
    (tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [Core.termMaybe $ just $ int32 42])) (T.optional T.int32))
    (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) (T.optional T.int32)),
  checkTest "wrapped map" []
    (wrap (ref testTypePolymorphicWrapperNameDef) (list [mapTerm [(string "key", int32 42)]]))
    (tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [mapTerm [(string "key", int32 42)]])) (T.map T.string T.int32))
    (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) (T.map T.string T.int32))]

multipleWrappingLevelsTests :: TTerm TestGroup
multipleWrappingLevelsTests = subgroup "Multiple wrapping levels" [
  noChange "wrapped in optional"
    (Core.termMaybe $ just $ wrap (ref testTypeStringAliasNameDef) (string "wrapped"))
    (T.optional $ Core.typeVariable $ ref testTypeStringAliasNameDef),
  checkTest "list of wrapped polymorphic" []
    (list [wrap (ref testTypePolymorphicWrapperNameDef) (list [int32 1]),
           wrap (ref testTypePolymorphicWrapperNameDef) (list [int32 2])])
    (list [tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [int32 1])) T.int32,
           tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [int32 2])) T.int32])
    (T.list $ T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) T.int32)]

multiParameterPolymorphicWrappersTests :: TTerm TestGroup
multiParameterPolymorphicWrappersTests = subgroup "Multi-parameter polymorphic wrappers" [
  checkTest "symmetric triple wrapping simple types" []
    (wrap (ref testTypeSymmetricTripleNameDef) $
      record (ref testTypeTripleNameDef) [
        "first">: (int32 1),
        "second">: (string "edge"),
        "third">: (int32 2)])
    (tyapps (wrap (ref testTypeSymmetricTripleNameDef) $
      tyapps (record (ref testTypeTripleNameDef) [
        "first">: (int32 1),
        "second">: (string "edge"),
        "third">: (int32 2)]) [T.int32, T.string, T.int32])
      [T.int32, T.string])
    (T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.int32, T.string]),
  checkTest "symmetric triple from lambda" []
    (lambda "v1" $ lambda "e" $ lambda "v2" $
      wrap (ref testTypeSymmetricTripleNameDef) $
        record (ref testTypeTripleNameDef) [
          "first">: (var "v1"),
          "second">: (var "e"),
          "third">: (var "v2")])
    (tylams ["t0", "t1"] $
      lambdaTyped "v1" (T.var "t0") $
      lambdaTyped "e" (T.var "t1") $
      lambdaTyped "v2" (T.var "t0") $
      tyapps (wrap (ref testTypeSymmetricTripleNameDef) $
        tyapps (record (ref testTypeTripleNameDef) [
          "first">: (var "v1"),
          "second">: (var "e"),
          "third">: (var "v2")]) [T.var "t0", T.var "t1", T.var "t0"])
        [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $
      T.function (T.var "t0") $
      T.function (T.var "t1") $
      T.function (T.var "t0") $
      T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"]),
  checkTest "symmetric triple with nested polymorphic types and foldl" []
    (lets ["sumList">: lambda "lst" $
            primitive _lists_foldl @@
            (lambda "acc" $ lambda "x" $ primitive _math_add @@ var "acc" @@ var "x") @@
            int32 0 @@
            var "lst"] $
      lambda "nums1" $ lambda "nums2" $
        wrap (ref testTypeSymmetricTripleNameDef) $
          record (ref testTypeTripleNameDef) [
            "first">: (var "sumList" @@ var "nums1"),
            "second">: (list [var "nums1", var "nums2"]),
            "third">: (var "sumList" @@ var "nums2")])
    (letsTyped [("sumList",
                 lambdaTyped "lst" (T.list T.int32) $
                   tyapps (primitive _lists_foldl) [T.int32, T.int32] @@
                   (lambdaTyped "acc" T.int32 $ lambdaTyped "x" T.int32 $ primitive _math_add @@ var "acc" @@ var "x") @@
                   int32 0 @@
                   var "lst",
                 T.mono $ T.function (T.list T.int32) T.int32)] $
      lambdaTyped "nums1" (T.list T.int32) $
      lambdaTyped "nums2" (T.list T.int32) $
        tyapps (wrap (ref testTypeSymmetricTripleNameDef) $
          tyapps (record (ref testTypeTripleNameDef) [
            "first">: (var "sumList" @@ var "nums1"),
            "second">: (list [var "nums1", var "nums2"]),
            "third">: (var "sumList" @@ var "nums2")])
            [T.int32, T.list (T.list T.int32), T.int32])
          [T.int32, T.list (T.list T.int32)])
    (T.function (T.list T.int32) $
      T.function (T.list T.int32) $
      T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.int32, T.list (T.list T.int32)])]

------ Wrap eliminations ------

wrapEliminationsTests :: TTerm TestGroup
wrapEliminationsTests = supergroup "Wrap eliminations" [
  monomorphicUnwrappingTests,
  polymorphicUnwrappingTests,
  unwrapEliminationsInApplicationsTests,
  unwrapInComplexContextsTests,
  multiParameterPolymorphicUnwrappersTests,
  chainedUnwrappingTests,
  multipleUnwrapOperationsTests]

monomorphicUnwrappingTests :: TTerm TestGroup
monomorphicUnwrappingTests = subgroup "Monomorphic unwrapping" [
  noChange "unwrap string alias"
    (unwrap (ref testTypeStringAliasNameDef))
    (T.function (Core.typeVariable $ ref testTypeStringAliasNameDef) T.string)]

polymorphicUnwrappingTests :: TTerm TestGroup
polymorphicUnwrappingTests = subgroup "Polymorphic unwrapping" [
  checkTest "unwrap polymorphic wrapper" []
    (unwrap (ref testTypePolymorphicWrapperNameDef))
    (tylam "t0" $ tyapp (unwrap (ref testTypePolymorphicWrapperNameDef)) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) (T.var "t0")) (T.list $ T.var "t0"))]

unwrapEliminationsInApplicationsTests :: TTerm TestGroup
unwrapEliminationsInApplicationsTests = subgroup "Unwrap eliminations in applications" [
  noChange "unwrap applied to wrapped term"
    (unwrap (ref testTypeStringAliasNameDef) @@ wrap (ref testTypeStringAliasNameDef) (string "hello"))
    T.string,
  checkTest "unwrap polymorphic applied" []
    (unwrap (ref testTypePolymorphicWrapperNameDef) @@ wrap (ref testTypePolymorphicWrapperNameDef) (list [int32 1, int32 2]))
    (tyapp (unwrap (ref testTypePolymorphicWrapperNameDef)) T.int32 @@ tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [int32 1, int32 2])) T.int32)
    (T.list T.int32)]

unwrapInComplexContextsTests :: TTerm TestGroup
unwrapInComplexContextsTests = subgroup "Unwrap in complex contexts" [
  checkTest "unwrap in let binding" []
    (lets ["unwrapper" >: unwrap (ref testTypeStringAliasNameDef),
           "wrapped" >: wrap (ref testTypeStringAliasNameDef) (string "test")] $
          var "unwrapper" @@ var "wrapped")
    (letsTyped [
      ("unwrapper", unwrap (ref testTypeStringAliasNameDef), T.mono $ T.function (Core.typeVariable $ ref testTypeStringAliasNameDef) T.string),
      ("wrapped", wrap (ref testTypeStringAliasNameDef) (string "test"), T.mono $ Core.typeVariable $ ref testTypeStringAliasNameDef)] $
      var "unwrapper" @@ var "wrapped")
    T.string,
  noChange "unwrap in tuple"
    (tuple [unwrap (ref testTypeStringAliasNameDef), string "context"])
    (T.product [T.function (Core.typeVariable $ ref testTypeStringAliasNameDef) T.string, T.string]),
  checkTest "unwrap in lambda" []
    (lambda "wrapped" $ unwrap (ref testTypeStringAliasNameDef) @@ var "wrapped")
    (lambdaTyped "wrapped" (Core.typeVariable $ ref testTypeStringAliasNameDef) $ unwrap (ref testTypeStringAliasNameDef) @@ var "wrapped")
    (T.function (Core.typeVariable $ ref testTypeStringAliasNameDef) T.string)]

multiParameterPolymorphicUnwrappersTests :: TTerm TestGroup
multiParameterPolymorphicUnwrappersTests = subgroup "Multi-parameter polymorphic unwrappers" [
  checkTest "unwrap symmetric triple to tuple" []
    (lambda "st" $
      tuple [
        project (ref testTypeTripleNameDef) (name "first") @@ (unwrap (ref testTypeSymmetricTripleNameDef) @@ var "st"),
        project (ref testTypeTripleNameDef) (name "third") @@ (unwrap (ref testTypeSymmetricTripleNameDef) @@ var "st")])
    (tylams ["t0", "t1"] $
      lambdaTyped "st" (T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"]) $
      tuple [
        tyapps (project (ref testTypeTripleNameDef) (name "first")) [T.var "t0", T.var "t1", T.var "t0"] @@
          (tyapps (unwrap (ref testTypeSymmetricTripleNameDef)) [T.var "t0", T.var "t1"] @@ var "st"),
        tyapps (project (ref testTypeTripleNameDef) (name "third")) [T.var "t0", T.var "t1", T.var "t0"] @@
          (tyapps (unwrap (ref testTypeSymmetricTripleNameDef)) [T.var "t0", T.var "t1"] @@ var "st")])
    (T.forAlls ["t0", "t1"] $
      T.function
        (T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"])
        (T.product [T.var "t0", T.var "t0"])),
  checkTest "unwrap and collect edges in set" []
    (lets ["getEdge" >: lambda "st" $
            project (ref testTypeTripleNameDef) (name "second") @@ (unwrap (ref testTypeSymmetricTripleNameDef) @@ var "st")] $
      lambda "triples" $
        primitive _sets_map @@ var "getEdge" @@ var "triples")
    (tylams ["t0", "t1"] $
      letsTyped [("getEdge",
                 tylams ["t2", "t3"] $
                 lambdaTyped "st" (T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t2", T.var "t3"]) $
                   tyapps (project (ref testTypeTripleNameDef) (name "second")) [T.var "t2", T.var "t3", T.var "t2"] @@
                   (tyapps (unwrap (ref testTypeSymmetricTripleNameDef)) [T.var "t2", T.var "t3"] @@ var "st"),
                 T.poly ["t2", "t3"] $ T.function
                   (T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t2", T.var "t3"])
                   (T.var "t3"))] $
      lambdaTyped "triples" (T.set $ T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"]) $
        tyapps (primitive _sets_map) [T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"], T.var "t1"] @@
        (tyapps (var "getEdge") [T.var "t0", T.var "t1"]) @@
        var "triples")
    (T.forAlls ["t0", "t1"] $
      T.function
        (T.set $ T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"])
        (T.set $ T.var "t1")),

  checkTest "unwrap with maybe to handle optional symmetric triple" []
    (lambda "mst" $
      primitive _maybes_maybe @@
      (Core.termMaybe nothing) @@
      (lambda "st" $ Core.termMaybe $
        just $ project (ref testTypeTripleNameDef) (name "second") @@ (unwrap (ref testTypeSymmetricTripleNameDef) @@ var "st")) @@
      var "mst")
    (tylams ["t0", "t1"] $
      lambdaTyped "mst" (T.optional $ T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"]) $
      tyapps (primitive _maybes_maybe)
        [T.optional (T.var "t1"),
         T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"]] @@
      tyapp (Core.termMaybe nothing) (T.var "t1") @@
      (lambdaTyped "st" (T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"]) $
        Core.termMaybe $ just $
        (tyapps (project (ref testTypeTripleNameDef) (name "second")) [T.var "t0", T.var "t1", T.var "t0"] @@
         (tyapps (unwrap (ref testTypeSymmetricTripleNameDef)) [T.var "t0", T.var "t1"] @@ var "st"))) @@
      var "mst")
    (T.forAlls ["t0", "t1"] $
      T.function
        (T.optional $ T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"])
        (T.optional $ T.var "t1"))]

chainedUnwrappingTests :: TTerm TestGroup
chainedUnwrappingTests = subgroup "Chained unwrapping" [
  checkTest "unwrap then process" []
    (lambda "wrapped" $
      primitive _strings_cat2 @@ (unwrap (ref testTypeStringAliasNameDef) @@ var "wrapped") @@ string " suffix")
    (lambdaTyped "wrapped" (Core.typeVariable $ ref testTypeStringAliasNameDef) $
      primitive _strings_cat2 @@ (unwrap (ref testTypeStringAliasNameDef) @@ var "wrapped") @@ string " suffix")
    (T.function (Core.typeVariable $ ref testTypeStringAliasNameDef) T.string),
  checkTest "unwrap polymorphic then map" []
    (lambda "wrappedList" $
      primitive _lists_map @@ (primitive _math_add @@ int32 1) @@ (unwrap (ref testTypePolymorphicWrapperNameDef) @@ var "wrappedList"))
    (lambdaTyped "wrappedList" (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) T.int32) $
      (tyapps (primitive _lists_map) [T.int32, T.int32]) @@ (primitive _math_add @@ int32 1) @@ (tyapp (unwrap (ref testTypePolymorphicWrapperNameDef)) T.int32 @@ var "wrappedList"))
    (T.function (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) T.int32) (T.list T.int32))]

multipleUnwrapOperationsTests :: TTerm TestGroup
multipleUnwrapOperationsTests = subgroup "Multiple unwrap operations" [
  checkTest "unwrap different types" []
    (lambda "stringWrapped" $
      lambda "listWrapped" $
        tuple [
          unwrap (ref testTypeStringAliasNameDef) @@ var "stringWrapped",
          unwrap (ref testTypePolymorphicWrapperNameDef) @@ var "listWrapped"])
    (tylam "t0" $ lambdaTyped "stringWrapped" (Core.typeVariable $ ref testTypeStringAliasNameDef) $
      lambdaTyped "listWrapped" (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) (T.var "t0")) $
        tuple [
          unwrap (ref testTypeStringAliasNameDef) @@ var "stringWrapped",
          tyapp (unwrap (ref testTypePolymorphicWrapperNameDef)) (T.var "t0") @@ var "listWrapped"])
    (T.forAll "t0" $ T.function (Core.typeVariable $ ref testTypeStringAliasNameDef)
      (T.function (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) (T.var "t0"))
        (T.product [T.string, T.list $ T.var "t0"])))]

------ Fail on untyped (pre-inference) terms ------

failOnUntypedTests :: TTerm TestGroup
failOnUntypedTests = supergroup "Fail on untyped (pre-inference) terms" [
  untypedLambdasTests]

untypedLambdasTests :: TTerm TestGroup
untypedLambdasTests = subgroup "Untyped lambdas" [
  -- Note: The original HSpec test for this section was a failure test (typeOfShouldFail)
  -- which tested that typeOf fails on untyped terms. The TTerm DSL format may need
  -- a different mechanism for representing failure tests.
  ]

------ Helper functions ------

-- Helper function to create a type checking test case
checkTest :: String -> [Tag] -> TTerm Term -> TTerm Term -> TTerm Type -> TTerm TestCaseWithMetadata
checkTest name tags input outputTerm outputType = testCaseWithMetadata (Phantoms.string name)
  (testCaseTypeChecking $ typeCheckingTestCase input outputTerm outputType) Phantoms.nothing (Phantoms.list $ tag . unTag <$> tags)

-- Helper for tests where the term doesn't change during type checking
noChange :: String -> TTerm Term -> TTerm Type -> TTerm TestCaseWithMetadata
noChange name term typ = checkTest name [] term term typ

-- Create a TestCase variant for type checking
testCaseTypeChecking :: TTerm TypeCheckingTestCase -> TTerm TestCase
testCaseTypeChecking = variant _TestCase _TestCase_typeChecking

-- Create a TypeCheckingTestCase record
typeCheckingTestCase :: TTerm Term -> TTerm Term -> TTerm Type -> TTerm TypeCheckingTestCase
typeCheckingTestCase input outputTerm outputType = Phantoms.record _TypeCheckingTestCase [
  Phantoms.field _TypeCheckingTestCase_input input,
  Phantoms.field _TypeCheckingTestCase_outputTerm outputTerm,
  Phantoms.field _TypeCheckingTestCase_outputType outputType]
