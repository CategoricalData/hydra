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
topLevelAnnotationsTests = subgroup "Top-level annotations" []

nestedAnnotationsTests :: TTerm TestGroup
nestedAnnotationsTests = subgroup "Nested annotations" []

annotationsInComplexContextsTests :: TTerm TestGroup
annotationsInComplexContextsTests = subgroup "Annotations in complex contexts" []

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
    (T.forAlls ["t0"] $ T.either T.int32 (T.var "t0")),
  checkTest "left string" []
    (left $ string "error")
    (tylam "t0" $ tyapps (left $ string "error") [T.string, T.var "t0"])
    (T.forAlls ["t0"] $ T.either T.string (T.var "t0")),
  checkTest "left boolean" []
    (left $ boolean False)
    (tylam "t0" $ tyapps (left $ boolean False) [T.boolean, T.var "t0"])
    (T.forAlls ["t0"] $ T.either T.boolean (T.var "t0"))]

rightValuesTests :: TTerm TestGroup
rightValuesTests = subgroup "Right values" [
  checkTest "right int" []
    (right $ int32 42)
    (tylam "t0" $ tyapps (right $ int32 42) [T.var "t0", T.int32])
    (T.forAlls ["t0"] $ T.either (T.var "t0") T.int32),
  checkTest "right string" []
    (right $ string "success")
    (tylam "t0" $ tyapps (right $ string "success") [T.var "t0", T.string])
    (T.forAlls ["t0"] $ T.either (T.var "t0") T.string),
  checkTest "right boolean" []
    (right $ boolean True)
    (tylam "t0" $ tyapps (right $ boolean True) [T.var "t0", T.boolean])
    (T.forAlls ["t0"] $ T.either (T.var "t0") T.boolean)]

polymorphicEithersTests :: TTerm TestGroup
polymorphicEithersTests = subgroup "Polymorphic eithers" [
  checkTest "left from lambda" []
    (lambda "x" $ left $ var "x")
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ tyapps (left $ var "x") [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.either (T.var "t0") (T.var "t1"))),
  checkTest "right from lambda" []
    (lambda "x" $ right $ var "x")
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ tyapps (right $ var "x") [T.var "t1", T.var "t0"])
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.either (T.var "t1") (T.var "t0"))),
  checkTest "either from two lambdas" []
    (lambda "flag" $ lambda "x" $
      primitive _logic_ifElse @@ var "flag" @@
        (left $ var "x") @@
        (right $ var "x"))
    (tylam "t0" $ lambdaTyped "flag" T.boolean $ lambdaTyped "x" (T.var "t0") $
      tyapp (primitive _logic_ifElse) (T.either (T.var "t0") (T.var "t0")) @@ var "flag" @@
        tyapps (left $ var "x") [T.var "t0", T.var "t0"] @@
        tyapps (right $ var "x") [T.var "t0", T.var "t0"])
    (T.forAlls ["t0"] $ T.function T.boolean (T.function (T.var "t0") (T.either (T.var "t0") (T.var "t0"))))]

eithersInComplexContextsTests :: TTerm TestGroup
eithersInComplexContextsTests = subgroup "Eithers in complex contexts" [
  checkTest "either in tuple" []
    (tuple [left $ string "error", int32 100])
    (tylam "t0" $ tuple [tyapps (left $ string "error") [T.string, T.var "t0"], int32 100])
    (T.forAlls ["t0"] $ T.product [T.either T.string (T.var "t0"), T.int32]),
  checkTest "either in list" []
    (list [left $ string "error", right $ int32 42])
    (list [tyapps (left $ string "error") [T.string, T.int32], tyapps (right $ int32 42) [T.string, T.int32]])
    (T.list $ T.either T.string T.int32),
  checkTest "either in let binding" []
    (lets ["result">: right $ int32 42] $
      var "result")
    (tylam "t0" $ letsTyped [("result", tylam "t1" $ tyapps (right $ int32 42) [T.var "t1", T.int32], T.poly ["t1"] $ T.either (T.var "t1") T.int32)] $
      tyapp (var "result") (T.var "t0"))
    (T.forAlls ["t0"] $ T.either (T.var "t0") T.int32)]

nestedEithersTests :: TTerm TestGroup
nestedEithersTests = subgroup "Nested eithers" [
  checkTest "either of either (left left)" []
    (left $ left $ int32 1)
    (tylams ["t0", "t1"] $ tyapps (left $ tyapps (left $ int32 1) [T.int32, T.var "t0"]) [T.either T.int32 (T.var "t0"), T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.either (T.either T.int32 (T.var "t0")) (T.var "t1")),
  checkTest "either of either (left right)" []
    (left $ right $ string "nested")
    (tylams ["t0", "t1"] $ tyapps (left $ tyapps (right $ string "nested") [T.var "t0", T.string]) [T.either (T.var "t0") T.string, T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.either (T.either (T.var "t0") T.string) (T.var "t1")),
  checkTest "either of either (right)" []
    (right $ boolean True)
    (tylam "t0" $ tyapps (right $ boolean True) [T.var "t0", T.boolean])
    (T.forAlls ["t0"] $ T.either (T.var "t0") T.boolean),
  checkTest "either of list" []
    (left $ list [int32 1, int32 2])
    (tylam "t0" $ tyapps (left $ list [int32 1, int32 2]) [T.list T.int32, T.var "t0"])
    (T.forAlls ["t0"] $ T.either (T.list T.int32) (T.var "t0")),
  checkTest "list of eithers" []
    (list [left $ string "a", right $ int32 1, left $ string "b"])
    (list [tyapps (left $ string "a") [T.string, T.int32], tyapps (right $ int32 1) [T.string, T.int32], tyapps (left $ string "b") [T.string, T.int32]])
    (T.list $ T.either T.string T.int32)]

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
    (T.forAlls ["t0"] $ T.either (Core.typeVariable $ ref testTypePersonNameDef) (T.var "t0")),
  checkTest "either with record on right" []
    (right $ record (ref testTypePersonNameDef) [
      "firstName">: string "Bob",
      "lastName">: string "Jones",
      "age">: int32 25])
    (tylam "t0" $ tyapps (right $ record (ref testTypePersonNameDef) [
      "firstName">: string "Bob",
      "lastName">: string "Jones",
      "age">: int32 25]) [T.var "t0", Core.typeVariable $ ref testTypePersonNameDef])
    (T.forAlls ["t0"] $ T.either (T.var "t0") (Core.typeVariable $ ref testTypePersonNameDef)),
  checkTest "either with tuple" []
    (left $ tuple [string "error", int32 404])
    (tylam "t0" $ tyapps (left $ tuple [string "error", int32 404]) [T.product [T.string, T.int32], T.var "t0"])
    (T.forAlls ["t0"] $ T.either (T.product [T.string, T.int32]) (T.var "t0"))]

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
nestedLetTermsTests = subgroup "Nested let terms" []

letWithComplexExpressionsTests :: TTerm TestGroup
letWithComplexExpressionsTests = subgroup "Let with complex expressions" []

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
    (optional $ just $ TTerms.map $ Phantoms.map $ M.singleton (string "key") (int32 42))
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
nullaryPrimitivesTests = subgroup "Nullary primitives" []

unaryPrimitivesTests :: TTerm TestGroup
unaryPrimitivesTests = subgroup "Unary primitives" []

binaryPrimitivesTests :: TTerm TestGroup
binaryPrimitivesTests = subgroup "Binary primitives" []

ternaryPrimitivesTests :: TTerm TestGroup
ternaryPrimitivesTests = subgroup "Ternary primitives" []

monomorphicVsPolymorphicTests :: TTerm TestGroup
monomorphicVsPolymorphicTests = subgroup "Monomorphic vs polymorphic" []

higherOrderPrimitivesTests :: TTerm TestGroup
higherOrderPrimitivesTests = subgroup "Higher-order primitives" []

primitivesInComplexContextsTests :: TTerm TestGroup
primitivesInComplexContextsTests = subgroup "Primitives in complex contexts" []

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
simpleTupleProjectionsTests = subgroup "Simple tuple projections" []

polymorphicTupleProjectionsTests :: TTerm TestGroup
polymorphicTupleProjectionsTests = subgroup "Polymorphic tuple projections" []

projectionsWithVariablesTests :: TTerm TestGroup
projectionsWithVariablesTests = subgroup "Projections with variables" []

projectionsInComplexContextsTests :: TTerm TestGroup
projectionsInComplexContextsTests = subgroup "Projections in complex contexts" []

projectionsWithMixedTypesTests :: TTerm TestGroup
projectionsWithMixedTypesTests = subgroup "Projections with mixed types" []

projectionsWithPrimitiveFunctionsTests :: TTerm TestGroup
projectionsWithPrimitiveFunctionsTests = subgroup "Projections with primitive functions" []

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
        "second" >: variantPhantom (Name "PersonOrSomething") (Name "other")
          (TTerms.map $ Phantoms.map $ M.singleton (var "k") (var "v")),
        "third" >: int32 999])
    (tylams ["t0", "t1"] $
      lambdaTyped "k" (T.var "t0") $
      lambdaTyped "v" (T.var "t1") $
      tyapps (record (name "Triple") [
        "first" >: string "prefix",
        "second" >: tyapp (variantPhantom (Name "PersonOrSomething") (Name "other")
          (TTerms.map $ Phantoms.map $ M.singleton (var "k") (var "v"))) (T.map (T.var "t0") (T.var "t1")),
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
simpleRecordProjectionsTests = subgroup "Simple record projections" []

recordProjectionsAppliedToRecordsTests :: TTerm TestGroup
recordProjectionsAppliedToRecordsTests = subgroup "Record projections applied to records" []

polymorphicRecordProjectionsTests :: TTerm TestGroup
polymorphicRecordProjectionsTests = subgroup "Polymorphic record projections" []

polymorphicRecordProjectionsAppliedTests :: TTerm TestGroup
polymorphicRecordProjectionsAppliedTests = subgroup "Polymorphic record projections applied" []

recordProjectionsWithVariablesTests :: TTerm TestGroup
recordProjectionsWithVariablesTests = subgroup "Record projections with variables" []

recordProjectionsInComplexContextsTests :: TTerm TestGroup
recordProjectionsInComplexContextsTests = subgroup "Record projections in complex contexts" []

multiParameterPolymorphicProjectionsTests :: TTerm TestGroup
multiParameterPolymorphicProjectionsTests = subgroup "Multi-parameter polymorphic projections" []

higherOrderRecordProjectionsTests :: TTerm TestGroup
higherOrderRecordProjectionsTests = subgroup "Higher-order record projections" []

recursiveRecordProjectionsTests :: TTerm TestGroup
recursiveRecordProjectionsTests = subgroup "Recursive record projections" []

recordProjectionsWithMutualRecursionTests :: TTerm TestGroup
recordProjectionsWithMutualRecursionTests = subgroup "Record projections with mutual recursion" []

------ Sets ------

setsTests :: TTerm TestGroup
setsTests = supergroup "Sets" [
  monomorphicSetsTests,
  polymorphicSetsTests,
  setsInComplexContextsTests,
  nestedSetsTests,
  setsWithComplexTypesTests]

monomorphicSetsTests :: TTerm TestGroup
monomorphicSetsTests = subgroup "Monomorphic sets" []

polymorphicSetsTests :: TTerm TestGroup
polymorphicSetsTests = subgroup "Polymorphic sets" []

setsInComplexContextsTests :: TTerm TestGroup
setsInComplexContextsTests = subgroup "Sets in complex contexts" []

nestedSetsTests :: TTerm TestGroup
nestedSetsTests = subgroup "Nested sets" []

setsWithComplexTypesTests :: TTerm TestGroup
setsWithComplexTypesTests = subgroup "Sets with complex types" []

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
simpleUnionInjectionsTests = subgroup "Simple union injections" []

unionInjectionsWithDataTests :: TTerm TestGroup
unionInjectionsWithDataTests = subgroup "Union injections with data" []

polymorphicUnionInjectionsTests :: TTerm TestGroup
polymorphicUnionInjectionsTests = subgroup "Polymorphic union injections" []

polymorphicRecursiveUnionInjectionsTests :: TTerm TestGroup
polymorphicRecursiveUnionInjectionsTests = subgroup "Polymorphic recursive union injections" []

polymorphicUnionsFromLambdaTests :: TTerm TestGroup
polymorphicUnionsFromLambdaTests = subgroup "Polymorphic unions from lambda" []

unionsInComplexContextsTests :: TTerm TestGroup
unionsInComplexContextsTests = subgroup "Unions in complex contexts" []

multiParameterPolymorphicInjectionsTests :: TTerm TestGroup
multiParameterPolymorphicInjectionsTests = subgroup "Multi-parameter polymorphic injections" []

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
simpleUnitVariantEliminationsTests = subgroup "Simple unit variant eliminations" []

unionEliminationsWithDataTests :: TTerm TestGroup
unionEliminationsWithDataTests = subgroup "Union eliminations with data" []

polymorphicUnionEliminationsTests :: TTerm TestGroup
polymorphicUnionEliminationsTests = subgroup "Polymorphic union eliminations" []

unionEliminationsWithDefaultsTests :: TTerm TestGroup
unionEliminationsWithDefaultsTests = subgroup "Union eliminations with defaults" []

nestedUnionEliminationsTests :: TTerm TestGroup
nestedUnionEliminationsTests = subgroup "Nested union eliminations" []

unionEliminationsInComplexContextsTests :: TTerm TestGroup
unionEliminationsInComplexContextsTests = subgroup "Union eliminations in complex contexts" []

multiParameterPolymorphicCaseStatementsTests :: TTerm TestGroup
multiParameterPolymorphicCaseStatementsTests = subgroup "Multi-parameter polymorphic case statements" []

higherOrderUnionEliminationsTests :: TTerm TestGroup
higherOrderUnionEliminationsTests = subgroup "Higher-order union eliminations" []

recursiveUnionEliminationsTests :: TTerm TestGroup
recursiveUnionEliminationsTests = subgroup "Recursive union eliminations" []

------ Unit ------

unitTests :: TTerm TestGroup
unitTests = supergroup "Unit" [
  unitTermTests,
  unitTermInPolymorphicContextTests]

unitTermTests :: TTerm TestGroup
unitTermTests = subgroup "Unit term" []

unitTermInPolymorphicContextTests :: TTerm TestGroup
unitTermInPolymorphicContextTests = subgroup "Unit term in polymorphic context" []

------ Variables ------

variablesTests :: TTerm TestGroup
variablesTests = supergroup "Variables" [
  simpleVariableLookupTests,
  variableScopingTests,
  polymorphicVariablesTests,
  variablesInComplexContextsTests,
  recursiveVariablesTests]

simpleVariableLookupTests :: TTerm TestGroup
simpleVariableLookupTests = subgroup "Simple variable lookup" []

variableScopingTests :: TTerm TestGroup
variableScopingTests = subgroup "Variable scoping" []

polymorphicVariablesTests :: TTerm TestGroup
polymorphicVariablesTests = subgroup "Polymorphic variables" []

variablesInComplexContextsTests :: TTerm TestGroup
variablesInComplexContextsTests = subgroup "Variables in complex contexts" []

recursiveVariablesTests :: TTerm TestGroup
recursiveVariablesTests = subgroup "Recursive variables" []

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
monomorphicWrappedTermsTests = subgroup "Monomorphic wrapped terms" []

polymorphicWrappedTermsTests :: TTerm TestGroup
polymorphicWrappedTermsTests = subgroup "Polymorphic wrapped terms" []

wrappedTermsInComplexContextsTests :: TTerm TestGroup
wrappedTermsInComplexContextsTests = subgroup "Wrapped terms in complex contexts" []

nestedWrappedTermsTests :: TTerm TestGroup
nestedWrappedTermsTests = subgroup "Nested wrapped terms" []

multipleWrappingLevelsTests :: TTerm TestGroup
multipleWrappingLevelsTests = subgroup "Multiple wrapping levels" []

multiParameterPolymorphicWrappersTests :: TTerm TestGroup
multiParameterPolymorphicWrappersTests = subgroup "Multi-parameter polymorphic wrappers" []

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
monomorphicUnwrappingTests = subgroup "Monomorphic unwrapping" []

polymorphicUnwrappingTests :: TTerm TestGroup
polymorphicUnwrappingTests = subgroup "Polymorphic unwrapping" []

unwrapEliminationsInApplicationsTests :: TTerm TestGroup
unwrapEliminationsInApplicationsTests = subgroup "Unwrap eliminations in applications" []

unwrapInComplexContextsTests :: TTerm TestGroup
unwrapInComplexContextsTests = subgroup "Unwrap in complex contexts" []

multiParameterPolymorphicUnwrappersTests :: TTerm TestGroup
multiParameterPolymorphicUnwrappersTests = subgroup "Multi-parameter polymorphic unwrappers" []

chainedUnwrappingTests :: TTerm TestGroup
chainedUnwrappingTests = subgroup "Chained unwrapping" []

multipleUnwrapOperationsTests :: TTerm TestGroup
multipleUnwrapOperationsTests = subgroup "Multiple unwrap operations" []

------ Fail on untyped (pre-inference) terms ------

failOnUntypedTests :: TTerm TestGroup
failOnUntypedTests = supergroup "Fail on untyped (pre-inference) terms" [
  untypedLambdasTests]

untypedLambdasTests :: TTerm TestGroup
untypedLambdasTests = subgroup "Untyped lambdas" []

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
