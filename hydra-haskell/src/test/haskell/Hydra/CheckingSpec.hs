-- TODO: add these type checking test cases to the generated test suite

{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.CheckingSpec.spec
-}

module Hydra.CheckingSpec where

import Hydra.Kernel
import Hydra.TestUtils
import Hydra.Staging.TestGraph
import Hydra.Tools.Monads
import qualified Hydra.Lib.Flows as Flows
import           Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


spec :: H.Spec
spec = do
  checkTypeOf
  checkFailTypeOfOnUntypeApplicationTerms

----------------------------------------

checkFailTypeOfOnUntypeApplicationTerms :: H.SpecWith ()
checkFailTypeOfOnUntypeApplicationTerms = H.describe "Fail on untyped (pre-inference) terms" $ do
  H.describe "Untyped lambdas" $ do
    withDefaults typeOfShouldFail "untyped var in record"
      (lambda "x" (record testTypeLatLonName [
        field "lat" (float32 19.5429),
        field "lon" (var "x")]))

----------------------------------------

checkTypeOf :: H.SpecWith ()
checkTypeOf = H.describe "typeOf" $ do
  checkTypeOfAnnotatedTerms
  checkTypeOfApplications
  checkTypeOfFlows
  checkTypeOfFunctions
  checkTypeOfLetTerms
  checkTypeOfLists
  checkTypeOfLiterals
  checkTypeOfMaps
  checkTypeOfOptionals
  checkTypeOfProducts
  checkTypeOfRecords
  checkTypeOfSets
  checkTypeOfSums
  checkTypeOfUnions
  checkTypeOfUnit
  checkTypeOfVariables
  checkTypeOfWrappedTerms

checkTypeOfAnnotatedTerms :: H.SpecWith ()
checkTypeOfAnnotatedTerms = H.describe "Annotated terms" $ do
  H.describe "Top-level annotations" $ do
    expectSameTermWithType "annotated literal"
      (annotated (int32 42) M.empty)
      Types.int32
    expectSameTermWithType "annotated list"
      (annotated (list [string "a", string "b"]) M.empty)
      (Types.list Types.string)
    expectSameTermWithType "annotated record"
      (annotated (record testTypePersonName [
        field "firstName" (string "John"),
        field "lastName" (string "Doe"),
        field "age" (int32 25)]) M.empty)
      (Types.var "Person")
    expectTermWithType "annotated lambda"
      (annotated (lambda "x" $ var "x") M.empty)
      (annotated (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ var "x") M.empty)
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))

  H.describe "Nested annotations" $ do
    expectSameTermWithType "annotation within annotation"
      (annotated (annotated (int32 100) M.empty) M.empty)
      Types.int32
    expectSameTermWithType "annotated terms in tuple"
      (tuple [annotated (int32 1) M.empty,
              annotated (string "hello") M.empty])
      (Types.product [Types.int32, Types.string])
    expectTermWithType "annotated term in function application"
        (annotated (lambda "x" $ var "x") M.empty @@ annotated (int32 42) M.empty)
        (annotated (lambdaTyped "x" Types.int32 $ var "x") M.empty @@ annotated (int32 42) M.empty)
        Types.int32

  H.describe "Annotations in complex contexts" $ do
    expectTermWithType "annotated let binding"
      (lets ["x">: annotated (int32 5) M.empty,
             "y">: annotated (string "world") M.empty] $
            annotated (tuple [var "x", var "y"]) M.empty)
      (letsTyped [("x", annotated (int32 5) M.empty, Types.mono Types.int32),
                  ("y", annotated (string "world") M.empty, Types.mono Types.string)] $
        annotated (tuple [var "x", var "y"]) M.empty)
      (Types.product [Types.int32, Types.string])
    expectSameTermWithType "annotated record fields"
      (record testTypePersonName [
        field "firstName" (annotated (string "Alice") M.empty),
        field "lastName" (annotated (string "Smith") M.empty),
        field "age" (annotated (int32 30) M.empty)])
      (Types.var "Person")
    expectTermWithType "annotated function in application"
      (lets ["add">: annotated (primitive _math_add) M.empty] $
            var "add" @@ annotated (int32 10) M.empty @@ annotated (int32 20) M.empty)
      (letsTyped [("add", annotated (primitive _math_add) M.empty, Types.mono $ Types.function Types.int32 (Types.function Types.int32 Types.int32))] $
        var "add" @@ annotated (int32 10) M.empty @@ annotated (int32 20) M.empty)
      Types.int32

checkTypeOfApplications :: H.SpecWith ()
checkTypeOfApplications = H.describe "Applications" $ do
  H.describe "Simple function applications" $ do
    expectTermWithType "identity application"
      (lambda "x" (var "x") @@ int32 42)
      (lambdaTyped "x" Types.int32 (var "x") @@ int32 42)
      Types.int32
    expectSameTermWithType "primitive application"
      (primitive _math_add @@ int32 10 @@ int32 20)
      Types.int32
    expectSameTermWithType "string concatenation"
      (primitive _strings_cat2 @@ string "hello" @@ string "world")
      Types.string

  H.describe "Partial applications" $ do
    expectSameTermWithType "partially applied add"
      (primitive _math_add @@ int32 5)
      (Types.function Types.int32 Types.int32)
    expectSameTermWithType "partially applied string cat"
      (primitive _strings_cat2 @@ string "prefix")
      (Types.function Types.string Types.string)

  H.describe "Higher-order applications" $ do
    expectTermWithType "apply function to function"
      (lets ["apply">: lambda "f" $ lambda "x" $ var "f" @@ var "x",
             "double">: lambda "n" $ primitive _math_mul @@ var "n" @@ int32 2] $
            var "apply" @@ var "double" @@ int32 5)
      (letsTyped [
        ("apply", tylams ["t0", "t1"] $ lambdaTyped "f" (Types.function (Types.var "t0") (Types.var "t1")) $ lambdaTyped "x" (Types.var "t0") $ var "f" @@ var "x",
          Types.poly ["t0", "t1"] $ Types.function (Types.function (Types.var "t0") (Types.var "t1")) (Types.function (Types.var "t0") (Types.var "t1"))),
        ("double", lambdaTyped "n" Types.int32 $ primitive _math_mul @@ var "n" @@ int32 2,
          Types.mono $ Types.function Types.int32 Types.int32)] $
        tyapps (var "apply") [Types.int32, Types.int32] @@ var "double" @@ int32 5)
      Types.int32
    expectTermWithType "function composition"
      (lets ["compose">: lambda "f" $ lambda "g" $ lambda "x" $ var "f" @@ (var "g" @@ var "x"),
             "add1">: lambda "n" $ primitive _math_add @@ var "n" @@ int32 1,
             "mul2">: lambda "n" $ primitive _math_mul @@ var "n" @@ int32 2] $
            var "compose" @@ var "add1" @@ var "mul2" @@ int32 3)
      (letsTyped [
        ("compose", tylams ["t0", "t1", "t2"] $ lambdaTyped "f" (Types.function (Types.var "t0") (Types.var "t1")) $ lambdaTyped "g" (Types.function (Types.var "t2") (Types.var "t0")) $ lambdaTyped "x" (Types.var "t2") $ var "f" @@ (var "g" @@ var "x"),
          Types.poly ["t0", "t1", "t2"] $ Types.function (Types.function (Types.var "t0") (Types.var "t1")) (Types.function (Types.function (Types.var "t2") (Types.var "t0")) (Types.function (Types.var "t2") (Types.var "t1")))),
        ("add1", lambdaTyped "n" Types.int32 $ primitive _math_add @@ var "n" @@ int32 1,
          Types.mono $ Types.function Types.int32 Types.int32),
        ("mul2", lambdaTyped "n" Types.int32 $ primitive _math_mul @@ var "n" @@ int32 2,
          Types.mono $ Types.function Types.int32 Types.int32)] $
        tyapps (var "compose") [Types.int32, Types.int32, Types.int32] @@ var "add1" @@ var "mul2" @@ int32 3)
      Types.int32

  H.describe "Polymorphic applications" $ do
    expectTermWithType "polymorphic identity"
      (lets ["id">: lambda "x" $ var "x"] $
            tuple [var "id" @@ int32 42, var "id" @@ string "hello"])
      (letsTyped [
        ("id", tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ var "x",
          Types.poly ["t0"] $ Types.function (Types.var "t0") (Types.var "t0"))] $
        tuple [tyapp (var "id") Types.int32 @@ int32 42, tyapp (var "id") Types.string @@ string "hello"])
      (Types.product [Types.int32, Types.string])
    expectTermWithType "polymorphic const"
      (lets ["const">: lambdas ["x", "y"] $ var "x"] $
             var "const" @@ string "keep" @@ int32 999)
      (letsTyped [
        ("const", tylams ["t0", "t1"] $ lambdaTyped "x" (Types.var "t0") $ lambdaTyped "y" (Types.var "t1") $ var "x",
          Types.poly ["t0", "t1"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.var "t0")))] $
        tyapps (var "const") [Types.string, Types.int32] @@ string "keep" @@ int32 999)
      Types.string
    expectTermWithType "polymorphic flip"
      (lets ["flip">: lambda "f" $ lambda "x" $ lambda "y" $ var "f" @@ var "y" @@ var "x"] $
            var "flip" @@ primitive _strings_cat2 @@ string "world" @@ string "hello")
      (letsTyped [
        ("flip", tylams ["t0", "t1", "t2"] $ lambdaTyped "f" (Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.var "t2"))) $ lambdaTyped "x" (Types.var "t1") $ lambdaTyped "y" (Types.var "t0") $ var "f" @@ var "y" @@ var "x",
          Types.poly ["t0", "t1", "t2"] $ Types.function (Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.var "t2"))) (Types.function (Types.var "t1") (Types.function (Types.var "t0") (Types.var "t2"))))] $
        tyapps (var "flip") [Types.string, Types.string, Types.string] @@ primitive _strings_cat2 @@ string "world" @@ string "hello")
      Types.string

  H.describe "Applications in complex contexts" $ do
    expectSameTermWithType "application in tuple"
      (tuple [primitive _math_add @@ int32 1 @@ int32 2,
              primitive _strings_cat2 @@ string "a" @@ string "b"])
      (Types.product [Types.int32, Types.string])
    expectSameTermWithType "application in record"
      (record testTypePersonName [
        field "firstName" (primitive _strings_cat2 @@ string "John" @@ string "ny"),
        field "lastName" (string "Doe"),
        field "age" (primitive _math_add @@ int32 20 @@ int32 5)])
      (Types.var "Person")
    expectTermWithType "application in let binding"
      (lets ["result">: primitive _math_mul @@ int32 6 @@ int32 7] $
            var "result")
      (letsTyped [
        ("result", primitive _math_mul @@ int32 6 @@ int32 7,
          Types.mono Types.int32)] $
        var "result")
      Types.int32
    expectSameTermWithType "nested applications"
      (primitive _math_add @@ (primitive _math_mul @@ int32 3 @@ int32 4) @@ (primitive _math_add @@ int32 1 @@ int32 2))
      Types.int32

  H.describe "Applications with complex arguments" $ do
    expectTermWithType "application with record argument"
      (lets ["getName">: lambda "person" $ project testTypePersonName (Name "firstName") @@ var "person"] $
            var "getName" @@ record testTypePersonName [
              field "firstName" (string "Alice"),
              field "lastName" (string "Smith"),
              field "age" (int32 25)])
      (letsTyped [
        ("getName", lambdaTyped "person" (Types.var "Person") $ project testTypePersonName (Name "firstName") @@ var "person",
          Types.mono $ Types.function (Types.var "Person") Types.string)] $
        var "getName" @@ record testTypePersonName [
          field "firstName" (string "Alice"),
          field "lastName" (string "Smith"),
          field "age" (int32 25)])
      Types.string
    expectTermWithType "application with list argument"
      (lets ["head">: lambda "xs" $ primitive _lists_head @@ var "xs"] $
            var "head" @@ list [string "first", string "second"])
      (letsTyped [
        ("head", tylam "t0" $ lambdaTyped "xs" (Types.list (Types.var "t0")) $ tyapp (primitive _lists_head) (Types.var "t0") @@ var "xs",
          Types.poly ["t0"] $ Types.function (Types.list (Types.var "t0")) (Types.var "t0"))] $
        tyapp (var "head") Types.string @@ list [string "first", string "second"])
      Types.string

checkTypeOfEliminations :: H.SpecWith ()
checkTypeOfEliminations = H.describe "Eliminations" $ do
  checkTypeOfProductEliminations
  checkTypeOfRecordEliminations
  checkTypeOfUnionEliminations
  checkTypeOfWrapEliminations

-- Note: Flow is not part of Hydra Core, but it is essential to Hydra. See https://github.com/CategoricalData/hydra/issues/200.
checkTypeOfFlows :: H.SpecWith ()
checkTypeOfFlows = H.describe "Flows" $ do
  H.describe "Flows with failure across let bindings" $ do
    -- Note: this test case was used to reproduce an issue which had to do with missing type instantiation
    --       in the type checker; normalized type variables in sibling let bindings were coming into conflict
    --       during substitution.
    expectTermWithType "mutually referential failure functions with Flow monad"
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
           lambdaTyped "s" Types.string $
           lambdaTyped "b" Types.boolean $
           lambdaTyped "ignored" (Types.var "t3") $
           tyapp (primitive _logic_ifElse) (Types.applys (Types.var "hydra.compute.Flow") [Types.var "t4", Types.var "t5"]) @@ var "b" @@
             (tyapps (var "unexpected") [Types.var "t4", Types.var "t5"] @@ string "oops") @@
             (tyapps (var "unexpected") [Types.var "t4", Types.var "t5"] @@ var "s"),
           Types.poly ["t3", "t4", "t5"] $
             Types.function Types.string $
             Types.function Types.boolean $
             Types.function (Types.var "t3") $
             Types.applys (Types.var "hydra.compute.Flow") [Types.var "t4", Types.var "t5"]),
          ("unexpected",
           tylams ["t3", "t4"] $
           lambdaTyped "s" Types.string $
             tyapps (primitive _flows_fail) [Types.var "t3", Types.var "t4"] @@ var "s",
           Types.poly ["t3", "t4"] $
             Types.function Types.string $
             Types.applys (Types.var "hydra.compute.Flow") [Types.var "t3", Types.var "t4"])] $
        tyapps (var "conditionalUnexpected") [Types.var "t0", Types.var "t1", Types.var "t2"])
      (Types.forAlls ["t0", "t1", "t2"] $
        Types.function Types.string $
        Types.function Types.boolean $
        Types.function (Types.var "t0") $
        Types.applys (Types.var "hydra.compute.Flow") [Types.var "t1", Types.var "t2"])

checkTypeOfFunctions :: H.SpecWith ()
checkTypeOfFunctions = H.describe "Functions" $ do
  checkTypeOfEliminations
  checkTypeOfLambdas
  checkTypeOfPrimitives

checkTypeOfLambdas :: H.SpecWith ()
checkTypeOfLambdas = H.describe "Lambdas" $ do
  H.describe "Simple lambdas" $ do
    expectTermWithType "identity function"
      (lambda "x" $ var "x")
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ var "x")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))
    expectTermWithType "constant function"
      (lambda "x" $ int32 42)
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ int32 42)
      (Types.forAll "t0" $ Types.function (Types.var "t0") Types.int32)

  H.describe "Multi-parameter lambdas" $ do
    expectTermWithType "two parameters"
      (lambda "x" $ lambda "y" $ var "x")
      (tylams ["t0", "t1"] $ lambdaTyped "x" (Types.var "t0") $ lambdaTyped "y" (Types.var "t1") $ var "x")
      (Types.forAlls ["t0", "t1"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.var "t0")))
    expectTermWithType "three parameters"
      (lambda "x" $ lambda "y" $ lambda "z" $ var "y")
      (tylams ["t0", "t1", "t2"] $ lambdaTyped "x" (Types.var "t0") $ lambdaTyped "y" (Types.var "t1") $ lambdaTyped "z" (Types.var "t2") $ var "y")
      (Types.forAlls ["t0", "t1", "t2"] $ Types.function
        (Types.var "t0")
        (Types.function (Types.var "t1") (Types.function (Types.var "t2") (Types.var "t1"))))
    expectTermWithType "parameter reuse"
      (lambda "x" $ lambda "y" $ tuple [var "x", var "x", var "y"])
      (tylams ["t0", "t1"] $ lambdaTyped "x" (Types.var "t0") $ lambdaTyped "y" (Types.var "t1") $ tuple [var "x", var "x", var "y"])
      (Types.forAlls ["t0", "t1"] $ Types.function
        (Types.var "t0") (Types.function (Types.var "t1")
        (Types.product [Types.var "t0", Types.var "t0", Types.var "t1"])))

  H.describe "Lambdas with operations" $ do
    expectTermWithType "lambda with primitive"
      (lambda "x" $ primitive _math_add @@ var "x" @@ int32 1)
      (lambdaTyped "x" Types.int32 $ primitive _math_add @@ var "x" @@ int32 1)
      (Types.function Types.int32 Types.int32)
    expectTermWithType "lambda with application"
      (lambda "f" $ lambda "x" $ var "f" @@ var "x")
      (tylams ["t0", "t1"] $ lambdaTyped "f" (Types.function (Types.var "t0") (Types.var "t1")) $ lambdaTyped "x" (Types.var "t0") $ var "f" @@ var "x")
      (Types.forAlls ["t0", "t1"] $ Types.function (Types.function (Types.var "t0") (Types.var "t1")) (Types.function (Types.var "t0") (Types.var "t1")))
    expectTermWithType "lambda with construction"
      (lambda "x" $ lambda "y" $ tuple [var "x", var "y"])
      (tylams ["t0", "t1"] $ lambdaTyped "x" (Types.var "t0") $ lambdaTyped "y" (Types.var "t1") $ tuple [var "x", var "y"])
      (Types.forAlls ["t0", "t1"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.product [Types.var "t0", Types.var "t1"])))

  H.describe "Nested lambdas" $ do
    expectTermWithType "lambda returning lambda"
      (lambda "x" $ lambda "y" $ lambda "z" $ var "x")
      (tylams ["t0", "t1", "t2"] $ lambdaTyped "x" (Types.var "t0") $ lambdaTyped "y" (Types.var "t1") $ lambdaTyped "z" (Types.var "t2") $ var "x")
      (Types.forAlls ["t0", "t1", "t2"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.function (Types.var "t2") (Types.var "t0"))))
    expectTermWithType "lambda with let binding"
      (lambda "x" $ lets ["y">: var "x"] $ var "y")
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ letsTyped [("y", var "x", Types.mono (Types.var "t0"))] $ var "y")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))
    expectTermWithType "lambda with inner lambda"
      (lambda "outer" $ lets ["inner">: lambda "x" $ var "x"] $ var "inner" @@ var "outer")
      (tylam "t0" $ lambdaTyped "outer" (Types.var "t0") $ letsTyped [("inner", tylam "t1" $ lambdaTyped "x" (Types.var "t1") $ var "x", Types.poly ["t1"] $ Types.function (Types.var "t1") (Types.var "t1"))] $ tyapp (var "inner") (Types.var "t0") @@ var "outer")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))

  H.describe "Lambdas in complex contexts" $ do
    expectTermWithType "lambda in tuple"
      (tuple [lambda "x" $ var "x", int32 42])
      (tylam "t0" $ tuple [lambdaTyped "x" (Types.var "t0") $ var "x", int32 42])
      (Types.forAll "t0" $ Types.product [Types.function (Types.var "t0") (Types.var "t0"), Types.int32])
    expectTermWithType "lambda in list"
      (list [lambda "x" $ primitive _math_add @@ var "x" @@ int32 1,
             lambda "y" $ primitive _math_mul @@ var "y" @@ int32 2])
      (list [lambdaTyped "x" Types.int32 $ primitive _math_add @@ var "x" @@ int32 1,
             lambdaTyped "y" Types.int32 $ primitive _math_mul @@ var "y" @@ int32 2])
      (Types.list $ Types.function Types.int32 Types.int32)
    expectTermWithType "lambda in record"
      (lambda "name" $ record testTypePersonName [
        field "firstName" (var "name"),
        field "lastName" (string "Doe"),
        field "age" (int32 30)])
      (lambdaTyped "name" Types.string $ record testTypePersonName [
        field "firstName" (var "name"),
        field "lastName" (string "Doe"),
        field "age" (int32 30)])
      (Types.function Types.string (Types.var "Person"))

  H.describe "Higher-order lambdas" $ do
    expectTermWithType "function composition"
      (lambda "f" $ lambda "g" $ lambda "x" $ var "f" @@ (var "g" @@ var "x"))
      (tylams ["t0", "t1", "t2"] $ lambdaTyped "f" (Types.function (Types.var "t0") (Types.var "t1")) $ lambdaTyped "g" (Types.function (Types.var "t2") (Types.var "t0")) $ lambdaTyped "x" (Types.var "t2") $ var "f" @@ (var "g" @@ var "x"))
      (Types.forAlls ["t0", "t1", "t2"] $ Types.function
        (Types.function (Types.var "t0") (Types.var "t1"))
        (Types.function
          (Types.function (Types.var "t2") (Types.var "t0"))
          (Types.function (Types.var "t2") (Types.var "t1"))))
    expectTermWithType "function application"
      (lambda "f" $ lambda "x" $ var "f" @@ var "x")
      (tylams ["t0", "t1"] $ lambdaTyped "f" (Types.function (Types.var "t0") (Types.var "t1")) $ lambdaTyped "x" (Types.var "t0") $ var "f" @@ var "x")
      (Types.forAlls ["t0", "t1"] $ Types.function (Types.function (Types.var "t0") (Types.var "t1")) (Types.function (Types.var "t0") (Types.var "t1")))
    expectTermWithType "curried function"
      (lambda "x" $ lambda "y" $ lambda "z" $ primitive _logic_ifElse @@ var "x" @@ var "y" @@ var "z")
      (tylam "t0" $ lambdaTyped "x" Types.boolean $ lambdaTyped "y" (Types.var "t0") $ lambdaTyped "z" (Types.var "t0") $ tyapp (primitive _logic_ifElse) (Types.var "t0") @@ var "x" @@ var "y" @@ var "z")
      (Types.forAll "t0" $ Types.function Types.boolean (Types.function (Types.var "t0") (Types.function (Types.var "t0") (Types.var "t0"))))

checkTypeOfLetTerms :: H.SpecWith ()
checkTypeOfLetTerms = H.describe "Let terms" $ do
  H.describe "Simple let bindings" $ do
    expectTermWithType "single binding"
      (lets ["x">: int32 42] $
            var "x")
      (letsTyped [("x", int32 42, Types.mono Types.int32)] $
        var "x")
      Types.int32
    expectTermWithType "multiple bindings"
      (lets ["x">: int32 42,
             "y">: string "hello"] $
            tuple [var "x", var "y"])
      (letsTyped [("x", int32 42, Types.mono Types.int32),
                  ("y", string "hello", Types.mono Types.string)] $
        tuple [var "x", var "y"])
      (Types.product [Types.int32, Types.string])

  H.describe "Let terms with shadowing" $ do
    expectTermWithType "lambda parameter shadowing let binding"
      (lets ["x">: int32 42] $
        lambda "x" $ var "x")
      (tylam "t0" $
        letsTyped [("x", int32 42, Types.mono Types.int32)] $
          lambdaTyped "x" (Types.var "t0") $ var "x")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))
    expectTermWithType "nested lambda shadowing"
      (lambda "x" $
        lets ["y">: var "x"] $
          lambda "x" $
            tuple [var "x", var "y"])
      (tylams ["t0", "t1"] $
        lambdaTyped "x" (Types.var "t0") $
          letsTyped [("y", var "x", Types.mono $ Types.var "t0")] $
            lambdaTyped "x" (Types.var "t1") $
              tuple [var "x", var "y"])
      (Types.forAlls ["t0", "t1"] $
        Types.function (Types.var "t0") $
          Types.function (Types.var "t1") $
            Types.product [Types.var "t1", Types.var "t0"])
    expectTermWithType "multiple levels of let shadowing"
      (lets ["x">: int32 1] $
        lets ["x">: string "second"] $
          lets ["x">: boolean True] $
            var "x")
      (letsTyped [("x", int32 1, Types.mono Types.int32)] $
        letsTyped [("x", string "second", Types.mono Types.string)] $
          letsTyped [("x", boolean True, Types.mono Types.boolean)] $
            var "x")
      Types.boolean
    expectTermWithType "let shadowing with lambda and reference to outer binding"
      (lets ["x">: int32 10, "y">: int32 20] $
        lambda "x" $
          lets ["z">: var "y"] $
            tuple [var "x", var "z"])
      (tylam "t0" $
        letsTyped [("x", int32 10, Types.mono Types.int32),
                   ("y", int32 20, Types.mono Types.int32)] $
          lambdaTyped "x" (Types.var "t0") $
            letsTyped [("z", var "y", Types.mono Types.int32)] $
              tuple [var "x", var "z"])
      (Types.forAll "t0" $
        Types.function (Types.var "t0") $
          Types.product [Types.var "t0", Types.int32])

  H.describe "Recursive bindings" $ do
    expectTermWithType "simple arithmetic recursion"
      (lets ["double">: lambda "n" $ primitive _math_add @@ var "n" @@ var "n"] $
            var "double" @@ int32 5)
      (letsTyped [("double", lambdaTyped "n" Types.int32 $ primitive _math_add @@ var "n" @@ var "n",
                   Types.mono $ Types.function Types.int32 Types.int32)] $
        var "double" @@ int32 5)
      Types.int32

  H.describe "Mutual recursion" $ do
    expectTermWithType "mutually recursive data"
      (lets ["listA">: record testTypeBuddyListAName [
               field "head" (int32 1),
               field "tail" (just $ var "listB")],
             "listB">: record testTypeBuddyListBName [
               field "head" (int32 2),
               field "tail" (nothing)]] $
            var "listA")
      (letsTyped [("listA", tyapp (record testTypeBuddyListAName [
                     field "head" (int32 1),
                     field "tail" (just $ var "listB")]) Types.int32,
                   Types.mono $ Types.apply (Types.var "BuddyListA") Types.int32),
                  ("listB", tyapp (record testTypeBuddyListBName [
                     field "head" (int32 2),
                     field "tail" (tyapp nothing (Types.apply (Types.var "BuddyListA") Types.int32))]) Types.int32,
                   Types.mono $ Types.apply (Types.var "BuddyListB") Types.int32)] $
        var "listA")
      (Types.apply (Types.var "BuddyListA") Types.int32)
    expectTermWithType "(monomorphic) mutually recursive functions"
      (lets ["f">: lambda "x" $ var "g" @@ var "x",
             "g">: lambda "y" $ primitive _math_add @@ var "y" @@ int32 1] $
            var "f" @@ int32 5)
      (letsTyped [("f", lambdaTyped "x" Types.int32 $ var "g" @@ var "x",
                   Types.mono $ Types.function Types.int32 Types.int32),
                  ("g", lambdaTyped "y" Types.int32 $ primitive _math_add @@ var "y" @@ int32 1,
                   Types.mono $ Types.function Types.int32 Types.int32)] $
        var "f" @@ int32 5)
      Types.int32

  H.describe "Nested let terms" $ do
    expectTermWithType "monomorphic nesting"
      (lets ["x">: int32 1] $
       lets ["y">: primitive _math_add @@ var "x" @@ int32 2] $
       lets ["z">: primitive _math_mul @@ var "y" @@ int32 3] $
            var "z")
      (letsTyped [("x", int32 1, Types.mono Types.int32)] $
       letsTyped [("y", primitive _math_add @@ var "x" @@ int32 2, Types.mono Types.int32)] $
       letsTyped [("z", primitive _math_mul @@ var "y" @@ int32 3, Types.mono Types.int32)] $
        var "z")
      Types.int32
    expectTermWithType "polymorphic nesting"
      (lets ["id">: lambda "x" $ var "x"] $
       lets ["apply">: lambda "f" $ lambda "x" $ var "f" @@ var "x"] $
            var "apply" @@ var "id" @@ string "test")
      (letsTyped [("id", tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ var "x",
                   Types.poly ["t0"] $ Types.function (Types.var "t0") (Types.var "t0"))] $
       letsTyped [("apply", tylams ["t0", "t1"] $ lambdaTyped "f" (Types.function (Types.var "t0") (Types.var "t1")) $ lambdaTyped "x" (Types.var "t0") $ var "f" @@ var "x",
                   Types.poly ["t0", "t1"] $ Types.function (Types.function (Types.var "t0") (Types.var "t1")) (Types.function (Types.var "t0") (Types.var "t1")))] $
        tyapps (var "apply") [Types.string, Types.string] @@ tyapp (var "id") Types.string @@ string "test")
      Types.string
    expectTermWithType "variable capture avoidance"
        (lets ["x">: int32 1] $
          lambda "x" $ lets ["y">: var "x"] $ var "y")
        (tylam "t0" $ letsTyped [("x", int32 1, Types.mono Types.int32)] $
          lambdaTyped "x" (Types.var "t0") $ letsTyped [("y", var "x", Types.mono (Types.var "t0"))] $ var "y")
        (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))
    expectTermWithType "simple let in lambda"
      (lambda "z" $ lets ["y">: var "z"] $ var "y")
      (tylam "t0" $ lambdaTyped "z" (Types.var "t0") $ letsTyped [("y", var "z", Types.mono (Types.var "t0"))] $ var "y")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))

  H.describe "Let with complex expressions" $ do
    expectTermWithType "let in record"
      (record testTypePersonName [
        field "firstName" (lets ["first">: string "John",
                                "middle">: string "Q"] $
                               primitive _strings_cat2 @@ var "first" @@ var "middle"),
        field "lastName" (string "Doe"),
        field "age" (int32 30)])
      (record testTypePersonName [
        field "firstName" (letsTyped [("first", string "John", Types.mono Types.string),
                                     ("middle", string "Q", Types.mono Types.string)] $
                           primitive _strings_cat2 @@ var "first" @@ var "middle"),
        field "lastName" (string "Doe"),
        field "age" (int32 30)])
      (Types.var "Person")
    expectTermWithType "let in function application"
      (lets ["x">: int32 5,
             "y">: int32 3] $
            primitive _math_add @@ var "x" @@ var "y")
      (letsTyped [("x", int32 5, Types.mono Types.int32),
                  ("y", int32 3, Types.mono Types.int32)] $
        primitive _math_add @@ var "x" @@ var "y")
      Types.int32
    expectTermWithType "polymorphic let binding"
      (lets ["id">: lambda "x" $ var "x"] $
            tuple [var "id" @@ int32 42, var "id" @@ string "hello"])
      (letsTyped [("id", tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ var "x",
                   Types.poly ["t0"] $ Types.function (Types.var "t0") (Types.var "t0"))] $
        tuple [tyapp (var "id") Types.int32 @@ int32 42, tyapp (var "id") Types.string @@ string "hello"])
      (Types.product [Types.int32, Types.string])
    expectTermWithType "composition"
      (lets ["compose">: lambda "f" $ lambda "g" $ lambda "x" $ var "f" @@ (var "g" @@ var "x"),
             "add1">: lambda "n" $ primitive _math_add @@ var "n" @@ int32 1,
             "double">: lambda "n" $ primitive _math_mul @@ var "n" @@ int32 2] $
            (var "compose" @@ var "add1" @@ var "double") @@ int32 5)
      (letsTyped [
        ("compose", tylams ["t0", "t1", "t2"] $
          lambdaTyped "f" (Types.function (Types.var "t0") (Types.var "t1")) $
            lambdaTyped "g" (Types.function (Types.var "t2") (Types.var "t0")) $
              lambdaTyped "x" (Types.var "t2") $ var "f" @@ (var "g" @@ var "x"),
          Types.poly ["t0", "t1", "t2"] $
            Types.function
              (Types.function (Types.var "t0") (Types.var "t1"))
              (Types.function (Types.function (Types.var "t2") (Types.var "t0"))
                (Types.function (Types.var "t2") (Types.var "t1")))),
        ("add1", lambdaTyped "n" Types.int32 $ primitive _math_add @@ var "n" @@ int32 1,
         Types.mono $ Types.function Types.int32 Types.int32),
        ("double", lambdaTyped "n" Types.int32 $ primitive _math_mul @@ var "n" @@ int32 2,
         Types.mono $ Types.function Types.int32 Types.int32)] $
        (tyapps (var "compose") [Types.int32, Types.int32, Types.int32] @@ var "add1" @@ var "double") @@ int32 5)
      Types.int32

checkTypeOfLists :: H.SpecWith ()
checkTypeOfLists = H.describe "Lists" $ do
  H.describe "Lists of literals" $ do
    expectSameTermWithType "int list"
      (list [int32 1, int32 2])
      (Types.list Types.int32)
    expectSameTermWithType "string list"
      (list [string "hello", string "world"])
      (Types.list Types.string)
    expectSameTermWithType "single element list"
      (list [bigint 42])
      (Types.list Types.bigint)
    expectSameTermWithType "mixed numeric types"
      (list [float32 1.0, float32 2.5, float32 3.14])
      (Types.list Types.float32)

  H.describe "Empty lists" $ do
    expectTermWithType "empty list"
      (list [])
      (tylam "t0" $ tyapp (list []) (Types.var "t0"))
      (Types.forAll "t0" $ Types.list $ Types.var "t0")
    expectTermWithType "pair of empty lists"
      (pair (list []) (list []))
      (tylams ["t0", "t1"] $ pair (tyapp (list []) (Types.var "t0")) (tyapp (list []) (Types.var "t1")))
      (Types.forAlls ["t0", "t1"] $ Types.pair (Types.list $ Types.var "t0") (Types.list $ Types.var "t1"))
    expectTermWithType "empty list in tuple"
      (tuple [list [], string "context"])
      (tylam "t0" $ tuple [tyapp (list []) (Types.var "t0"), string "context"])
      (Types.forAll "t0" $ Types.product [Types.list $ Types.var "t0", Types.string])

  H.describe "Polymorphic lists" $ do
    expectTermWithType "list from lambda"
      (lambda "x" $ list [var "x"])
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ list [var "x"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.list $ Types.var "t0"))
    expectTermWithType "list with repeated var"
      (lambda "x" $ list [var "x", var "x"])
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ list [var "x", var "x"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.list $ Types.var "t0"))
    expectTermWithType "list from two lambdas"
      (lambda "x" $ lambda "y" $ list [var "x", var "y"])
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ lambdaTyped "y" (Types.var "t0") $ list [var "x", var "y"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.function (Types.var "t0") (Types.list $ Types.var "t0")))

  H.describe "Nested lists" $ do
    expectSameTermWithType "list of lists"
      (list [list [int32 1], list [int32 2, int32 3]])
      (Types.list $ Types.list Types.int32)
    expectTermWithType "empty nested lists"
      (list [list [], list []])
      (tylam "t0" $ list [tyapp (list []) (Types.var "t0"), tyapp (list []) (Types.var "t0")])
      (Types.forAll "t0" $ Types.list $ Types.list $ Types.var "t0")
    expectTermWithType "nested polymorphic"
      (lambda "x" $ list [list [var "x"]])
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ list [list [var "x"]])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.list $ Types.list $ Types.var "t0"))

  H.describe "Lists in complex contexts" $ do
    expectSameTermWithType "multiple lists in tuple"
      (tuple [
        list [int32 1, int32 2],
        list [string "a", string "b"]])
      (Types.product [Types.list Types.int32, Types.list Types.string])

checkTypeOfLiterals :: H.SpecWith ()
checkTypeOfLiterals = H.describe "Literals" $ do
  H.describe "Boolean literals" $ do
    expectSameTermWithType "true"
      (boolean True)
      Types.boolean
    expectSameTermWithType "false"
      (boolean False)
      Types.boolean

  H.describe "String literals" $ do
    expectSameTermWithType "simple string"
      (string "hello")
      Types.string
    expectSameTermWithType "empty string"
      (string "")
      Types.string
    expectSameTermWithType "unicode string"
      (string "café")
      Types.string

  H.describe "Integer literals" $ do
    expectSameTermWithType "bigint"
      (bigint 42)
      Types.bigint
    expectSameTermWithType "int8"
      (int8 127)
      Types.int8
    expectSameTermWithType "int16"
      (int16 32767)
      Types.int16
    expectSameTermWithType "int32"
      (int32 2147483647)
      Types.int32
    expectSameTermWithType "int64"
      (int64 9223372036854775807)
      Types.int64
    expectSameTermWithType "uint8"
      (uint8 255)
      Types.uint8
    expectSameTermWithType "uint16"
      (uint16 65535)
      Types.uint16
    expectSameTermWithType "uint32"
      (uint32 4294967295)
      Types.uint32
    expectSameTermWithType "uint64"
      (uint64 18446744073709551615)
      Types.uint64

  H.describe "Float literals" $ do
    expectSameTermWithType "bigfloat"
      (bigfloat 3.14159)
      Types.bigfloat
    expectSameTermWithType "float32"
      (float32 2.71828)
      Types.float32
    expectSameTermWithType "float64"
      (float64 1.41421)
      Types.float64

  H.describe "Binary literals" $ do
    expectSameTermWithType "binary"
      (binary "SGVsbG8gV29ybGQ=")  -- "Hello World" in base64
      Types.binary

  H.describe "Literals in complex contexts" $ do
    expectSameTermWithType "literals in tuple"
      (tuple [boolean True, string "test", int32 42, float32 3.14])
      (Types.product [Types.boolean, Types.string, Types.int32, Types.float32])
    expectSameTermWithType "literals in list"
      (list [string "one", string "two", string "three"])
      (Types.list Types.string)
    expectSameTermWithType "literals in record"
      (record testTypePersonName [
        field "firstName" (string "Alice"),
        field "lastName" (string "Smith"),
        field "age" (int32 30)])
      (Types.var "Person")
    expectTermWithType "literals in let binding"
      (lets ["x">: int32 100,
             "y">: string "hello",
             "z">: boolean True] $
            tuple [var "x", var "y", var "z"])
      (letsTyped [("x", int32 100, Types.mono Types.int32),
                  ("y", string "hello", Types.mono Types.string),
                  ("z", boolean True, Types.mono Types.boolean)] $
        tuple [var "x", var "y", var "z"])
      (Types.product [Types.int32, Types.string, Types.boolean])

checkTypeOfMaps :: H.SpecWith ()
checkTypeOfMaps = H.describe "Maps" $ do
  H.describe "Monomorphic maps" $ do
    expectTermWithType "empty map"
      (Terms.map M.empty)
      (tylams ["t0", "t1"] $ tyapps (Terms.map M.empty) [Types.var "t0", Types.var "t1"])
      (Types.forAlls ["t0", "t1"] $ Types.map (Types.var "t0") (Types.var "t1"))
    expectSameTermWithType "int to string map"
      (Terms.map $ M.fromList [(int32 1, string "one"),
                               (int32 2, string "two")])
      (Types.map Types.int32 Types.string)
    expectSameTermWithType "string to int map"
      (Terms.map $ M.fromList [(string "a", int32 1),
                               (string "b", int32 2)])
      (Types.map Types.string Types.int32)
    expectSameTermWithType "single entry map"
      (Terms.map $ M.singleton (bigint 42) (boolean True))
      (Types.map Types.bigint Types.boolean)

  H.describe "Polymorphic maps" $ do
    expectTermWithType "map from lambda keys"
      (lambda "k" $ Terms.map $ M.singleton (var "k") (string "value"))
      (tylam "t0" $ lambdaTyped "k" (Types.var "t0") $ Terms.map $ M.singleton (var "k") (string "value"))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.map (Types.var "t0") Types.string))
    expectTermWithType "map from lambda values"
      (lambda "v" $ Terms.map $ M.singleton (string "key") (var "v"))
      (tylam "t0" $ lambdaTyped "v" (Types.var "t0") $ Terms.map $ M.singleton (string "key") (var "v"))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.map Types.string (Types.var "t0")))
    expectTermWithType "map from lambda both"
      (lambda "k" $ lambda "v" $ Terms.map $ M.singleton (var "k") (var "v"))
      (tylams ["t0", "t1"] $ lambdaTyped "k" (Types.var "t0") $ lambdaTyped "v" (Types.var "t1") $ Terms.map $ M.singleton (var "k") (var "v"))
      (Types.forAlls ["t0", "t1"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.map (Types.var "t0") (Types.var "t1"))))
    expectTermWithType "map with repeated variables"
      (lambda "x" $ Terms.map $ M.singleton (var "x") (var "x"))
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ Terms.map $ M.singleton (var "x") (var "x"))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.map (Types.var "t0") (Types.var "t0")))

  H.describe "Maps in complex contexts" $ do
    expectSameTermWithType "map in tuple"
      (tuple [Terms.map $ M.singleton (int32 1) (string "one"),
              string "context"])
      (Types.product [Types.map Types.int32 Types.string, Types.string])
    expectSameTermWithType "nested maps"
      (Terms.map $ M.singleton (string "outer") (Terms.map $ M.singleton (int32 1) (boolean True)))
      (Types.map Types.string (Types.map Types.int32 Types.boolean))
    expectTermWithType "map in let binding"
      (lets ["lookup">: Terms.map $ M.fromList [(string "key1", int32 100),
                                                (string "key2", int32 200)]] $
            var "lookup")
      (letsTyped [("lookup", Terms.map $ M.fromList [(string "key1", int32 100),
                                                     (string "key2", int32 200)],
                   Types.mono $ Types.map Types.string Types.int32)] $
        var "lookup")
      (Types.map Types.string Types.int32)

  H.describe "Maps with complex types" $ do
    expectSameTermWithType "map of records"
      (Terms.map $ M.singleton (string "person1")
                     (record testTypePersonName [
                       field "firstName" (string "Alice"),
                       field "lastName" (string "Smith"),
                       field "age" (int32 25)]))
      (Types.map Types.string (Types.var "Person"))
    expectSameTermWithType "map of lists"
      (Terms.map $ M.fromList [(int32 1, list [string "a", string "b"]),
                               (int32 2, list [string "c", string "d"])])
      (Types.map Types.int32 (Types.list Types.string))
    expectSameTermWithType "map of tuples"
      (Terms.map $ M.singleton (string "coords") (tuple [int32 10, int32 20]))
      (Types.map Types.string (Types.product [Types.int32, Types.int32]))

checkTypeOfOptionals :: H.SpecWith ()
checkTypeOfOptionals = H.describe "Optionals" $ do
  H.describe "Monomorphic optionals" $ do
    expectTermWithType "nothing"
      (nothing)
      (tylam "t0" $ tyapp nothing (Types.var "t0"))
      (Types.forAll "t0" $ Types.optional $ Types.var "t0")
    expectSameTermWithType "just int"
      (just $ int32 42)
      (Types.optional Types.int32)
    expectSameTermWithType "just string"
      (just $ string "hello")
      (Types.optional Types.string)
    expectSameTermWithType "just boolean"
      (just $ boolean True)
      (Types.optional Types.boolean)

  H.describe "Polymorphic optionals" $ do
    expectTermWithType "optional from lambda"
      (lambda "x" $ just $ var "x")
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ just $ var "x")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.optional $ Types.var "t0"))
    expectTermWithType "nothing from lambda"
      (lambda "x" $ nothing)
      (tylams ["t0", "t1"] $ lambdaTyped "x" (Types.var "t0") $ tyapp nothing (Types.var "t1"))
      (Types.forAlls ["t0", "t1"] $ Types.function (Types.var "t0") (Types.optional $ Types.var "t1"))
    expectTermWithType "conditional optional"
      (lambda "x" $ lambda "flag" $
        primitive _logic_ifElse @@ var "flag" @@
          (just $ var "x") @@
          (nothing))
      (tylams ["t0"] $ lambdaTyped "x" (Types.var "t0") $ lambdaTyped "flag" Types.boolean $
        tyapp (primitive _logic_ifElse) (Types.optional $ Types.var "t0") @@ var "flag" @@
          (just $ var "x") @@
          (tyapp nothing (Types.var "t0")))
      (Types.forAlls ["t0"] $ Types.function (Types.var "t0") (Types.function Types.boolean (Types.optional $ Types.var "t0")))

  H.describe "Optionals in complex contexts" $ do
    expectSameTermWithType "optional in tuple"
      (tuple [just $ int32 100, string "context"])
      (Types.product [Types.optional Types.int32, Types.string])
    expectTermWithType "optional in record"
      (record testTypeBuddyListAName [
        field "head" (string "first"),
        field "tail" (just $ record testTypeBuddyListBName [
          field "head" (string "second"),
          field "tail" (nothing)])])
      (tyapp (record testTypeBuddyListAName [
        field "head" (string "first"),
        field "tail" (just $ tyapp (record testTypeBuddyListBName [
          field "head" (string "second"),
          field "tail" (tyapp nothing (Types.apply (Types.var "BuddyListA") Types.string))]) Types.string)]) Types.string)
      (Types.apply (Types.var "BuddyListA") Types.string)
    expectTermWithType "optional in let binding"
      (lets ["maybeValue">: just $ int32 42] $
            var "maybeValue")
      (letsTyped [("maybeValue", just $ int32 42, Types.mono $ Types.optional Types.int32)] $
        var "maybeValue")
      (Types.optional Types.int32)

  H.describe "Nested optionals" $ do
    expectSameTermWithType "optional of optional"
      (just $ just $ string "nested")
      (Types.optional $ Types.optional Types.string)
    expectSameTermWithType "optional of list"
      (just $ list [int32 1, int32 2, int32 3])
      (Types.optional $ Types.list Types.int32)
    expectTermWithType "list of optionals"
      (list [just $ string "a", nothing, just $ string "b"])
      (list [just $ string "a", tyapp nothing Types.string, just $ string "b"])
      (Types.list $ Types.optional Types.string)

  H.describe "Optionals with complex types" $ do
    expectSameTermWithType "optional record"
      (just $ record testTypePersonName [
        field "firstName" (string "Alice"),
        field "lastName" (string "Smith"),
        field "age" (int32 30)])
      (Types.optional $ Types.var "Person")
    expectSameTermWithType "optional tuple"
      (just $ tuple [int32 10, string "test"])
      (Types.optional $ Types.product [Types.int32, Types.string])
    expectSameTermWithType "optional map"
      (just $ Terms.map $ M.singleton (string "key") (int32 42))
      (Types.optional $ Types.map Types.string Types.int32)

checkTypeOfPrimitives :: H.SpecWith ()
checkTypeOfPrimitives = H.describe "Primitives" $ do
  H.describe "Nullary primitives" $ do
    expectTermWithType "empty map"
      (primitive _maps_empty)
      (tylams ["t0", "t1"] $ tyapps (primitive _maps_empty) [Types.var "t0", Types.var "t1"])
      (Types.forAlls ["t0", "t1"] $ Types.map (Types.var "t0") (Types.var "t1"))
    expectTermWithType "empty set"
      (primitive _sets_empty)
      (tylam "t0" $ tyapp (primitive _sets_empty) (Types.var "t0"))
      (Types.forAll "t0" $ Types.set $ Types.var "t0")

  H.describe "Unary primitives" $ do
    expectTermWithType "lists head"
      (primitive _lists_head)
      (tylam "t0" $ tyapp (primitive _lists_head) (Types.var "t0"))
      (Types.forAll "t0" $ Types.function (Types.list $ Types.var "t0") (Types.var "t0"))
    expectSameTermWithType "math neg"
      (primitive _math_neg)
      (Types.function Types.int32 Types.int32)
    expectSameTermWithType "logic not"
      (primitive _logic_not)
      (Types.function Types.boolean Types.boolean)

  H.describe "Binary primitives" $ do
    expectSameTermWithType "math add"
      (primitive _math_add)
      (Types.function Types.int32 (Types.function Types.int32 Types.int32))
    expectTermWithType "lists cons"
      (primitive _lists_cons)
      (tylam "t0" $ tyapp (primitive _lists_cons) (Types.var "t0"))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.function (Types.list $ Types.var "t0") (Types.list $ Types.var "t0")))
    expectTermWithType "maps insert"
      (primitive _maps_insert)
      (tylams ["t0", "t1"] $ tyapps (primitive _maps_insert) [Types.var "t0", Types.var "t1"])
      (Types.forAlls ["t0", "t1"] $ Types.function
        (Types.var "t0")
        (Types.function (Types.var "t1") (Types.function (Types.map (Types.var "t0") (Types.var "t1")) (Types.map (Types.var "t0") (Types.var "t1")))))

  H.describe "Ternary primitives" $ do
    expectTermWithType "logic ifElse"
      (primitive _logic_ifElse)
      (tylam "t0" $ tyapp (primitive _logic_ifElse) (Types.var "t0"))
      (Types.forAll "t0" $ Types.function Types.boolean (Types.function (Types.var "t0") (Types.function (Types.var "t0") (Types.var "t0"))))
    expectTermWithType "lists foldl"
      (primitive _lists_foldl)
      (tylams ["t0", "t1"] $ tyapps (primitive _lists_foldl) [Types.var "t0", Types.var "t1"])
      (Types.forAlls ["t0", "t1"] $ Types.function
        (Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.var "t0")))
        (Types.function (Types.var "t0") (Types.function (Types.list $ Types.var "t1") (Types.var "t0"))))

  H.describe "Monomorphic vs polymorphic" $ do
    expectSameTermWithType "monomorphic math"
      (primitive _math_add)
      (Types.function Types.int32 (Types.function Types.int32 Types.int32))
    expectTermWithType "polymorphic identity"
      (primitive _equality_identity)
      (tylam "t0" $ tyapp (primitive _equality_identity) (Types.var "t0"))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))
    expectTermWithType "polymorphic map"
      (primitive _lists_map)
      (tylams ["t0", "t1"] $ tyapps (primitive _lists_map) [Types.var "t0", Types.var "t1"])
      (Types.forAlls ["t0", "t1"] $ Types.function
        (Types.function (Types.var "t0") (Types.var "t1"))
        (Types.function (Types.list $ Types.var "t0") (Types.list $ Types.var "t1")))

  H.describe "Higher-order primitives" $ do
    expectTermWithType "lists map function"
      (primitive _lists_map @@ (lambda "x" $ primitive _math_add @@ var "x" @@ int32 1))
      (tyapps (primitive _lists_map) [Types.int32, Types.int32] @@ (lambdaTyped "x" Types.int32 $ primitive _math_add @@ var "x" @@ int32 1))
      (Types.function (Types.list Types.int32) (Types.list Types.int32))
    expectTermWithType "lists filter"
      (primitive _lists_filter)
      (tylam "t0" $ tyapp (primitive _lists_filter) (Types.var "t0"))
      (Types.forAll "t0" $ Types.function (Types.function (Types.var "t0") Types.boolean) (Types.function (Types.list $ Types.var "t0") (Types.list $ Types.var "t0")))
    expectTermWithType "optionals maybe"
      (primitive _optionals_maybe)
      (tylams ["t0", "t1"] $ tyapps (primitive _optionals_maybe) [Types.var "t0", Types.var "t1"])
      (Types.forAlls ["t0", "t1"] $
        Types.function (Types.var "t0") (Types.function (Types.function (Types.var "t1") (Types.var "t0")) (Types.function (Types.optional $ Types.var "t1") (Types.var "t0"))))

  H.describe "Primitives in complex contexts" $ do
    expectTermWithType "primitive composition"
      (lets ["double">: lambda "x" $ primitive _math_mul @@ var "x" @@ int32 2,
             "increment">: lambda "x" $ primitive _math_add @@ var "x" @@ int32 1] $
            primitive _lists_map @@ var "double" @@ (primitive _lists_map @@ var "increment" @@ list [int32 1, int32 2, int32 3]))
      (letsTyped [("double", lambdaTyped "x" Types.int32 $ primitive _math_mul @@ var "x" @@ int32 2,
                   Types.mono $ Types.function Types.int32 Types.int32),
                  ("increment", lambdaTyped "x" Types.int32 $ primitive _math_add @@ var "x" @@ int32 1,
                   Types.mono $ Types.function Types.int32 Types.int32)] $
        tyapps (primitive _lists_map) [Types.int32, Types.int32] @@ var "double" @@ (tyapps (primitive _lists_map) [Types.int32, Types.int32] @@ var "increment" @@ list [int32 1, int32 2, int32 3]))
      (Types.list Types.int32)
    expectTermWithType "nested higher-order"
      (primitive _lists_map @@ (primitive _lists_map @@ (primitive _math_add @@ int32 1)) @@
       list [list [int32 1, int32 2], list [int32 3, int32 4]])
      (tyapps (primitive _lists_map) [Types.list Types.int32, Types.list Types.int32] @@ (tyapps (primitive _lists_map) [Types.int32, Types.int32] @@ (primitive _math_add @@ int32 1)) @@
       list [list [int32 1, int32 2], list [int32 3, int32 4]])
      (Types.list $ Types.list Types.int32)

checkTypeOfProducts :: H.SpecWith ()
checkTypeOfProducts = H.describe "Products" $ do
  H.describe "Monomorphic products" $ do
    expectSameTermWithType "empty tuple"
      (tuple [])
      (Types.product [])
    expectSameTermWithType "singleton tuple"
      (tuple [int32 42])
      (Types.product [Types.int32])
    expectSameTermWithType "pair tuple"
      (tuple [int32 42, string "foo"])
      (Types.product [Types.int32, Types.string])
    expectSameTermWithType "triple tuple"
      (tuple [int32 1, int32 2, int32 3])
      (Types.product [Types.int32, Types.int32, Types.int32])
    expectSameTermWithType "mixed types"
      (tuple [unit, string "test", bigint 100])
      (Types.product [Types.unit, Types.string, Types.bigint])

  H.describe "Polymorphic products" $ do
    expectTermWithType "lambda with var"
      (lambda "x" $ tuple [var "x", string "foo"])
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ tuple [var "x", string "foo"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.product [Types.var "t0", Types.string]))
    expectTermWithType "two variables"
      (lambda "x" $ lambda "y" $ tuple [var "x", var "y"])
      (tylams ["t0", "t1"] $ lambdaTyped "x" (Types.var "t0") $ lambdaTyped "y" (Types.var "t1") $ tuple [var "x", var "y"])
      (Types.forAlls ["t0", "t1"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.product [Types.var "t0", Types.var "t1"])))
    expectTermWithType "repeated variable"
      (lambda "x" $ tuple [var "x", var "x"])
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ tuple [var "x", var "x"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.product [Types.var "t0", Types.var "t0"]))

  H.describe "Nested products" $ do
    expectSameTermWithType "tuple in tuple"
      (tuple [tuple [int32 1], string "foo"])
      (Types.product [Types.product [Types.int32], Types.string])
    expectTermWithType "nested polymorphic"
      (lambda "x" $ tuple [tuple [var "x"], tuple [string "test"]])
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ tuple [tuple [var "x"], tuple [string "test"]])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.product [Types.product [Types.var "t0"], Types.product [Types.string]]))

checkTypeOfProductEliminations :: H.SpecWith ()
checkTypeOfProductEliminations = H.describe "Product eliminations" $ do
  H.describe "Simple tuple projections" $ do
    expectSameTermWithType "projection from pair"
      (untuple 2 0 @@ pair (int32 42) (string "hello"))
      Types.int32
    expectSameTermWithType "second projection from pair"
      (untuple 2 1 @@ pair (int32 42) (string "hello"))
      Types.string
    expectSameTermWithType "projection from triple"
      (untuple 3 1 @@ triple (int32 1) (string "middle") (boolean True))
      Types.string
    expectSameTermWithType "first element of triple"
      (untuple 3 0 @@ triple (boolean False) (int32 100) (string "last"))
      Types.boolean
    expectSameTermWithType "last element of triple"
      (untuple 3 2 @@ triple (boolean False) (int32 100) (string "last"))
      Types.string

  H.describe "Polymorphic tuple projections" $ do
    expectTermWithType "projection function"
      (untuple 2 0)
      (tylams ["t0", "t1"] $ untuple 2 0)
      (Types.forAlls ["t0", "t1"] $ Types.function
        (Types.product [Types.var "t0", Types.var "t1"])
        (Types.var "t0"))
    expectTermWithType "second projection function"
      (untuple 2 1)
      (tylams ["t0", "t1"] $ untuple 2 1)
      (Types.forAlls ["t0", "t1"] $ Types.function
        (Types.product [Types.var "t0", Types.var "t1"])
        (Types.var "t1"))
    expectTermWithType "triple projection function"
      (untuple 3 1)
      (tylams ["t0", "t1", "t2"] $ untuple 3 1)
      (Types.forAlls ["t0", "t1", "t2"] $ Types.function
        (Types.product [Types.var "t0", Types.var "t1", Types.var "t2"])
        (Types.var "t1"))
    expectTermWithType "projection from lambda"
      (lambda "pair" $ untuple 2 0 @@ var "pair")
      (tylams ["t0", "t1"] $ lambdaTyped "pair" (Types.product [Types.var "t0", Types.var "t1"]) $ untuple 2 0 @@ var "pair")
      (Types.forAlls ["t0", "t1"] $ Types.function
        (Types.product [Types.var "t0", Types.var "t1"])
        (Types.var "t0"))

  H.describe "Projections with variables" $ do
    expectTermWithType "projection with variable tuple"
      (lambda "x" $ lambda "y" $ untuple 2 0 @@ pair (var "x") (var "y"))
      (tylams ["t0", "t1"] $ lambdaTyped "x" (Types.var "t0") $ lambdaTyped "y" (Types.var "t1") $ untuple 2 0 @@ pair (var "x") (var "y"))
      (Types.forAlls ["t0", "t1"] $ Types.function (Types.var "t0")
        (Types.function (Types.var "t1") (Types.var "t0")))
    expectTermWithType "projection preserves polymorphism"
      (lambda "pair" $ pair (untuple 2 0 @@ var "pair") (untuple 2 1 @@ var "pair"))
      (tylams ["t0", "t1"] $ lambdaTyped "pair" (Types.product [Types.var "t0", Types.var "t1"]) $ pair (untuple 2 0 @@ var "pair") (untuple 2 1 @@ var "pair"))
      (Types.forAlls ["t0", "t1"] $ Types.function
        (Types.product [Types.var "t0", Types.var "t1"])
        (Types.product [Types.var "t0", Types.var "t1"]))
    expectTermWithType "nested projection"
      (lambda "nested" $ untuple 2 0 @@ (untuple 2 1 @@ var "nested"))
      (tylams ["t0", "t1", "t2"] $ lambdaTyped "nested" (Types.product [Types.var "t0", Types.product [Types.var "t1", Types.var "t2"]]) $ untuple 2 0 @@ (untuple 2 1 @@ var "nested"))
      (Types.forAlls ["t0", "t1", "t2"] $ Types.function
        (Types.product [Types.var "t0", Types.product [Types.var "t1", Types.var "t2"]])
        (Types.var "t1"))

  H.describe "Projections in complex contexts" $ do
    expectTermWithType "projection in let binding"
      (lets ["pair">: pair (int32 10) (string "test")] $
            untuple 2 0 @@ var "pair")
      (letsTyped [("pair", pair (int32 10) (string "test"), Types.mono $ Types.product [Types.int32, Types.string])] $
        untuple 2 0 @@ var "pair")
      Types.int32
    expectSameTermWithType "projection in tuple"
      (pair (untuple 2 0 @@ pair (int32 1) (string "a"))
            (untuple 2 1 @@ pair (int32 2) (string "b")))
      (Types.product [Types.int32, Types.string])
    expectSameTermWithType "projection in list"
      (list [untuple 2 0 @@ pair (int32 1) (string "a"),
             untuple 2 0 @@ pair (int32 2) (string "b")])
      (Types.list Types.int32)

  H.describe "Projections with mixed types" $ do
    expectSameTermWithType "projection from mixed tuple"
      (untuple 4 2 @@ tuple4 (int32 1) (string "test") (boolean True) (float32 3.14))
      Types.boolean
    expectTermWithType "projection chain"
      (lets ["quadruple">: tuple4 (int32 1) (string "test") (boolean True) (float32 3.14)] $
            tuple4 (untuple 4 0 @@ var "quadruple")
                   (untuple 4 1 @@ var "quadruple")
                   (untuple 4 2 @@ var "quadruple")
                   (untuple 4 3 @@ var "quadruple"))
      (letsTyped [("quadruple", tuple4 (int32 1) (string "test") (boolean True) (float32 3.14),
                   Types.mono $ Types.product [Types.int32, Types.string, Types.boolean, Types.float32])] $
        tuple4 (untuple 4 0 @@ var "quadruple")
               (untuple 4 1 @@ var "quadruple")
               (untuple 4 2 @@ var "quadruple")
               (untuple 4 3 @@ var "quadruple"))
      (Types.product [Types.int32, Types.string, Types.boolean, Types.float32])
    expectTermWithType "projection with function result"
      (untuple 2 1 @@ pair (int32 42) (lambda "x" $ var "x"))
      (tylam "t0" $ untuple 2 1 @@ pair (int32 42) (lambdaTyped "x" (Types.var "t0") $ var "x"))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))

  H.describe "Projections with primitive functions" $ do
    expectTermWithType "lists.map with untuple"
      (primitive _lists_map @@ (untuple 2 0))
      (tylams ["t0", "t1"] $ tyapps (primitive _lists_map) [Types.product [Types.var "t0", Types.var "t1"], Types.var "t0"] @@ (untuple 2 0))
      (Types.forAlls ["t0", "t1"] $
        Types.function (Types.list (Types.product [Types.var "t0", Types.var "t1"])) (Types.list (Types.var "t0")))

checkTypeOfRecords :: H.SpecWith ()
checkTypeOfRecords = H.describe "Records" $ do
  H.describe "Monomorphic records" $ do
    expectSameTermWithType "latlon record"
      (record testTypeLatLonName [
        field "lat" (float32 19.5429),
        field "lon" (float32 (0-155.6659))])
      (Types.var "LatLon")
    expectTermWithType "latlon with variable"
      (lambda "x" $ record testTypeLatLonName [
        field "lat" (float32 19.5429),
        field "lon" (var "x")])
      (lambdaTyped "x" Types.float32 $ record testTypeLatLonName [
        field "lat" (float32 19.5429),
        field "lon" (var "x")])
      (Types.function Types.float32 (Types.var "LatLon"))
    expectSameTermWithType "person record"
      (record testTypePersonName [
        field "firstName" (string "Alice"),
        field "lastName" (string "Smith"),
        field "age" (int32 30)])
      (Types.var "Person")
    expectSameTermWithType "empty record"
      (record testTypeUnitName [])
      (Types.var "Unit")
    expectTermWithType "person with variables"
      (lambda "name" $ lambda "age" $ record testTypePersonName [
        field "firstName" (var "name"),
        field "lastName" (string "Doe"),
        field "age" (var "age")])
      (lambdaTyped "name" Types.string $ lambdaTyped "age" Types.int32 $ record testTypePersonName [
        field "firstName" (var "name"),
        field "lastName" (string "Doe"),
        field "age" (var "age")])
      (Types.function Types.string (Types.function Types.int32 (Types.var "Person")))

  H.describe "Polymorphic records" $ do
    expectTermWithType "latlon poly float"
      (record testTypeLatLonPolyName [
        field "lat" (float32 19.5429),
        field "lon" (float32 (0-155.6659))])
      (tyapp (record testTypeLatLonPolyName [
        field "lat" (float32 19.5429),
        field "lon" (float32 (0-155.6659))]) Types.float32)
      (Types.apply (Types.var "LatLonPoly") Types.float32)
    expectTermWithType "latlon poly int64"
      (record testTypeLatLonPolyName [
        field "lat" (int64 195429),
        field "lon" (int64 (0-1556659))])
      (tyapp (record testTypeLatLonPolyName [
        field "lat" (int64 195429),
        field "lon" (int64 (0-1556659))]) Types.int64)
      (Types.apply (Types.var "LatLonPoly") Types.int64)
    expectTermWithType "latlon poly variable"
      (lambda "x" $ record testTypeLatLonPolyName [
        field "lat" (var "x"),
        field "lon" (var "x")])
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ tyapp (record testTypeLatLonPolyName [
        field "lat" (var "x"),
        field "lon" (var "x")]) (Types.var "t0"))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.apply (Types.var "LatLonPoly") (Types.var "t0")))
    expectTermWithType "buddylist string"
      (record testTypeBuddyListAName [
        field "head" (string "first"),
        field "tail" (nothing)])
      (tyapp (record testTypeBuddyListAName [
        field "head" (string "first"),
        field "tail" (tyapp nothing (Types.apply (Types.var "BuddyListB") Types.string))]) Types.string)
      (Types.apply (Types.var "BuddyListA") Types.string)
    expectTermWithType "buddylist variable"
      (lambda "x" $ record testTypeBuddyListAName [
        field "head" (var "x"),
        field "tail" (nothing)])
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ tyapp (record testTypeBuddyListAName [
        field "head" (var "x"),
        field "tail" (tyapp nothing (Types.apply (Types.var "BuddyListB") (Types.var "t0")))]) (Types.var "t0"))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.apply (Types.var "BuddyListA") (Types.var "t0")))

  H.describe "Records in complex contexts" $ do
    expectSameTermWithType "records in tuple"
      (tuple [
        record testTypePersonName [
          field "firstName" (string "Bob"),
          field "lastName" (string "Jones"),
          field "age" (int32 25)],
        record testTypeLatLonName [
          field "lat" (float32 1.0),
          field "lon" (float32 2.0)]])
      (Types.product [Types.var "Person", Types.var "LatLon"])
    expectTermWithType "poly records in tuple"
      (tuple [
        record testTypeLatLonPolyName [
          field "lat" (int32 1),
          field "lon" (int32 2)],
        record testTypeBuddyListAName [
          field "head" (string "test"),
          field "tail" (nothing)]])
      (tuple [
        tyapp (record testTypeLatLonPolyName [
          field "lat" (int32 1),
          field "lon" (int32 2)]) Types.int32,
        tyapp (record testTypeBuddyListAName [
          field "head" (string "test"),
          field "tail" (tyapp nothing (Types.apply (Types.var "BuddyListB") Types.string))]) Types.string])
      (Types.product [
        Types.apply (Types.var "LatLonPoly") Types.int32,
        Types.apply (Types.var "BuddyListA") Types.string])
    expectTermWithType "recursive record"
      (record testTypeIntListName [
        field "head" (int32 42),
        field "tail" (just $
          record testTypeIntListName [
            field "head" (int32 43),
            field "tail" (nothing)])])
      (record testTypeIntListName [
        field "head" (int32 42),
        field "tail" (just $
          record testTypeIntListName [
            field "head" (int32 43),
            field "tail" (tyapp nothing (Types.var "IntList"))])])
      (Types.var "IntList")

  H.describe "Multi-parameter polymorphic records" $ do
    expectTermWithType "triple with three monomorphic types"
      (record testTypeTripleName [
        field "first" (int32 1),
        field "second" (string "middle"),
        field "third" (boolean True)])
      (tyapps (record testTypeTripleName [
        field "first" (int32 1),
        field "second" (string "middle"),
        field "third" (boolean True)]) [Types.int32, Types.string, Types.boolean])
      (Types.applys (Types.var "Triple") [Types.int32, Types.string, Types.boolean])

    expectTermWithType "triple with PersonOrSomething containing map"
      (lambda "k" $ lambda "v" $
        record testTypeTripleName [
          field "first" (string "prefix"),
          field "second" (variant testTypePersonOrSomethingName (Name "other")
            (Terms.map $ M.singleton (var "k") (var "v"))),
          field "third" (int32 999)])
      (tylams ["t0", "t1"] $
        lambdaTyped "k" (Types.var "t0") $
        lambdaTyped "v" (Types.var "t1") $
        tyapps (record testTypeTripleName [
          field "first" (string "prefix"),
          field "second" (tyapp (variant testTypePersonOrSomethingName (Name "other")
            (Terms.map $ M.singleton (var "k") (var "v"))) (Types.map (Types.var "t0") (Types.var "t1"))),
          field "third" (int32 999)])
          [Types.string,
           Types.apply (Types.var "PersonOrSomething") (Types.map (Types.var "t0") (Types.var "t1")),
           Types.int32])
      (Types.forAlls ["t0", "t1"] $
        Types.function (Types.var "t0") $
        Types.function (Types.var "t1") $
        Types.applys (Types.var "Triple")
          [Types.string,
           Types.apply (Types.var "PersonOrSomething") (Types.map (Types.var "t0") (Types.var "t1")),
           Types.int32])

checkTypeOfRecordEliminations :: H.SpecWith ()
checkTypeOfRecordEliminations = H.describe "Record eliminations" $ do
  H.describe "Simple record projections" $ do
    expectSameTermWithType "project firstName from Person"
      (project testTypePersonName (Name "firstName"))
      (Types.function (Types.var "Person") Types.string)
    expectSameTermWithType "project lastName from Person"
      (project testTypePersonName (Name "lastName"))
      (Types.function (Types.var "Person") Types.string)
    expectSameTermWithType "project age from Person"
      (project testTypePersonName (Name "age"))
      (Types.function (Types.var "Person") Types.int32)
    expectSameTermWithType "project lat from LatLon"
      (project testTypeLatLonName (Name "lat"))
      (Types.function (Types.var "LatLon") Types.float32)
    expectSameTermWithType "project lon from LatLon"
      (project testTypeLatLonName (Name "lon"))
      (Types.function (Types.var "LatLon") Types.float32)

  H.describe "Record projections applied to records" $ do
    expectSameTermWithType "project firstName applied to person record"
      (project testTypePersonName (Name "firstName") @@
       record testTypePersonName [
         field "firstName" (string "Alice"),
         field "lastName" (string "Smith"),
         field "age" (int32 30)])
      Types.string
    expectSameTermWithType "project age applied to person record"
      (project testTypePersonName (Name "age") @@
       record testTypePersonName [
         field "firstName" (string "Bob"),
         field "lastName" (string "Jones"),
         field "age" (int32 25)])
      Types.int32
    expectSameTermWithType "project lat applied to LatLon record"
      (project testTypeLatLonName (Name "lat") @@
       record testTypeLatLonName [
         field "lat" (float32 40.7128),
         field "lon" (float32 (-74.0060))])
      Types.float32

  H.describe "Polymorphic record projections" $ do
    expectTermWithType "project lat from polymorphic LatLonPoly"
      (project testTypeLatLonPolyName (Name "lat"))
      (tylam "t0" $ tyapp (project testTypeLatLonPolyName (Name "lat")) (Types.var "t0"))
      (Types.forAll "t0" $ Types.function (Types.apply (Types.var "LatLonPoly") (Types.var "t0")) (Types.var "t0"))
    expectTermWithType "project lon from polymorphic LatLonPoly"
      (project testTypeLatLonPolyName (Name "lon"))
      (tylam "t0" $ tyapp (project testTypeLatLonPolyName (Name "lon")) (Types.var "t0"))
      (Types.forAll "t0" $ Types.function (Types.apply (Types.var "LatLonPoly") (Types.var "t0")) (Types.var "t0"))
    expectTermWithType "project head from BuddyListA"
      (project testTypeBuddyListAName (Name "head"))
      (tylam "t0" $ tyapp (project testTypeBuddyListAName (Name "head")) (Types.var "t0"))
      (Types.forAll "t0" $ Types.function (Types.apply (Types.var "BuddyListA") (Types.var "t0")) (Types.var "t0"))
    expectTermWithType "project tail from BuddyListA"
      (project testTypeBuddyListAName (Name "tail"))
      (tylam "t0" $ tyapp (project testTypeBuddyListAName (Name "tail")) (Types.var "t0"))
      (Types.forAll "t0" $ Types.function
        (Types.apply (Types.var "BuddyListA") (Types.var "t0"))
        (Types.optional (Types.apply (Types.var "BuddyListB") (Types.var "t0"))))

  H.describe "Polymorphic record projections applied" $ do
    expectTermWithType "project lat from LatLonPoly with int32"
      (project testTypeLatLonPolyName (Name "lat") @@
       record testTypeLatLonPolyName [
         field "lat" (int32 40),
         field "lon" (int32 (-74))])
      (tyapp (project testTypeLatLonPolyName (Name "lat")) Types.int32 @@
       tyapp (record testTypeLatLonPolyName [
         field "lat" (int32 40),
         field "lon" (int32 (-74))]) Types.int32)
      Types.int32
    expectTermWithType "project lon from LatLonPoly with float64"
      (project testTypeLatLonPolyName (Name "lon") @@
       record testTypeLatLonPolyName [
         field "lat" (float64 40.7128),
         field "lon" (float64 (-74.0060))])
      (tyapp (project testTypeLatLonPolyName (Name "lon")) Types.float64 @@
       tyapp (record testTypeLatLonPolyName [
         field "lat" (float64 40.7128),
         field "lon" (float64 (-74.0060))]) Types.float64)
      Types.float64
    expectTermWithType "project head from BuddyListA with string"
      (project testTypeBuddyListAName (Name "head") @@
       record testTypeBuddyListAName [
         field "head" (string "Alice"),
         field "tail" (nothing)])
      (tyapp (project testTypeBuddyListAName (Name "head")) Types.string @@
       tyapp (record testTypeBuddyListAName [
         field "head" (string "Alice"),
         field "tail" (tyapp nothing (Types.apply (Types.var "BuddyListB") Types.string))]) Types.string)
      Types.string

  H.describe "Record projections with variables" $ do
    expectTermWithType "project from lambda parameter"
      (lambda "person" $ project testTypePersonName (Name "firstName") @@ var "person")
      (lambdaTyped "person" (Types.var "Person") $ project testTypePersonName (Name "firstName") @@ var "person")
      (Types.function (Types.var "Person") Types.string)
    expectTermWithType "project from polymorphic lambda parameter"
      (lambda "coords" $ project testTypeLatLonPolyName (Name "lat") @@ var "coords")
      (tylam "t0" $ lambdaTyped "coords" (Types.apply (Types.var "LatLonPoly") (Types.var "t0")) $ tyapp (project testTypeLatLonPolyName (Name "lat")) (Types.var "t0") @@ var "coords")
      (Types.forAll "t0" $ Types.function (Types.apply (Types.var "LatLonPoly") (Types.var "t0")) (Types.var "t0"))
    expectTermWithType "multiple projections from same record"
      (lambda "person" $
       tuple [project testTypePersonName (Name "firstName") @@ var "person",
              project testTypePersonName (Name "lastName") @@ var "person"])
      (lambdaTyped "person" (Types.var "Person") $
       tuple [project testTypePersonName (Name "firstName") @@ var "person",
              project testTypePersonName (Name "lastName") @@ var "person"])
      (Types.function (Types.var "Person") (Types.product [Types.string, Types.string]))

  H.describe "Record projections in complex contexts" $ do
    expectTermWithType "projection in let binding"
      (lets ["person">: record testTypePersonName [
               field "firstName" (string "Charlie"),
               field "lastName" (string "Brown"),
               field "age" (int32 35)],
             "getName">: project testTypePersonName (Name "firstName")] $
            var "getName" @@ var "person")
      (letsTyped [("person", record testTypePersonName [
                     field "firstName" (string "Charlie"),
                     field "lastName" (string "Brown"),
                     field "age" (int32 35)],
                   Types.mono $ Types.var "Person"),
                  ("getName", project testTypePersonName (Name "firstName"),
                   Types.mono $ Types.function (Types.var "Person") Types.string)] $
        var "getName" @@ var "person")
      Types.string
    expectSameTermWithType "projection in tuple"
      (tuple [project testTypePersonName (Name "firstName"),
              project testTypePersonName (Name "age")])
      (Types.product [Types.function (Types.var "Person") Types.string,
                      Types.function (Types.var "Person") Types.int32])
    expectSameTermWithType "projection in list"
      (list [project testTypePersonName (Name "firstName"),
             project testTypePersonName (Name "lastName")])
      (Types.list (Types.function (Types.var "Person") Types.string))

  H.describe "Multi-parameter polymorphic projections" $ do
    expectTermWithType "project first from Triple"
      (project testTypeTripleName (Name "first"))
      (tylams ["t0", "t1", "t2"] $ tyapps (project testTypeTripleName (Name "first")) [Types.var "t0", Types.var "t1", Types.var "t2"])
      (Types.forAlls ["t0", "t1", "t2"] $
        Types.function
          (Types.applys (Types.var "Triple") [Types.var "t0", Types.var "t1", Types.var "t2"])
          (Types.var "t0"))
    expectTermWithType "project second from Triple applied"
      (project testTypeTripleName (Name "second") @@
        record testTypeTripleName [
          field "first" (int32 1),
          field "second" (string "middle"),
          field "third" (boolean True)])
      (tyapps (project testTypeTripleName (Name "second")) [Types.int32, Types.string, Types.boolean] @@
        tyapps (record testTypeTripleName [
          field "first" (int32 1),
          field "second" (string "middle"),
          field "third" (boolean True)]) [Types.int32, Types.string, Types.boolean])
      Types.string
    expectTermWithType "project from Triple and use second field, which is another polymorphic record"
        (lambda "triple" $ lambda "key" $
          match testTypePersonOrSomethingName Nothing [
            "person">: lambda "p" $ nothing,
            "other">: lambda "m" $
              primitive _maps_lookup @@ var "key" @@ var "m"] @@
          (project testTypeTripleName (Name "second") @@ var "triple"))
        (tylams ["t0", "t1", "t2", "t3"] $
          lambdaTyped "triple"
            (Types.applys (Types.var "Triple")
              [Types.var "t0",
               Types.apply (Types.var "PersonOrSomething") (Types.map (Types.var "t1") (Types.var "t2")),
               Types.var "t3"]) $
          lambdaTyped "key" (Types.var "t1") $
          tyapp (match testTypePersonOrSomethingName Nothing [
            "person">: lambdaTyped "p" (Types.var "Person") (tyapp nothing (Types.var "t2")),
            "other">: lambdaTyped "m" (Types.map (Types.var "t1") (Types.var "t2")) $
              tyapps (primitive _maps_lookup) [Types.var "t1", Types.var "t2"] @@ var "key" @@ var "m"]) (Types.map (Types.var "t1") (Types.var "t2")) @@
          (tyapps (project testTypeTripleName (Name "second"))
            [Types.var "t0",
             Types.apply (Types.var "PersonOrSomething") (Types.map (Types.var "t1") (Types.var "t2")),
             Types.var "t3"] @@ var "triple"))
        (Types.forAlls ["t0", "t1", "t2", "t3"] $
          Types.function
            (Types.applys (Types.var "Triple")
              [Types.var "t0",
               Types.apply (Types.var "PersonOrSomething") (Types.map (Types.var "t1") (Types.var "t2")),
               Types.var "t3"])
            (Types.function (Types.var "t1") (Types.optional (Types.var "t2"))))

  H.describe "Higher-order record projections" $ do
    expectTermWithType "map projection over list of records"
      (primitive _lists_map @@ (project testTypePersonName (Name "firstName")) @@
       list [record testTypePersonName [
               field "firstName" (string "Alice"),
               field "lastName" (string "Smith"),
               field "age" (int32 30)],
             record testTypePersonName [
               field "firstName" (string "Bob"),
               field "lastName" (string "Jones"),
               field "age" (int32 25)]])
      (tyapps (primitive _lists_map) [Types.var "Person", Types.string] @@ (project testTypePersonName (Name "firstName")) @@
       list [record testTypePersonName [
               field "firstName" (string "Alice"),
               field "lastName" (string "Smith"),
               field "age" (int32 30)],
             record testTypePersonName [
               field "firstName" (string "Bob"),
               field "lastName" (string "Jones"),
               field "age" (int32 25)]])
      (Types.list Types.string)
    expectTermWithType "map polymorphic projection"
      (primitive _lists_map @@ (project testTypeLatLonPolyName (Name "lat")) @@
       list [record testTypeLatLonPolyName [
               field "lat" (int32 40),
               field "lon" (int32 (-74))],
             record testTypeLatLonPolyName [
               field "lat" (int32 34),
               field "lon" (int32 (-118))]])
      (tyapps (primitive _lists_map) [Types.apply (Types.var "LatLonPoly") Types.int32, Types.int32]
        @@ (tyapp (project testTypeLatLonPolyName (Name "lat")) Types.int32) @@
       list [tyapp (record testTypeLatLonPolyName [
               field "lat" (int32 40),
               field "lon" (int32 (-74))]) Types.int32,
             tyapp (record testTypeLatLonPolyName [
               field "lat" (int32 34),
               field "lon" (int32 (-118))]) Types.int32])
      (Types.list Types.int32)
    expectTermWithType "filter using projection"
        (primitive _lists_filter @@
         (lambda "person" $
          primitive _equality_gt @@
          (project testTypePersonName (Name "age") @@ var "person") @@
          int32 30) @@
         list [record testTypePersonName [
                 field "firstName" (string "Alice"),
                 field "lastName" (string "Smith"),
                 field "age" (int32 35)],
               record testTypePersonName [
                 field "firstName" (string "Bob"),
                 field "lastName" (string "Jones"),
                 field "age" (int32 25)]])
        (tyapp (primitive _lists_filter) (Types.var "Person") @@
         (lambdaTyped "person" (Types.var "Person") $
          tyapp (primitive _equality_gt) Types.int32 @@
          (project testTypePersonName (Name "age") @@ var "person") @@
          int32 30) @@
         list [record testTypePersonName [
                 field "firstName" (string "Alice"),
                 field "lastName" (string "Smith"),
                 field "age" (int32 35)],
               record testTypePersonName [
                 field "firstName" (string "Bob"),
                 field "lastName" (string "Jones"),
                 field "age" (int32 25)]])
        (Types.list (Types.var "Person"))

  H.describe "Recursive record projections" $ do
    expectSameTermWithType "project head from IntList"
      (project testTypeIntListName (Name "head"))
      (Types.function (Types.var "IntList") Types.int32)
    expectSameTermWithType "project tail from IntList"
      (project testTypeIntListName (Name "tail"))
      (Types.function (Types.var "IntList") (Types.optional (Types.var "IntList")))
    expectTermWithType "project head applied to IntList"
      (project testTypeIntListName (Name "head") @@
       record testTypeIntListName [
         field "head" (int32 42),
         field "tail" (nothing)])
      (project testTypeIntListName (Name "head") @@
       record testTypeIntListName [
         field "head" (int32 42),
         field "tail" (tyapp nothing (Types.var "IntList"))])
      Types.int32
    expectTermWithType "nested projection from recursive record"
      (lambda "intList" $
       primitive _optionals_maybe @@
       int32 0 @@
       (project testTypeIntListName (Name "head")) @@
       (project testTypeIntListName (Name "tail") @@ var "intList"))
      (lambdaTyped "intList" (Types.var "IntList") $
       tyapps (primitive _optionals_maybe) [Types.int32, Types.var "IntList"] @@
       int32 0 @@
       (project testTypeIntListName (Name "head")) @@
       (project testTypeIntListName (Name "tail") @@ var "intList"))
      (Types.function (Types.var "IntList") Types.int32)

  H.describe "Record projections with mutual recursion" $ do
    expectTermWithType "project head from BuddyListA"
      (project testTypeBuddyListAName (Name "head"))
      (tylam "t0" $ tyapp (project testTypeBuddyListAName (Name "head")) (Types.var "t0"))
      (Types.forAll "t0" $ Types.function (Types.apply (Types.var "BuddyListA") (Types.var "t0")) (Types.var "t0"))
    expectTermWithType "project tail from BuddyListB"
      (project testTypeBuddyListBName (Name "tail"))
      (tylam "t0" $ tyapp (project testTypeBuddyListBName (Name "tail")) (Types.var "t0"))
      (Types.forAll "t0" $ Types.function
        (Types.apply (Types.var "BuddyListB") (Types.var "t0"))
        (Types.optional (Types.apply (Types.var "BuddyListA") (Types.var "t0"))))
    expectTermWithType "chained projections across mutual recursion"
        (lambda "listA" $
          primitive _optionals_maybe
            @@ nothing
            @@ (lambda "listB" $
              primitive _optionals_maybe
                @@ nothing
                @@ (project testTypeBuddyListAName (Name "tail"))
                @@ (project testTypeBuddyListBName (Name "tail") @@ var "listB"))
            @@ (project testTypeBuddyListAName (Name "tail") @@ var "listA"))
        (tylam "t0" $ lambdaTyped "listA" (Types.apply (Types.var "BuddyListA") (Types.var "t0")) $
          tyapps (primitive _optionals_maybe) [Types.optional (Types.apply (Types.var "BuddyListB") (Types.var "t0")), Types.apply (Types.var "BuddyListB") (Types.var "t0")] @@
            tyapp nothing (Types.apply (Types.var "BuddyListB") (Types.var "t0")) @@
            (lambdaTyped "listB" (Types.apply (Types.var "BuddyListB") (Types.var "t0")) $
              tyapps (primitive _optionals_maybe) [Types.optional (Types.apply (Types.var "BuddyListB") (Types.var "t0")), Types.apply (Types.var "BuddyListA") (Types.var "t0")] @@
                tyapp nothing (Types.apply (Types.var "BuddyListB") (Types.var "t0")) @@
                (tyapp (project testTypeBuddyListAName (Name "tail")) (Types.var "t0")) @@
                (tyapp (project testTypeBuddyListBName (Name "tail")) (Types.var "t0") @@ var "listB")) @@
            (tyapp (project testTypeBuddyListAName (Name "tail")) (Types.var "t0") @@ var "listA"))
        (Types.forAll "t0" $ Types.function
          (Types.apply (Types.var "BuddyListA") (Types.var "t0"))
          (Types.optional (Types.apply (Types.var "BuddyListB") (Types.var "t0"))))

    H.describe "Record projection composition" $ do
      expectTermWithType "compose projections"
        (lambda "person" $
         primitive _strings_cat2 @@
         (project testTypePersonName (Name "firstName") @@ var "person") @@
         (project testTypePersonName (Name "lastName") @@ var "person"))
        (lambdaTyped "person" (Types.var "Person") $
         primitive _strings_cat2 @@
         (project testTypePersonName (Name "firstName") @@ var "person") @@
         (project testTypePersonName (Name "lastName") @@ var "person"))
        (Types.function (Types.var "Person") Types.string)
      expectTermWithType "projection with arithmetic"
        (lambda "person" $
         primitive _math_add @@
         (project testTypePersonName (Name "age") @@ var "person") @@
         int32 1)
        (lambdaTyped "person" (Types.var "Person") $
         primitive _math_add @@
         (project testTypePersonName (Name "age") @@ var "person") @@
         int32 1)
        (Types.function (Types.var "Person") Types.int32)
      expectTermWithType "record construction using string projection"
       (lambda "person" $
        record testTypePersonName [
          field "firstName" (project testTypePersonName (Name "firstName") @@ var "person"),
          field "lastName" (project testTypePersonName (Name "lastName") @@ var "person"),
          field "age" (int32 0)])
       (lambdaTyped "person" (Types.var "Person") $
        record testTypePersonName [
          field "firstName" (project testTypePersonName (Name "firstName") @@ var "person"),
          field "lastName" (project testTypePersonName (Name "lastName") @@ var "person"),
          field "age" (int32 0)])
       (Types.function (Types.var "Person") (Types.var "Person"))

checkTypeOfSets :: H.SpecWith ()
checkTypeOfSets = H.describe "Sets" $ do
  H.describe "Monomorphic sets" $ do
    expectTermWithType "empty set"
      (Terms.set S.empty)
      (tylam "t0" $ tyapp (Terms.set S.empty) (Types.var "t0"))
      (Types.forAll "t0" $ Types.set $ Types.var "t0")
    expectSameTermWithType "int set"
      (Terms.set $ S.fromList [int32 1, int32 2, int32 3])
      (Types.set Types.int32)
    expectSameTermWithType "string set"
      (Terms.set $ S.fromList [string "apple", string "banana", string "cherry"])
      (Types.set Types.string)
    expectSameTermWithType "single element set"
      (Terms.set $ S.singleton $ boolean True)
      (Types.set Types.boolean)

  H.describe "Polymorphic sets" $ do
    expectTermWithType "set from lambda"
      (lambda "x" $ Terms.set $ S.singleton $ var "x")
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ Terms.set $ S.singleton $ var "x")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.set $ Types.var "t0"))
    expectTermWithType "set with repeated variable"
      (lambda "x" $ Terms.set $ S.fromList [var "x", var "x"])
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ Terms.set $ S.fromList [var "x", var "x"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.set $ Types.var "t0"))
    expectTermWithType "set from two variables"
      (lambda "x" $ lambda "y" $ Terms.set $ S.fromList [var "x", var "y"])
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ lambdaTyped "y" (Types.var "t0") $ Terms.set $ S.fromList [var "x", var "y"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.function (Types.var "t0") (Types.set $ Types.var "t0")))

  H.describe "Sets in complex contexts" $ do
    expectSameTermWithType "set in tuple"
      (tuple [Terms.set $ S.fromList [int32 1, int32 2], string "context"])
      (Types.product [Types.set Types.int32, Types.string])
    expectTermWithType "set in let binding"
      (lets ["numbers">: Terms.set $ S.fromList [int32 10, int32 20, int32 30]] $
            var "numbers")
      (letsTyped [("numbers", Terms.set $ S.fromList [int32 10, int32 20, int32 30],
                   Types.mono $ Types.set Types.int32)] $
        var "numbers")
      (Types.set Types.int32)

  H.describe "Nested sets" $ do
    expectSameTermWithType "set of lists"
      (Terms.set $ S.fromList [
        list [string "a", string "b"],
        list [string "c", string "d"]])
      (Types.set $ Types.list Types.string)
    expectSameTermWithType "set of tuples"
      (Terms.set $ S.fromList [
        tuple [int32 1, int32 2],
        tuple [int32 3, int32 4]])
      (Types.set $ Types.product [Types.int32, Types.int32])
    expectSameTermWithType "set of sets"
      (Terms.set $ S.singleton $ Terms.set $ S.fromList [string "nested"])
      (Types.set $ Types.set Types.string)

  H.describe "Sets with complex types" $ do
    expectSameTermWithType "set of records"
      (Terms.set $ S.singleton $ record testTypePersonName [
        field "firstName" (string "Alice"),
        field "lastName" (string "Smith"),
        field "age" (int32 30)])
      (Types.set $ Types.var "Person")
    expectTermWithType "set of optionals"
      (Terms.set $ S.fromList [
        just $ int32 42,
        nothing])
      (Terms.set $ S.fromList [
        just $ int32 42,
        tyapp nothing Types.int32])
      (Types.set $ Types.optional Types.int32)
    expectSameTermWithType "set of maps"
      (Terms.set $ S.singleton $ Terms.map $ M.singleton (string "key") (int32 42))
      (Types.set $ Types.map Types.string Types.int32)

checkTypeOfSums :: H.SpecWith ()
checkTypeOfSums = H.describe "Sums" $ do
  -- TODO: we probably will not bother to test sum terms; see https://github.com/CategoricalData/hydra/issues/134
  return ()

checkTypeOfUnions :: H.SpecWith ()
checkTypeOfUnions = H.describe "Unions" $ do
  H.describe "Simple union injections" $ do
    expectSameTermWithType "inject into Comparison lessThan variant"
      (unitVariant testTypeComparisonName (Name "lessThan"))
      (Types.var "Comparison")
    expectSameTermWithType "inject into Comparison equalTo variant"
      (unitVariant testTypeComparisonName (Name "equalTo"))
      (Types.var "Comparison")
    expectSameTermWithType "inject into Comparison greaterThan variant"
      (unitVariant testTypeComparisonName (Name "greaterThan"))
      (Types.var "Comparison")

  H.describe "Union injections with data" $ do
    expectSameTermWithType "inject into Number int variant"
      (variant testTypeNumberName (Name "int") (int32 42))
      (Types.var "Number")
    expectSameTermWithType "inject into Number float variant"
      (variant testTypeNumberName (Name "float") (float32 3.14))
      (Types.var "Number")
    expectSameTermWithType "inject into Timestamp unixTimeMillis variant"
      (variant testTypeTimestampName (Name "unixTimeMillis") (uint64 1609459200000))
      (Types.var "Timestamp")
    expectSameTermWithType "inject into Timestamp date variant"
      (variant testTypeTimestampName (Name "date") (string "2021-01-01"))
      (Types.var "Timestamp")

  H.describe "Polymorphic union injections" $ do
    expectTermWithType "inject person into PersonOrSomething"
      (variant testTypePersonOrSomethingName (Name "person")
        (record testTypePersonName [
          field "firstName" (string "Alice"),
          field "lastName" (string "Smith"),
          field "age" (int32 30)]))
      (tylam "t0" $ tyapp (variant testTypePersonOrSomethingName (Name "person")
        (record testTypePersonName [
          field "firstName" (string "Alice"),
          field "lastName" (string "Smith"),
          field "age" (int32 30)])) (Types.var "t0"))
      (Types.forAll "t0" $ Types.apply (Types.var "PersonOrSomething") (Types.var "t0"))
    expectTermWithType "inject string into PersonOrSomething other variant"
      (variant testTypePersonOrSomethingName (Name "other") (string "something else"))
      (tyapp (variant testTypePersonOrSomethingName (Name "other") (string "something else")) Types.string)
      (Types.apply (Types.var "PersonOrSomething") Types.string)
    expectTermWithType "inject int into PersonOrSomething other variant"
      (variant testTypePersonOrSomethingName (Name "other") (int32 42))
      (tyapp (variant testTypePersonOrSomethingName (Name "other") (int32 42)) Types.int32)
      (Types.apply (Types.var "PersonOrSomething") Types.int32)

  H.describe "Polymorphic recursive union injections" $ do
    expectTermWithType "inject boolean into UnionPolymorphicRecursive"
      (variant testTypeUnionPolymorphicRecursiveName (Name "bool") (boolean True))
      (tylam "t0" $ tyapp (variant testTypeUnionPolymorphicRecursiveName (Name "bool") (boolean True)) (Types.var "t0"))
      (Types.forAll "t0" $ Types.apply (Types.var "UnionPolymorphicRecursive") (Types.var "t0"))
    expectTermWithType "inject string value into UnionPolymorphicRecursive"
      (variant testTypeUnionPolymorphicRecursiveName (Name "value") (string "test"))
      (tyapp (variant testTypeUnionPolymorphicRecursiveName (Name "value") (string "test")) Types.string)
      (Types.apply (Types.var "UnionPolymorphicRecursive") Types.string)
    expectTermWithType "inject int value into UnionPolymorphicRecursive"
      (variant testTypeUnionPolymorphicRecursiveName (Name "value") (int32 123))
      (tyapp (variant testTypeUnionPolymorphicRecursiveName (Name "value") (int32 123)) Types.int32)
      (Types.apply (Types.var "UnionPolymorphicRecursive") Types.int32)

  H.describe "Polymorphic unions from lambda" $ do
    expectTermWithType "lambda creating PersonOrSomething other variant"
      (lambda "x" $ variant testTypePersonOrSomethingName (Name "other") (var "x"))
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ tyapp (variant testTypePersonOrSomethingName (Name "other") (var "x")) (Types.var "t0"))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.apply (Types.var "PersonOrSomething") (Types.var "t0")))
    expectTermWithType "lambda creating UnionPolymorphicRecursive value variant"
      (lambda "x" $ variant testTypeUnionPolymorphicRecursiveName (Name "value") (var "x"))
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ tyapp (variant testTypeUnionPolymorphicRecursiveName (Name "value") (var "x")) (Types.var "t0"))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.apply (Types.var "UnionPolymorphicRecursive") (Types.var "t0")))

  H.describe "Unions in complex contexts" $ do
    expectSameTermWithType "union in tuple"
      (tuple [variant testTypeNumberName (Name "int") (int32 42),
              string "context"])
      (Types.product [Types.var "Number", Types.string])
    expectSameTermWithType "union in list"
      (list [variant testTypeNumberName (Name "int") (int32 1),
             variant testTypeNumberName (Name "float") (float32 2.5)])
      (Types.list $ Types.var "Number")
    expectTermWithType "polymorphic union in let binding"
      (lets ["value">: variant testTypePersonOrSomethingName (Name "other") (string "test")] $
            var "value")
      (letsTyped [("value", tyapp (variant testTypePersonOrSomethingName (Name "other") (string "test")) Types.string,
                   Types.mono $ Types.apply (Types.var "PersonOrSomething") Types.string)] $
        var "value")
      (Types.apply (Types.var "PersonOrSomething") Types.string)

  H.describe "Multi-parameter polymorphic injections" $ do
    expectTermWithType "either left with int"
      (variant testTypeEitherName (Name "left") (int32 42))
      (tylam "t0" $ tyapps (variant testTypeEitherName (Name "left") (int32 42)) [Types.int32, Types.var "t0"])
      (Types.forAll "t0" $ Types.applys (Types.var "Either") [Types.int32, Types.var "t0"])
    expectTermWithType "either right with string"
      (variant testTypeEitherName (Name "right") (string "hello"))
      (tylam "t0" $ tyapps (variant testTypeEitherName (Name "right") (string "hello")) [Types.var "t0", Types.string])
      (Types.forAll "t0" $ Types.applys (Types.var "Either") [Types.var "t0", Types.string])
    expectTermWithType "either containing LatLonPoly in list"
      (variant testTypeEitherName (Name "right")
        (list [record testTypeLatLonPolyName [
          field "lat" (int32 40),
          field "lon" (int32 (-74))]]))
      (tylam "t0" $ tyapps (variant testTypeEitherName (Name "right")
        (list [tyapp (record testTypeLatLonPolyName [
          field "lat" (int32 40),
          field "lon" (int32 (-74))]) Types.int32]))
        [Types.var "t0", Types.list (Types.apply (Types.var "LatLonPoly") Types.int32)])
      (Types.forAll "t0" $ Types.applys (Types.var "Either") [Types.var "t0", Types.list (Types.apply (Types.var "LatLonPoly") Types.int32)])
    expectTermWithType "either in triple in map with shared type variables"
      (lambda "x0" $ lambda "x1" $ lambda "x2" $
        Terms.map $ M.singleton (string "key") $
          record testTypeTripleName [
            field "first" (variant testTypeEitherName (Name "left") (var "x0")),
            field "second" (variant testTypeEitherName (Name "left") (var "x0")),
            field "third" (variant testTypeEitherName (Name "right") (var "x1"))])
      (tylams ["t0", "t1", "t2", "t3", "t4", "t5"] $
        lambdaTyped "x0" (Types.var "t0") $
        lambdaTyped "x1" (Types.var "t1") $
        lambdaTyped "x2" (Types.var "t2") $
        Terms.map $ M.singleton (string "key") $
          tyapps (record testTypeTripleName [
            field "first" (tyapps (variant testTypeEitherName (Name "left") (var "x0")) [Types.var "t0", Types.var "t3"]),
            field "second" (tyapps (variant testTypeEitherName (Name "left") (var "x0")) [Types.var "t0", Types.var "t4"]),
            field "third" (tyapps (variant testTypeEitherName (Name "right") (var "x1")) [Types.var "t5", Types.var "t1"])])
          [Types.applys (Types.var "Either") [Types.var "t0", Types.var "t3"],
           Types.applys (Types.var "Either") [Types.var "t0", Types.var "t4"],
           Types.applys (Types.var "Either") [Types.var "t5", Types.var "t1"]])
      (Types.forAlls ["t0", "t1", "t2", "t3", "t4", "t5"] $
        Types.function (Types.var "t0") $
        Types.function (Types.var "t1") $
        Types.function (Types.var "t2") $
        Types.map Types.string $
          Types.applys (Types.var "Triple")
            [Types.applys (Types.var "Either") [Types.var "t0", Types.var "t3"],
             Types.applys (Types.var "Either") [Types.var "t0", Types.var "t4"],
             Types.applys (Types.var "Either") [Types.var "t5", Types.var "t1"]])

checkTypeOfUnionEliminations :: H.SpecWith ()
checkTypeOfUnionEliminations = H.describe "Union eliminations" $ do
  H.describe "Simple unit variant eliminations" $ do
    expectTermWithType "match Comparison with all cases"
      (match testTypeComparisonName Nothing [
        "lessThan">: lambda "x" (string "less"),
        "equalTo">: lambda "x" (string "equal"),
        "greaterThan">: lambda "x" (string "greater")])
      (match testTypeComparisonName Nothing [
        "lessThan">: lambdaTyped "x" Types.unit (string "less"),
        "equalTo">: lambdaTyped "x" Types.unit (string "equal"),
        "greaterThan">: lambdaTyped "x" Types.unit (string "greater")])
      (Types.function (Types.var "Comparison") Types.string)
    expectTermWithType "match Comparison returning int32"
      (match testTypeComparisonName Nothing [
        "lessThan">: lambda "x" (int32 (-1)),
        "equalTo">: lambda "x" (int32 0),
        "greaterThan">: lambda "x" (int32 1)])
      (match testTypeComparisonName Nothing [
        "lessThan">: lambdaTyped "x" Types.unit (int32 (-1)),
        "equalTo">: lambdaTyped "x" Types.unit (int32 0),
        "greaterThan">: lambdaTyped "x" Types.unit (int32 1)])
      (Types.function (Types.var "Comparison") Types.int32)
    expectTermWithType "match applied to Comparison variant"
      (match testTypeComparisonName Nothing [
        "lessThan">: lambda "x" (string "less"),
        "equalTo">: lambda "x" (string "equal"),
        "greaterThan">: lambda "x" (string "greater")] @@
       unitVariant testTypeComparisonName (Name "equalTo"))
      (match testTypeComparisonName Nothing [
        "lessThan">: lambdaTyped "x" Types.unit (string "less"),
        "equalTo">: lambdaTyped "x" Types.unit (string "equal"),
        "greaterThan">: lambdaTyped "x" Types.unit (string "greater")] @@
       unitVariant testTypeComparisonName (Name "equalTo"))
      Types.string

  H.describe "Union eliminations with data" $ do
    expectTermWithType "match Number extracting int values"
      (match testTypeNumberName Nothing [
        "int">: lambda "i" (var "i"),
        "float">: lambda "f" (int32 0)])
      (match testTypeNumberName Nothing [
        "int">: lambdaTyped "i" Types.int32 (var "i"),
        "float">: lambdaTyped "f" Types.float32 (int32 0)])
      (Types.function (Types.var "Number") Types.int32)
    expectTermWithType "match Number converting to string"
      (match testTypeNumberName Nothing [
        "int">: lambda "i" (primitive _literals_showInt32 @@ var "i"),
        "float">: lambda "f" (primitive _literals_showFloat32 @@ var "f")])
      (match testTypeNumberName Nothing [
        "int">: lambdaTyped "i" Types.int32 (primitive _literals_showInt32 @@ var "i"),
        "float">: lambdaTyped "f" Types.float32 (primitive _literals_showFloat32 @@ var "f")])
      (Types.function (Types.var "Number") Types.string)
    expectTermWithType "match Number applied to int variant"
      (match testTypeNumberName Nothing [
        "int">: lambda "i" (primitive _math_add @@ var "i" @@ int32 10),
        "float">: lambda "f" (int32 0)] @@
       variant testTypeNumberName (Name "int") (int32 42))
      (match testTypeNumberName Nothing [
        "int">: lambdaTyped "i" Types.int32 (primitive _math_add @@ var "i" @@ int32 10),
        "float">: lambdaTyped "f" Types.float32 (int32 0)] @@
       variant testTypeNumberName (Name "int") (int32 42))
      Types.int32
    expectTermWithType "match Timestamp with mixed data types"
        (match testTypeTimestampName Nothing [
          "unixTimeMillis">: lambda "millis" (primitive _literals_showUint64 @@ var "millis"),
          "date">: lambda "dateStr" (var "dateStr")])
        (match testTypeTimestampName Nothing [
          "unixTimeMillis">: lambdaTyped "millis" Types.uint64 (primitive _literals_showUint64 @@ var "millis"),
          "date">: lambdaTyped "dateStr" Types.string (var "dateStr")])
        (Types.function (Types.var "Timestamp") Types.string)

  H.describe "Polymorphic union eliminations" $ do
    expectTermWithType "match PersonOrSomething with string"
      (match testTypePersonOrSomethingName Nothing [
        "person">: lambda "p" (project testTypePersonName (Name "firstName") @@ var "p"),
        "other">: lambda "x" (var "x")])
      (tyapp (match testTypePersonOrSomethingName Nothing [
        "person">: lambdaTyped "p" (Types.var "Person") (project testTypePersonName (Name "firstName") @@ var "p"),
        "other">: lambdaTyped "x" Types.string (var "x")]) Types.string)
      (Types.function (Types.apply (Types.var "PersonOrSomething") (Types.string)) (Types.string))
    expectTermWithType "match PersonOrSomething instantiated with string"
      (match testTypePersonOrSomethingName Nothing [
        "person">: lambda "p" (project testTypePersonName (Name "firstName") @@ var "p"),
        "other">: lambda "x" (var "x")] @@
       variant testTypePersonOrSomethingName (Name "other") (string "test"))
      (tyapp (match testTypePersonOrSomethingName Nothing [
        "person">: lambdaTyped "p" (Types.var "Person") (project testTypePersonName (Name "firstName") @@ var "p"),
        "other">: lambdaTyped "x" Types.string (var "x")]) Types.string @@
       tyapp (variant testTypePersonOrSomethingName (Name "other") (string "test")) Types.string)
      Types.string

  H.describe "Union eliminations with defaults" $ do
    expectTermWithType "match Comparison with default case"
      (match testTypeComparisonName (Just (string "unknown")) [
        "lessThan">: lambda "x" (string "less"),
        "equalTo">: lambda "x" (string "equal")])
      (match testTypeComparisonName (Just (string "unknown")) [
        "lessThan">: lambdaTyped "x" Types.unit (string "less"),
        "equalTo">: lambdaTyped "x" Types.unit (string "equal")])
      (Types.function (Types.var "Comparison") Types.string)
    expectTermWithType "match Number with default case"
      (match testTypeNumberName (Just (int32 (-1))) [
        "int">: lambda "i" (var "i")])
      (match testTypeNumberName (Just (int32 (-1))) [
        "int">: lambdaTyped "i" Types.int32 (var "i")])
      (Types.function (Types.var "Number") Types.int32)
    expectTermWithType "match UnionMonomorphic with default"
      (match testTypeUnionMonomorphicName (Just (string "fallback")) [
        "bool">: lambda "b" (primitive _literals_showBoolean @@ var "b"),
        "string">: lambda "s" (var "s")])
      (match testTypeUnionMonomorphicName (Just (string "fallback")) [
        "bool">: lambdaTyped "b" Types.boolean (primitive _literals_showBoolean @@ var "b"),
        "string">: lambdaTyped "s" Types.string (var "s")])
      (Types.function (Types.var "UnionMonomorphic") Types.string)

  H.describe "Nested union eliminations" $ do
    expectTermWithType "nested match statements"
      (match testTypePersonOrSomethingName Nothing [
        "person">: lambda "p" (project testTypePersonName (Name "firstName") @@ var "p"),
        "other">: lambda "x" (
          match testTypeNumberName Nothing [
            "int">: lambda "i" (primitive _literals_showInt32 @@ var "i"),
            "float">: lambda "f" (primitive _literals_showFloat32 @@ var "f")] @@
          var "x")])
      (tyapp (match testTypePersonOrSomethingName Nothing [
        "person">: lambdaTyped "p" (Types.var "Person") (project testTypePersonName (Name "firstName") @@ var "p"),
        "other">: lambdaTyped "x" (Types.var "Number") (
          match testTypeNumberName Nothing [
            "int">: lambdaTyped "i" Types.int32 (primitive _literals_showInt32 @@ var "i"),
            "float">: lambdaTyped "f" Types.float32 (primitive _literals_showFloat32 @@ var "f")] @@
          var "x")]) (Types.var "Number"))
      (Types.function (Types.apply (Types.var "PersonOrSomething") (Types.var "Number")) Types.string)
    expectTermWithType "match in tuple"
      (tuple [
        match testTypeComparisonName Nothing [
          "lessThan">: lambda "x" (int32 1),
          "equalTo">: lambda "x" (int32 0),
          "greaterThan">: lambda "x" (int32 (-1))],
        string "context"])
      (tuple [
        match testTypeComparisonName Nothing [
          "lessThan">: lambdaTyped "x" Types.unit (int32 1),
          "equalTo">: lambdaTyped "x" Types.unit (int32 0),
          "greaterThan">: lambdaTyped "x" Types.unit (int32 (-1))],
        string "context"])
      (Types.product [Types.function (Types.var "Comparison") Types.int32, Types.string])

  H.describe "Union eliminations in complex contexts" $ do
    expectTermWithType "match in let binding"
      (lets ["matcher">: match testTypeComparisonName Nothing [
               "lessThan">: lambda "x" (string "less"),
               "equalTo">: lambda "x" (string "equal"),
               "greaterThan">: lambda "x" (string "greater")]] $
            var "matcher")
      (letsTyped [("matcher", match testTypeComparisonName Nothing [
                     "lessThan">: lambdaTyped "x" Types.unit (string "less"),
                     "equalTo">: lambdaTyped "x" Types.unit (string "equal"),
                     "greaterThan">: lambdaTyped "x" Types.unit (string "greater")],
                   Types.mono $ Types.function (Types.var "Comparison") Types.string)] $
        var "matcher")
      (Types.function (Types.var "Comparison") Types.string)
    expectTermWithType "match in record"
      (record testTypePersonName [
        field "firstName" (match testTypePersonOrSomethingName Nothing [
          "person">: lambda "p" (project testTypePersonName (Name "firstName") @@ var "p"),
          "other">: lambda "x" (var "x")] @@
         variant testTypePersonOrSomethingName (Name "other") (string "John")),
        field "lastName" (string "Doe"),
        field "age" (int32 30)])
      (record testTypePersonName [
        field "firstName" (tyapp (match testTypePersonOrSomethingName Nothing [
          "person">: lambdaTyped "p" (Types.var "Person") (project testTypePersonName (Name "firstName") @@ var "p"),
          "other">: lambdaTyped "x" Types.string (var "x")]) Types.string @@
         tyapp (variant testTypePersonOrSomethingName (Name "other") (string "John")) Types.string),
        field "lastName" (string "Doe"),
        field "age" (int32 30)])
      (Types.var "Person")
    expectTermWithType "match with polymorphic result in list"
      (list [
        match testTypePersonOrSomethingName Nothing [
          "person">: lambda "p" (project testTypePersonName (Name "age") @@ var "p"),
          "other">: lambda "x" (var "x")] @@
        variant testTypePersonOrSomethingName (Name "other") (int32 25),
        int32 30])
      (list [
        tyapp (match testTypePersonOrSomethingName Nothing [
          "person">: lambdaTyped "p" (Types.var "Person") (project testTypePersonName (Name "age") @@ var "p"),
          "other">: lambdaTyped "x" Types.int32 (var "x")]) Types.int32 @@
        tyapp (variant testTypePersonOrSomethingName (Name "other") (int32 25)) Types.int32,
        int32 30])
      (Types.list Types.int32)

  H.describe "Multi-parameter polymorphic case statements" $ do
    expectTermWithType "case Either converting both to string"
      (match testTypeEitherName Nothing [
        "left">: lambda "x" $ primitive _literals_showInt32 @@ var "x",
        "right">: lambda "y" $ primitive _literals_showFloat32 @@ var "y"])
      (tyapps (match testTypeEitherName Nothing [
        "left">: lambdaTyped "x" Types.int32 (primitive _literals_showInt32 @@ var "x"),
        "right">: lambdaTyped "y" Types.float32 (primitive _literals_showFloat32 @@ var "y")]) [Types.int32, Types.float32])
      (Types.function
        (Types.applys (Types.var "Either") [Types.int32, Types.float32])
        Types.string)
    expectTermWithType "case Either applied to injection"
      (match testTypeEitherName Nothing [
        "left">: lambda "n" $ primitive _math_add @@ var "n" @@ int32 1,
        "right">: lambda "s" $ primitive _strings_length @@ var "s"] @@
       variant testTypeEitherName (Name "left") (int32 42))
      (tyapps (match testTypeEitherName Nothing [
        "left">: lambdaTyped "n" Types.int32 (primitive _math_add @@ var "n" @@ int32 1),
        "right">: lambdaTyped "s" Types.string (primitive _strings_length @@ var "s")]) [Types.int32, Types.string] @@
       tyapps (variant testTypeEitherName (Name "left") (int32 42)) [Types.int32, Types.string])
      Types.int32
    expectTermWithType "case Either with Triple and nested projections"
        (lambda "triple" $
          match testTypeEitherName Nothing [
            "left">: lambda "coords" $
              project testTypeLatLonPolyName (Name "lat") @@ var "coords",
            "right">: lambda "t" $
              project testTypeTripleName (Name "first") @@ var "t"] @@
          (project testTypeTripleName (Name "second") @@ var "triple"))
        (tylams ["t0", "t1", "t2", "t3", "t4"] $
          lambdaTyped "triple"
            (Types.applys (Types.var "Triple")
              [Types.var "t0",
               Types.applys (Types.var "Either")
                 [Types.apply (Types.var "LatLonPoly") (Types.var "t1"),
                  Types.applys (Types.var "Triple") [Types.var "t1", Types.var "t2", Types.var "t3"]],
               Types.var "t4"]) $
          tyapps (match testTypeEitherName Nothing [
            "left">: lambdaTyped "coords" (Types.apply (Types.var "LatLonPoly") (Types.var "t1")) $
              tyapp (project testTypeLatLonPolyName (Name "lat")) (Types.var "t1") @@ var "coords",
            "right">: lambdaTyped "t" (Types.applys (Types.var "Triple") [Types.var "t1", Types.var "t2", Types.var "t3"]) $
              tyapps (project testTypeTripleName (Name "first")) [Types.var "t1", Types.var "t2", Types.var "t3"] @@ var "t"])
            [Types.apply (Types.var "LatLonPoly") (Types.var "t1"),
             Types.applys (Types.var "Triple") [Types.var "t1", Types.var "t2", Types.var "t3"]] @@
          (tyapps (project testTypeTripleName (Name "second"))
            [Types.var "t0",
             Types.applys (Types.var "Either")
               [Types.apply (Types.var "LatLonPoly") (Types.var "t1"),
                Types.applys (Types.var "Triple") [Types.var "t1", Types.var "t2", Types.var "t3"]],
             Types.var "t4"] @@ var "triple"))
        (Types.forAlls ["t0", "t1", "t2", "t3", "t4"] $
          Types.function
            (Types.applys (Types.var "Triple")
              [Types.var "t0",
               Types.applys (Types.var "Either")
                 [Types.apply (Types.var "LatLonPoly") (Types.var "t1"),
                  Types.applys (Types.var "Triple") [Types.var "t1", Types.var "t2", Types.var "t3"]],
               Types.var "t4"])
            (Types.var "t1"))
    expectTermWithType "case Either with polymorphic let bindings"
        (lets ["makeLeft">: lambda "x" $ variant testTypeEitherName (Name "left") (var "x"),
               "makeRight">: lambda "y" $ variant testTypeEitherName (Name "right") (var "y")] $
          lambda "flag" $
            match testTypeEitherName Nothing [
              "left">: lambda "n" $ var "makeRight" @@ (primitive _math_add @@ var "n" @@ int32 10),
              "right">: lambda "s" $ var "makeLeft" @@ (primitive _strings_length @@ var "s")] @@
            var "flag")
        (letsTyped [("makeLeft", tylams ["t0", "t1"] $ lambdaTyped "x" (Types.var "t0") $ tyapps (variant testTypeEitherName (Name "left") (var "x")) [Types.var "t0", Types.var "t1"],
                     Types.poly ["t0", "t1"] $ Types.function (Types.var "t0") (Types.applys (Types.var "Either") [Types.var "t0", Types.var "t1"])),
                    ("makeRight", tylams ["t0", "t1"] $ lambdaTyped "y" (Types.var "t0") $ tyapps (variant testTypeEitherName (Name "right") (var "y")) [Types.var "t1", Types.var "t0"],
                     Types.poly ["t0", "t1"] $ Types.function (Types.var "t0") (Types.applys (Types.var "Either") [Types.var "t1", Types.var "t0"]))] $
          lambdaTyped "flag" (Types.applys (Types.var "Either") [Types.int32, Types.string]) $
            tyapps (match testTypeEitherName Nothing [
              "left">: lambdaTyped "n" Types.int32 $ tyapps (var "makeRight") [Types.int32, Types.int32] @@ (primitive _math_add @@ var "n" @@ int32 10),
              "right">: lambdaTyped "s" Types.string $ tyapps (var "makeLeft") [Types.int32, Types.int32] @@ (primitive _strings_length @@ var "s")]) [Types.int32, Types.string] @@
            var "flag")
        (Types.function (Types.applys (Types.var "Either") [Types.int32, Types.string]) (Types.applys (Types.var "Either") [Types.int32, Types.int32]))

  H.describe "Higher-order union eliminations" $ do
    expectTermWithType "map match over list"
      (primitive _lists_map @@
       (match testTypeComparisonName Nothing [
         "lessThan">: lambda "x" (string "less"),
         "equalTo">: lambda "x" (string "equal"),
         "greaterThan">: lambda "x" (string "greater")]) @@
       list [unitVariant testTypeComparisonName (Name "lessThan"),
             unitVariant testTypeComparisonName (Name "equalTo")])
      (tyapps (primitive _lists_map) [Types.var "Comparison", Types.string] @@
       (match testTypeComparisonName Nothing [
         "lessThan">: lambdaTyped "x" Types.unit (string "less"),
         "equalTo">: lambdaTyped "x" Types.unit (string "equal"),
         "greaterThan">: lambdaTyped "x" Types.unit (string "greater")]) @@
       list [unitVariant testTypeComparisonName (Name "lessThan"),
             unitVariant testTypeComparisonName (Name "equalTo")])
      (Types.list Types.string)

    expectTermWithType "compose match with other functions"
      (lambda "comp" $
       primitive _strings_length @@
       (match testTypeComparisonName Nothing [
         "lessThan">: lambda "x" (string "less"),
         "equalTo">: lambda "x" (string "equal"),
         "greaterThan">: lambda "x" (string "greater")] @@
        var "comp"))
      (lambdaTyped "comp" (Types.var "Comparison") $
       primitive _strings_length @@
       (match testTypeComparisonName Nothing [
         "lessThan">: lambdaTyped "x" Types.unit (string "less"),
         "equalTo">: lambdaTyped "x" Types.unit (string "equal"),
         "greaterThan">: lambdaTyped "x" Types.unit (string "greater")] @@
        var "comp"))
      (Types.function (Types.var "Comparison") Types.int32)

    expectTermWithType "match in lambda body"
      (lambda "unionValue" $
       match testTypeNumberName Nothing [
         "int">: lambda "i" (primitive _math_add @@ var "i" @@ int32 1),
         "float">: lambda "f" (int32 0)] @@
       var "unionValue")
      (lambdaTyped "unionValue" (Types.var "Number") $
       match testTypeNumberName Nothing [
         "int">: lambdaTyped "i" Types.int32 (primitive _math_add @@ var "i" @@ int32 1),
         "float">: lambdaTyped "f" Types.float32 (int32 0)] @@
       var "unionValue")
      (Types.function (Types.var "Number") Types.int32)

  H.describe "Recursive union eliminations" $ do
    expectTermWithType "match HydraType recursively"
      (match testTypeHydraTypeName Nothing [
        "literal">: lambda "lit" (
          match testTypeHydraLiteralTypeName Nothing [
            "boolean">: lambda "b" (primitive _literals_showBoolean @@ var "b"),
            "string">: lambda "s" (var "s")] @@
          var "lit"),
        "list">: lambda "nested" (string "list")])
      (match testTypeHydraTypeName Nothing [
        "literal">: lambdaTyped "lit" (Types.var "HydraLiteralType") (
          match testTypeHydraLiteralTypeName Nothing [
            "boolean">: lambdaTyped "b" Types.boolean (primitive _literals_showBoolean @@ var "b"),
            "string">: lambdaTyped "s" Types.string (var "s")] @@
          var "lit"),
        "list">: lambdaTyped "nested" (Types.var "HydraType") (string "list")])
      (Types.function (Types.var "HydraType") Types.string)

checkTypeOfUnit :: H.SpecWith ()
checkTypeOfUnit = H.describe "Unit" $ do
  H.describe "Unit term" $ do
    expectSameTermWithType "unit literal"
      unit
      Types.unit
  H.describe "Unit term in polymorphic context" $ do
    expectTermWithType "unit from lambda"
      (lambda "x" unit)
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") unit)
      (Types.forAll "t0" $ Types.function (Types.var "t0") Types.unit)
    expectSameTermWithType "unit in tuple"
      (tuple [unit, string "foo"])
      (Types.product [Types.unit, Types.string])

checkTypeOfVariables :: H.SpecWith ()
checkTypeOfVariables = H.describe "Variables" $ do
  H.describe "Simple variable lookup" $ do
    expectTermWithType "int variable"
      (lambda "x" $ var "x")
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ var "x")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))
    expectTermWithType "variable in let binding"
      (lets ["x">: int32 42] $ var "x")
      (letsTyped [("x", int32 42, Types.mono Types.int32)] $ var "x")
      Types.int32
    expectTermWithType "multiple variables"
      (lets ["x">: string "hello",
             "y">: int32 42] $
            tuple [var "x", var "y"])
      (letsTyped [("x", string "hello", Types.mono Types.string),
                  ("y", int32 42, Types.mono Types.int32)] $
        tuple [var "x", var "y"])
      (Types.product [Types.string, Types.int32])

  H.describe "Variable scoping" $ do
    expectTermWithType "lambda parameter"
      (lambda "x" $ lambda "y" $ var "x")
      (tylams ["t0", "t1"] $ lambdaTyped "x" (Types.var "t0") $ lambdaTyped "y" (Types.var "t1") $ var "x")
      (Types.forAlls ["t0", "t1"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.var "t0")))
    expectTermWithType "let binding scope"
      (lets ["x">: int32 1] $
       lets ["y">: string "hello"] $
            var "x")
      (letsTyped [("x", int32 1, Types.mono Types.int32)] $
       letsTyped [("y", string "hello", Types.mono Types.string)] $
        var "x")
      Types.int32
    expectTermWithType "variable shadowing"
      (lets ["x">: int32 1] $
       lambda "x" $ var "x")
      (tylam "t0" $ letsTyped [("x", int32 1, Types.mono Types.int32)] $
        lambdaTyped "x" (Types.var "t0") $ var "x")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))
    expectTermWithType "nested scoping"
      (lambda "x" $
       lets ["y">: var "x"] $
            lambda "z" $
            tuple [var "x", var "y", var "z"])
      (tylams ["t0", "t1"] $ lambdaTyped "x" (Types.var "t0") $
       letsTyped [("y", var "x", Types.mono (Types.var "t0"))] $
        lambdaTyped "z" (Types.var "t1") $
        tuple [var "x", var "y", var "z"])
      (Types.forAlls ["t0", "t1"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.product [Types.var "t0", Types.var "t0", Types.var "t1"])))

  H.describe "Polymorphic variables" $ do
    expectTermWithType "polymorphic function"
      (lets ["id">: lambda "x" $ var "x"] $
            var "id")
      (tylam "t0" $ letsTyped [("id", tylam "t1" $ lambdaTyped "x" (Types.var "t1") $ var "x",
                                Types.poly ["t1"] $ Types.function (Types.var "t1") (Types.var "t1"))] $
        tyapp (var "id") (Types.var "t0"))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))
    expectTermWithType "polymorphic application"
      (lets ["id">: lambda "x" $ var "x"] $
            tuple [var "id" @@ int32 42, var "id" @@ string "test"])
      (letsTyped [("id", tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ var "x",
                   Types.poly ["t0"] $ Types.function (Types.var "t0") (Types.var "t0"))] $
        tuple [tyapp (var "id") Types.int32 @@ int32 42, tyapp (var "id") Types.string @@ string "test"])
      (Types.product [Types.int32, Types.string])
    expectTermWithType "higher order polymorphic"
      (lets ["apply">: lambda "f" $ lambda "x" $ var "f" @@ var "x"] $
            var "apply")
      (tylams ["t0", "t1"] $ letsTyped [("apply", tylams ["t2", "t3"] $ lambdaTyped "f" (Types.function (Types.var "t2") (Types.var "t3")) $ lambdaTyped "x" (Types.var "t2") $ var "f" @@ var "x",
                                         Types.poly ["t2", "t3"] $ Types.function (Types.function (Types.var "t2") (Types.var "t3")) (Types.function (Types.var "t2") (Types.var "t3")))] $
        tyapps (var "apply") [Types.var "t0", Types.var "t1"])
      (Types.forAlls ["t0", "t1"] $
        Types.function (Types.function (Types.var "t0") (Types.var "t1")) (Types.function (Types.var "t0") (Types.var "t1")))

  H.describe "Variables in complex contexts" $ do
    expectTermWithType "variable in record"
      (lambda "name" $
       record testTypePersonName [
         field "firstName" (var "name"),
         field "lastName" (string "Doe"),
         field "age" (int32 25)])
      (lambdaTyped "name" Types.string $
       record testTypePersonName [
         field "firstName" (var "name"),
         field "lastName" (string "Doe"),
         field "age" (int32 25)])
      (Types.function Types.string (Types.var "Person"))
    expectTermWithType "variable in list"
      (lambda "x" $ list [var "x", var "x"])
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ list [var "x", var "x"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.list $ Types.var "t0"))
    expectTermWithType "variable in map"
      (lambda "key" $ lambda "value" $
       Terms.map $ M.singleton (var "key") (var "value"))
      (tylams ["t0", "t1"] $ lambdaTyped "key" (Types.var "t0") $ lambdaTyped "value" (Types.var "t1") $
       Terms.map $ M.singleton (var "key") (var "value"))
      (Types.forAlls ["t0", "t1"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.map (Types.var "t0") (Types.var "t1"))))
    expectTermWithType "variable in optional"
      (lambda "x" $ just $ var "x")
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ just $ var "x")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.optional $ Types.var "t0"))

  H.describe "Recursive variables" $ do
    expectTermWithType "simple recursion"
      (lets ["f">: lambda "x" $ primitive _math_add @@ var "x" @@ int32 1] $
            var "f")
      (letsTyped [("f", lambdaTyped "x" Types.int32 $ primitive _math_add @@ var "x" @@ int32 1,
                   Types.mono $ Types.function Types.int32 Types.int32)] $
        var "f")
      (Types.function Types.int32 Types.int32)
    expectTermWithType "mutual recursion"
      (lets ["f">: lambda "x" $ var "g" @@ var "x",
             "g">: lambda "y" $ primitive _math_add @@ var "y" @@ int32 1] $
            var "f")
      (letsTyped [("f", lambdaTyped "x" Types.int32 $ var "g" @@ var "x",
                   Types.mono $ Types.function Types.int32 Types.int32),
                  ("g", lambdaTyped "y" Types.int32 $ primitive _math_add @@ var "y" @@ int32 1,
                   Types.mono $ Types.function Types.int32 Types.int32)] $
        var "f")
      (Types.function Types.int32 Types.int32)

checkTypeOfWrappedTerms :: H.SpecWith ()
checkTypeOfWrappedTerms = H.describe "Wrapped terms" $ do
  H.describe "Monomorphic wrapped terms" $ do
    expectSameTermWithType "string alias"
      (wrap testTypeStringAliasName (string "hello"))
      (Types.var "StringTypeAlias")
    expectSameTermWithType "wrapped integer"
      (wrap testTypeStringAliasName (string "wrapped"))
      (Types.var "StringTypeAlias")
    expectSameTermWithType "wrapped in tuple"
      (tuple [wrap testTypeStringAliasName (string "first"),
              string "second"])
      (Types.product [Types.var "StringTypeAlias", Types.string])

  H.describe "Polymorphic wrapped terms" $ do
    expectTermWithType "polymorphic wrapper with int"
      (wrap testTypePolymorphicWrapperName (list [int32 1, int32 2]))
      (tyapp (wrap testTypePolymorphicWrapperName (list [int32 1, int32 2])) Types.int32)
      (Types.apply (Types.var "PolymorphicWrapper") Types.int32)
    expectTermWithType "polymorphic wrapper with string"
      (wrap testTypePolymorphicWrapperName (list [string "a", string "b"]))
      (tyapp (wrap testTypePolymorphicWrapperName (list [string "a", string "b"])) Types.string)
      (Types.apply (Types.var "PolymorphicWrapper") Types.string)
    expectTermWithType "polymorphic wrapper from lambda"
      (lambda "x" $ wrap testTypePolymorphicWrapperName (list [var "x"]))
      (tylam "t0" $ lambdaTyped "x" (Types.var "t0") $ tyapp (wrap testTypePolymorphicWrapperName (list [var "x"])) (Types.var "t0"))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.apply (Types.var "PolymorphicWrapper") (Types.var "t0")))

  H.describe "Wrapped terms in complex contexts" $ do
    expectSameTermWithType "wrapped in record"
      (record testTypePersonName [
        field "firstName" (string "John"),
        field "lastName" (string "Doe"),
        field "age" (int32 30)])
      (Types.var "Person")
    expectTermWithType "wrapped in let binding"
      (lets ["alias">: wrap testTypeStringAliasName (string "test")] $
            var "alias")
      (letsTyped [("alias", wrap testTypeStringAliasName (string "test"),
                   Types.mono $ Types.var "StringTypeAlias")] $
        var "alias")
      (Types.var "StringTypeAlias")
    expectSameTermWithType "wrapped in list"
      (list [wrap testTypeStringAliasName (string "first"),
             wrap testTypeStringAliasName (string "second")])
      (Types.list $ Types.var "StringTypeAlias")

  H.describe "Nested wrapped terms" $ do
    expectTermWithType "wrapped tuple"
      (wrap testTypePolymorphicWrapperName (list [tuple [int32 1, string "a"]]))
      (tyapp (wrap testTypePolymorphicWrapperName (list [tuple [int32 1, string "a"]])) (Types.product [Types.int32, Types.string]))
      (Types.apply (Types.var "PolymorphicWrapper") (Types.product [Types.int32, Types.string]))
    expectTermWithType "wrapped optional"
      (wrap testTypePolymorphicWrapperName (list [just $ int32 42]))
      (tyapp (wrap testTypePolymorphicWrapperName (list [just $ int32 42])) (Types.optional Types.int32))
      (Types.apply (Types.var "PolymorphicWrapper") (Types.optional Types.int32))
    expectTermWithType "wrapped map"
      (wrap testTypePolymorphicWrapperName (list [Terms.map $ M.singleton (string "key") (int32 42)]))
      (tyapp (wrap testTypePolymorphicWrapperName (list [Terms.map $ M.singleton (string "key") (int32 42)])) (Types.map Types.string Types.int32))
      (Types.apply (Types.var "PolymorphicWrapper") (Types.map Types.string Types.int32))

  H.describe "Multiple wrapping levels" $ do
    expectSameTermWithType "wrapped in optional"
      (just $ wrap testTypeStringAliasName (string "wrapped"))
      (Types.optional $ Types.var "StringTypeAlias")
    expectTermWithType "list of wrapped polymorphic"
      (list [wrap testTypePolymorphicWrapperName (list [int32 1]),
             wrap testTypePolymorphicWrapperName (list [int32 2])])
      (list [tyapp (wrap testTypePolymorphicWrapperName (list [int32 1])) Types.int32,
             tyapp (wrap testTypePolymorphicWrapperName (list [int32 2])) Types.int32])
      (Types.list $ Types.apply (Types.var "PolymorphicWrapper") Types.int32)

  H.describe "Multi-parameter polymorphic wrappers" $ do
    expectTermWithType "symmetric triple wrapping simple types"
      (wrap testTypeSymmetricTripleName $
        record testTypeTripleName [
          field "first" (int32 1),
          field "second" (string "edge"),
          field "third" (int32 2)])
      (tyapps (wrap testTypeSymmetricTripleName $
        tyapps (record testTypeTripleName [
          field "first" (int32 1),
          field "second" (string "edge"),
          field "third" (int32 2)]) [Types.int32, Types.string, Types.int32])
        [Types.int32, Types.string])
      (Types.applys (Types.var "SymmetricTriple") [Types.int32, Types.string])

    expectTermWithType "symmetric triple from lambda"
      (lambda "v1" $ lambda "e" $ lambda "v2" $
        wrap testTypeSymmetricTripleName $
          record testTypeTripleName [
            field "first" (var "v1"),
            field "second" (var "e"),
            field "third" (var "v2")])
      (tylams ["t0", "t1"] $
        lambdaTyped "v1" (Types.var "t0") $
        lambdaTyped "e" (Types.var "t1") $
        lambdaTyped "v2" (Types.var "t0") $
        tyapps (wrap testTypeSymmetricTripleName $
          tyapps (record testTypeTripleName [
            field "first" (var "v1"),
            field "second" (var "e"),
            field "third" (var "v2")]) [Types.var "t0", Types.var "t1", Types.var "t0"])
          [Types.var "t0", Types.var "t1"])
      (Types.forAlls ["t0", "t1"] $
        Types.function (Types.var "t0") $
        Types.function (Types.var "t1") $
        Types.function (Types.var "t0") $
        Types.applys (Types.var "SymmetricTriple") [Types.var "t0", Types.var "t1"])

    expectTermWithType "symmetric triple with nested polymorphic types and foldl"
      (lets ["sumList">: lambda "lst" $
              primitive _lists_foldl @@
              (lambda "acc" $ lambda "x" $ primitive _math_add @@ var "acc" @@ var "x") @@
              int32 0 @@
              var "lst"] $
        lambda "nums1" $ lambda "nums2" $
          wrap testTypeSymmetricTripleName $
            record testTypeTripleName [
              field "first" (var "sumList" @@ var "nums1"),
              field "second" (list [var "nums1", var "nums2"]),
              field "third" (var "sumList" @@ var "nums2")])
      (letsTyped [("sumList",
                   lambdaTyped "lst" (Types.list Types.int32) $
                     tyapps (primitive _lists_foldl) [Types.int32, Types.int32] @@
                     (lambdaTyped "acc" Types.int32 $ lambdaTyped "x" Types.int32 $ primitive _math_add @@ var "acc" @@ var "x") @@
                     int32 0 @@
                     var "lst",
                   Types.mono $ Types.function (Types.list Types.int32) Types.int32)] $
        lambdaTyped "nums1" (Types.list Types.int32) $
        lambdaTyped "nums2" (Types.list Types.int32) $
          tyapps (wrap testTypeSymmetricTripleName $
            tyapps (record testTypeTripleName [
              field "first" (var "sumList" @@ var "nums1"),
              field "second" (list [var "nums1", var "nums2"]),
              field "third" (var "sumList" @@ var "nums2")])
              [Types.int32, Types.list (Types.list Types.int32), Types.int32])
            [Types.int32, Types.list (Types.list Types.int32)])
      (Types.function (Types.list Types.int32) $
        Types.function (Types.list Types.int32) $
        Types.applys (Types.var "SymmetricTriple") [Types.int32, Types.list (Types.list Types.int32)])

checkTypeOfWrapEliminations :: H.SpecWith ()
checkTypeOfWrapEliminations = H.describe "Wrap eliminations" $ do
  H.describe "Monomorphic unwrapping" $ do
    expectSameTermWithType "unwrap string alias"
      (unwrap testTypeStringAliasName)
      (Types.function (Types.var "StringTypeAlias") Types.string)

  H.describe "Polymorphic unwrapping" $ do
    expectTermWithType "unwrap polymorphic wrapper"
      (unwrap testTypePolymorphicWrapperName)
      (tylam "t0" $ tyapp (unwrap testTypePolymorphicWrapperName) $ Types.var "t0")
      (Types.forAll "t0" $ Types.function (Types.apply (Types.var "PolymorphicWrapper") (Types.var "t0")) (Types.list $ Types.var "t0"))

  H.describe "Unwrap eliminations in applications" $ do
    expectSameTermWithType "unwrap applied to wrapped term"
      (unwrap testTypeStringAliasName @@ wrap testTypeStringAliasName (string "hello"))
      Types.string
    expectTermWithType "unwrap polymorphic applied"
      (unwrap testTypePolymorphicWrapperName @@ wrap testTypePolymorphicWrapperName (list [int32 1, int32 2]))
      (tyapp (unwrap testTypePolymorphicWrapperName) Types.int32 @@ tyapp (wrap testTypePolymorphicWrapperName (list [int32 1, int32 2])) Types.int32)
      (Types.list Types.int32)

  H.describe "Unwrap in complex contexts" $ do
    expectTermWithType "unwrap in let binding"
      (lets ["unwrapper">: unwrap testTypeStringAliasName,
             "wrapped">: wrap testTypeStringAliasName (string "test")] $
            var "unwrapper" @@ var "wrapped")
      (letsTyped [
        ("unwrapper", unwrap testTypeStringAliasName, Types.mono $ Types.function (Types.var "StringTypeAlias") Types.string),
        ("wrapped", wrap testTypeStringAliasName (string "test"), Types.mono $ Types.var "StringTypeAlias")] $
        var "unwrapper" @@ var "wrapped")
      Types.string
    expectSameTermWithType "unwrap in tuple"
      (tuple [unwrap testTypeStringAliasName, string "context"])
      (Types.product [Types.function (Types.var "StringTypeAlias") Types.string, Types.string])
    expectTermWithType "unwrap in lambda"
      (lambda "wrapped" $ unwrap testTypeStringAliasName @@ var "wrapped")
      (lambdaTyped "wrapped" (Types.var "StringTypeAlias") $ unwrap testTypeStringAliasName @@ var "wrapped")
      (Types.function (Types.var "StringTypeAlias") Types.string)

  H.describe "Multi-parameter polymorphic unwrappers" $ do
    expectTermWithType "unwrap symmetric triple to tuple"
      (lambda "st" $
        tuple [
          project testTypeTripleName (Name "first") @@ (unwrap testTypeSymmetricTripleName @@ var "st"),
          project testTypeTripleName (Name "third") @@ (unwrap testTypeSymmetricTripleName @@ var "st")])
      (tylams ["t0", "t1"] $
        lambdaTyped "st" (Types.applys (Types.var "SymmetricTriple") [Types.var "t0", Types.var "t1"]) $
        tuple [
          tyapps (project testTypeTripleName (Name "first")) [Types.var "t0", Types.var "t1", Types.var "t0"] @@
            (tyapps (unwrap testTypeSymmetricTripleName) [Types.var "t0", Types.var "t1"] @@ var "st"),
          tyapps (project testTypeTripleName (Name "third")) [Types.var "t0", Types.var "t1", Types.var "t0"] @@
            (tyapps (unwrap testTypeSymmetricTripleName) [Types.var "t0", Types.var "t1"] @@ var "st")])
      (Types.forAlls ["t0", "t1"] $
        Types.function
          (Types.applys (Types.var "SymmetricTriple") [Types.var "t0", Types.var "t1"])
          (Types.product [Types.var "t0", Types.var "t0"]))
    expectTermWithType "unwrap and collect edges in set"
        (lets ["getEdge">: lambda "st" $
                project testTypeTripleName (Name "second") @@ (unwrap testTypeSymmetricTripleName @@ var "st")] $
          lambda "triples" $
            primitive _sets_map @@ var "getEdge" @@ var "triples")
        (tylams ["t0", "t1"] $
          letsTyped [("getEdge",
                     tylams ["t2", "t3"] $
                     lambdaTyped "st" (Types.applys (Types.var "SymmetricTriple") [Types.var "t2", Types.var "t3"]) $
                       tyapps (project testTypeTripleName (Name "second")) [Types.var "t2", Types.var "t3", Types.var "t2"] @@
                       (tyapps (unwrap testTypeSymmetricTripleName) [Types.var "t2", Types.var "t3"] @@ var "st"),
                     Types.poly ["t2", "t3"] $ Types.function
                       (Types.applys (Types.var "SymmetricTriple") [Types.var "t2", Types.var "t3"])
                       (Types.var "t3"))] $
          lambdaTyped "triples" (Types.set $ Types.applys (Types.var "SymmetricTriple") [Types.var "t0", Types.var "t1"]) $
            tyapps (primitive _sets_map) [Types.applys (Types.var "SymmetricTriple") [Types.var "t0", Types.var "t1"], Types.var "t1"] @@
            (tyapps (var "getEdge") [Types.var "t0", Types.var "t1"]) @@
            var "triples")
        (Types.forAlls ["t0", "t1"] $
          Types.function
            (Types.set $ Types.applys (Types.var "SymmetricTriple") [Types.var "t0", Types.var "t1"])
            (Types.set $ Types.var "t1"))
    expectTermWithType "unwrap with maybe to handle optional symmetric triple"
        (lambda "mst" $
          primitive _optionals_maybe @@
          nothing @@
          (lambda "st" $
            just $ project testTypeTripleName (Name "second") @@ (unwrap testTypeSymmetricTripleName @@ var "st")) @@
          var "mst")
        (tylams ["t0", "t1"] $
          lambdaTyped "mst" (Types.optional $ Types.applys (Types.var "SymmetricTriple") [Types.var "t0", Types.var "t1"]) $
          tyapps (primitive _optionals_maybe)
            [Types.optional (Types.var "t1"),
             Types.applys (Types.var "SymmetricTriple") [Types.var "t0", Types.var "t1"]] @@
          tyapp nothing (Types.var "t1") @@
          (lambdaTyped "st" (Types.applys (Types.var "SymmetricTriple") [Types.var "t0", Types.var "t1"]) $
            just $
            (tyapps (project testTypeTripleName (Name "second")) [Types.var "t0", Types.var "t1", Types.var "t0"] @@
             (tyapps (unwrap testTypeSymmetricTripleName) [Types.var "t0", Types.var "t1"] @@ var "st"))) @@
          var "mst")
        (Types.forAlls ["t0", "t1"] $
          Types.function
            (Types.optional $ Types.applys (Types.var "SymmetricTriple") [Types.var "t0", Types.var "t1"])
            (Types.optional $ Types.var "t1"))

  H.describe "Chained unwrapping" $ do
    expectTermWithType "unwrap then process"
      (lambda "wrapped" $
        primitive _strings_cat2 @@ (unwrap testTypeStringAliasName @@ var "wrapped") @@ string " suffix")
      (lambdaTyped "wrapped" (Types.var "StringTypeAlias") $
        primitive _strings_cat2 @@ (unwrap testTypeStringAliasName @@ var "wrapped") @@ string " suffix")
      (Types.function (Types.var "StringTypeAlias") Types.string)
    expectTermWithType "unwrap polymorphic then map"
      (lambda "wrappedList" $
        primitive _lists_map @@ (primitive _math_add @@ int32 1) @@ (unwrap testTypePolymorphicWrapperName @@ var "wrappedList"))
      (lambdaTyped "wrappedList" (Types.apply (Types.var "PolymorphicWrapper") Types.int32) $
        (tyapps (primitive _lists_map) [Types.int32, Types.int32]) @@ (primitive _math_add @@ int32 1) @@ (tyapp (unwrap testTypePolymorphicWrapperName) Types.int32 @@ var "wrappedList"))
      (Types.function (Types.apply (Types.var "PolymorphicWrapper") Types.int32) (Types.list Types.int32))

  H.describe "Multiple unwrap operations" $ do
    expectTermWithType "unwrap different types"
      (lambda "stringWrapped" $
        lambda "listWrapped" $
          tuple [
            unwrap testTypeStringAliasName @@ var "stringWrapped",
            unwrap testTypePolymorphicWrapperName @@ var "listWrapped"])
      (tylam "t0" $ lambdaTyped "stringWrapped" (Types.var "StringTypeAlias") $
        lambdaTyped "listWrapped" (Types.apply (Types.var "PolymorphicWrapper") (Types.var "t0")) $
          tuple [
            unwrap testTypeStringAliasName @@ var "stringWrapped",
            tyapp (unwrap testTypePolymorphicWrapperName) (Types.var "t0") @@ var "listWrapped"])
      (Types.forAll "t0" $ Types.function (Types.var "StringTypeAlias")
        (Types.function (Types.apply (Types.var "PolymorphicWrapper") (Types.var "t0"))
          (Types.product [Types.string, Types.list $ Types.var "t0"])))

----------------------------------------

expectTypeOf :: String -> Term -> Type -> H.SpecWith ()
expectTypeOf desc term typ = H.describe desc $
  withDefaults expectTypeOfResult desc term Nothing typ

expectTermWithType :: String -> Term -> Term -> Type -> H.SpecWith ()
expectTermWithType desc term eterm typ = H.describe desc $
  withDefaults expectTypeOfResult desc term (Just eterm) typ

expectSameTermWithType :: String -> Term -> Type -> H.SpecWith ()
expectSameTermWithType desc term typ = expectTermWithType desc term term typ

typeOfShouldFail :: String -> M.Map Name Type -> Term -> H.SpecWith ()
typeOfShouldFail desc types term = H.it desc $ shouldFail $ do
  cx <- graphToInferenceContext testGraph
  let tx = TypeContext types S.empty cx
  typeOf tx [] term

withDefaults :: (String -> M.Map Name Type -> Term -> x) -> String -> Term -> x
withDefaults f desc = f desc M.empty
