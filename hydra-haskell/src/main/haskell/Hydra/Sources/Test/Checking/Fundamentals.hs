
-- | Fundamental type checking test cases: literals, variables, lambdas, applications, let terms, and primitives
module Hydra.Sources.Test.Checking.Fundamentals where

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
module_ = Module (Namespace "hydra.test.checking.fundamentals") elements
    [TestGraph.module_]
    kernelTypesModules
    (Just "Fundamental type checking test cases: literals, variables, lambdas, applications, let terms, and primitives")
  where
    elements = [
      Phantoms.toBinding allTests,
      Phantoms.toBinding applicationsTests,
      Phantoms.toBinding simpleFunctionApplicationsTests,
      Phantoms.toBinding partialApplicationsTests,
      Phantoms.toBinding higherOrderApplicationsTests,
      Phantoms.toBinding polymorphicApplicationsTests,
      Phantoms.toBinding applicationsInComplexContextsTests,
      Phantoms.toBinding applicationsWithComplexArgumentsTests,
      Phantoms.toBinding lambdasTests,
      Phantoms.toBinding simpleLambdasTests,
      Phantoms.toBinding multiParameterLambdasTests,
      Phantoms.toBinding lambdasWithOperationsTests,
      Phantoms.toBinding nestedLambdasTests,
      Phantoms.toBinding lambdasInComplexContextsTests,
      Phantoms.toBinding higherOrderLambdasTests,
      Phantoms.toBinding letTermsTests,
      Phantoms.toBinding simpleLetBindingsTests,
      Phantoms.toBinding letTermsWithShadowingTests,
      Phantoms.toBinding recursiveBindingsTests,
      Phantoms.toBinding mutualRecursionTests,
      Phantoms.toBinding nestedLetTermsTests,
      Phantoms.toBinding letWithComplexExpressionsTests,
      Phantoms.toBinding literalsTests,
      Phantoms.toBinding booleanLiteralsTests,
      Phantoms.toBinding stringLiteralsTests,
      Phantoms.toBinding integerLiteralsTests,
      Phantoms.toBinding floatLiteralsTests,
--      toBinding binaryLiteralsTests,  -- TODO: restore when binary literal code generation is supported
      Phantoms.toBinding literalsInComplexContextsTests,
      Phantoms.toBinding primitivesTests,
      Phantoms.toBinding nullaryPrimitivesTests,
      Phantoms.toBinding unaryPrimitivesTests,
      Phantoms.toBinding binaryPrimitivesTests,
      Phantoms.toBinding ternaryPrimitivesTests,
      Phantoms.toBinding monomorphicVsPolymorphicTests,
      Phantoms.toBinding higherOrderPrimitivesTests,
      Phantoms.toBinding primitivesInComplexContextsTests,
      Phantoms.toBinding variablesTests,
      Phantoms.toBinding simpleVariableLookupTests,
      Phantoms.toBinding variableScopingTests,
      Phantoms.toBinding polymorphicVariablesTests,
      Phantoms.toBinding variablesInComplexContextsTests,
      Phantoms.toBinding recursiveVariablesTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
  supergroup "Fundamentals" [
  literalsTests,
  variablesTests,
  lambdasTests,
  applicationsTests,
  letTermsTests,
  primitivesTests]

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

------ Applications ------

applicationsTests :: TBinding TestGroup
applicationsTests = define "applicationsTests" $
  supergroup "Applications" [
  simpleFunctionApplicationsTests,
  partialApplicationsTests,
  higherOrderApplicationsTests,
  polymorphicApplicationsTests,
  applicationsInComplexContextsTests,
  applicationsWithComplexArgumentsTests]

simpleFunctionApplicationsTests :: TBinding TestGroup
simpleFunctionApplicationsTests = define "simpleFunctionApplicationsTests" $
  subgroup "Simple function applications" [
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

partialApplicationsTests :: TBinding TestGroup
partialApplicationsTests = define "partialApplicationsTests" $
  subgroup "Partial applications" [
  noChange "partially applied add"
    (primitive _math_add @@ int32 5)
    (T.function T.int32 T.int32),
  noChange "partially applied string cat"
    (primitive _strings_cat2 @@ string "prefix")
    (T.function T.string T.string)]

higherOrderApplicationsTests :: TBinding TestGroup
higherOrderApplicationsTests = define "higherOrderApplicationsTests" $
  subgroup "Higher-order applications" [
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

polymorphicApplicationsTests :: TBinding TestGroup
polymorphicApplicationsTests = define "polymorphicApplicationsTests" $
  subgroup "Polymorphic applications" [
  checkTest "polymorphic identity" []
    (lets ["id">: lambda "x" $ var "x"] $
      tuple [var "id" @@ int32 42, var "id" @@ string "hello"])
    (letsTyped [
      ("id", tylam "t0" $ lambdaTyped "x" (T.var "t0") $ var "x",
        T.poly ["t0"] $ T.function (T.var "t0") (T.var "t0"))] $
      tyapps (pair (tyapp (var "id") T.int32 @@ int32 42) (tyapp (var "id") T.string @@ string "hello")) [T.int32, T.string])
    (T.pair T.int32 T.string),
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

applicationsInComplexContextsTests :: TBinding TestGroup
applicationsInComplexContextsTests = define "applicationsInComplexContextsTests" $
  subgroup "Applications in complex contexts" [
  checkTest "application in tuple" []
    (tuple [primitive _math_add @@ int32 1 @@ int32 2,
            primitive _strings_cat2 @@ string "a" @@ string "b"])
    (tyapps (pair (primitive _math_add @@ int32 1 @@ int32 2) (primitive _strings_cat2 @@ string "a" @@ string "b")) [T.int32, T.string])
    (T.pair T.int32 T.string),
  noChange "application in record"
    (record TestTypes.testTypePersonName [
      "firstName">: primitive _strings_cat2 @@ string "John" @@ string "ny",
      "lastName">: string "Doe",
      "age">: primitive _math_add @@ int32 20 @@ int32 5])
    (Core.typeVariable TestTypes.testTypePersonName),
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

applicationsWithComplexArgumentsTests :: TBinding TestGroup
applicationsWithComplexArgumentsTests = define "applicationsWithComplexArgumentsTests" $
  subgroup "Applications with complex arguments" [
  checkTest "application with record argument" []
    (lets ["getName">: lambda "person" $ project TestTypes.testTypePersonName (name "firstName") @@ var "person"] $
      var "getName" @@ record TestTypes.testTypePersonName [
        "firstName">: string "Alice",
        "lastName">: string "Smith",
        "age">: int32 25])
    (letsTyped [
      ("getName", lambdaTyped "person" (Core.typeVariable TestTypes.testTypePersonName) $ project TestTypes.testTypePersonName (name "firstName") @@ var "person",
        T.mono $ T.function (Core.typeVariable TestTypes.testTypePersonName) T.string)] $
      var "getName" @@ record TestTypes.testTypePersonName [
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

------ Lambdas ------

lambdasTests :: TBinding TestGroup
lambdasTests = define "lambdasTests" $
  supergroup "Lambdas" [
  simpleLambdasTests,
  multiParameterLambdasTests,
  lambdasWithOperationsTests,
  nestedLambdasTests,
  lambdasInComplexContextsTests,
  higherOrderLambdasTests]

simpleLambdasTests :: TBinding TestGroup
simpleLambdasTests = define "simpleLambdasTests" $
  subgroup "Simple lambdas" [
  checkTest "identity function" []
    (lambda "x" $ var "x")
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ var "x")
    (T.forAlls ["t0"] $ T.function (T.var "t0") (T.var "t0")),
  checkTest "constant function" []
    (lambda "x" $ int32 42)
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ int32 42)
    (T.forAlls ["t0"] $ T.function (T.var "t0") T.int32)]

multiParameterLambdasTests :: TBinding TestGroup
multiParameterLambdasTests = define "multiParameterLambdasTests" $
  subgroup "Multi-parameter lambdas" [
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
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t1") $ tyapps (pair (var "x") (tyapps (pair (var "x") (var "y")) [T.var "t0", T.var "t1"])) [T.var "t0", T.pair (T.var "t0") (T.var "t1")])
    (T.forAlls ["t0", "t1"] $ T.function
      (T.var "t0") (T.function (T.var "t1")
      (T.pair (T.var "t0") (T.pair (T.var "t0") (T.var "t1")))))]

lambdasWithOperationsTests :: TBinding TestGroup
lambdasWithOperationsTests = define "lambdasWithOperationsTests" $
  subgroup "Lambdas with operations" [
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
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t1") $ tyapps (pair (var "x") (var "y")) [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.function (T.var "t1") (T.pair (T.var "t0") (T.var "t1"))))]

nestedLambdasTests :: TBinding TestGroup
nestedLambdasTests = define "nestedLambdasTests" $
  subgroup "Nested lambdas" [
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

lambdasInComplexContextsTests :: TBinding TestGroup
lambdasInComplexContextsTests = define "lambdasInComplexContextsTests" $
  subgroup "Lambdas in complex contexts" [
  checkTest "lambda in tuple" []
    (tuple [lambda "x" $ var "x", int32 42])
    (tylam "t0" $ tyapps (pair (lambdaTyped "x" (T.var "t0") $ var "x") (int32 42)) [T.function (T.var "t0") (T.var "t0"), T.int32])
    (T.forAlls ["t0"] $ T.pair (T.function (T.var "t0") (T.var "t0")) T.int32),
  checkTest "lambda in list" []
    (list [lambda "x" $ primitive _math_add @@ var "x" @@ int32 1,
           lambda "y" $ primitive _math_mul @@ var "y" @@ int32 2])
    (list [lambdaTyped "x" T.int32 $ primitive _math_add @@ var "x" @@ int32 1,
           lambdaTyped "y" T.int32 $ primitive _math_mul @@ var "y" @@ int32 2])
    (T.list $ T.function T.int32 T.int32),
  checkTest "lambda in record" []
    (lambda "name" $ record TestTypes.testTypePersonName [
      "firstName">: var "name",
      "lastName">: string "Doe",
      "age">: int32 30])
    (lambdaTyped "name" T.string $ record TestTypes.testTypePersonName [
      "firstName">: var "name",
      "lastName">: string "Doe",
      "age">: int32 30])
    (T.function T.string (Core.typeVariable TestTypes.testTypePersonName))]

higherOrderLambdasTests :: TBinding TestGroup
higherOrderLambdasTests = define "higherOrderLambdasTests" $
  subgroup "Higher-order lambdas" [
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

letTermsTests :: TBinding TestGroup
letTermsTests = define "letTermsTests" $
  supergroup "Let terms" [
  simpleLetBindingsTests,
  letTermsWithShadowingTests,
  recursiveBindingsTests,
  mutualRecursionTests,
  nestedLetTermsTests,
  letWithComplexExpressionsTests]

simpleLetBindingsTests :: TBinding TestGroup
simpleLetBindingsTests = define "simpleLetBindingsTests" $
  subgroup "Simple let bindings" [
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
      tyapps (pair (var "x") (var "y")) [T.int32, T.string])
    (T.pair T.int32 T.string)]

letTermsWithShadowingTests :: TBinding TestGroup
letTermsWithShadowingTests = define "letTermsWithShadowingTests" $
  subgroup "Let terms with shadowing" [
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
            tyapps (pair (var "x") (var "y")) [T.var "t1", T.var "t0"])
    (T.forAlls ["t0", "t1"] $
      T.function (T.var "t0") $
        T.function (T.var "t1") $
          T.pair (T.var "t1") (T.var "t0")),
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
            tyapps (pair (var "x") (var "z")) [T.var "t0", T.int32])
    (T.forAlls ["t0"] $
      T.function (T.var "t0") $
        T.pair (T.var "t0") T.int32)]

recursiveBindingsTests :: TBinding TestGroup
recursiveBindingsTests = define "recursiveBindingsTests" $
  subgroup "Recursive bindings" [
  checkTest "simple arithmetic recursion" []
    (lets ["double">: lambda "n" $ primitive _math_add @@ var "n" @@ var "n"] $
          var "double" @@ int32 5)
    (letsTyped [("double", lambdaTyped "n" T.int32 $ primitive _math_add @@ var "n" @@ var "n",
                 T.mono $ T.function T.int32 T.int32)] $
      var "double" @@ int32 5)
    T.int32]

mutualRecursionTests :: TBinding TestGroup
mutualRecursionTests = define "mutualRecursionTests" $
  subgroup "Mutual recursion" [
  checkTest "mutually recursive data" []
    (lets ["listA">: record TestTypes.testTypeBuddyListAName [
             "head">: int32 1,
             "tail">: optional $ just $ var "listB"],
           "listB">: record TestTypes.testTypeBuddyListBName [
             "head">: int32 2,
             "tail">: optional nothing]] $
          var "listA")
    (letsTyped [("listA", tyapp (record TestTypes.testTypeBuddyListAName [
                   "head">: int32 1,
                   "tail">: optional $ just $ var "listB"]) T.int32,
                 T.mono $ T.apply (Core.typeVariable TestTypes.testTypeBuddyListAName) T.int32),
                ("listB", tyapp (record TestTypes.testTypeBuddyListBName [
                   "head">: int32 2,
                   "tail">: tyapp (optional nothing) (T.apply (Core.typeVariable TestTypes.testTypeBuddyListAName) T.int32)]) T.int32,
                 T.mono $ T.apply (Core.typeVariable TestTypes.testTypeBuddyListBName) T.int32)] $
      var "listA")
    (T.apply (Core.typeVariable TestTypes.testTypeBuddyListAName) T.int32),
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

nestedLetTermsTests :: TBinding TestGroup
nestedLetTermsTests = define "nestedLetTermsTests" $
  subgroup "Nested let terms" [
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

letWithComplexExpressionsTests :: TBinding TestGroup
letWithComplexExpressionsTests = define "letWithComplexExpressionsTests" $
  subgroup "Let with complex expressions" [
  checkTest "let in record" []
    (record TestTypes.testTypePersonName [
      "firstName">: lets ["first">: string "John",
                              "middle">: string "Q"] $
                             primitive _strings_cat2 @@ var "first" @@ var "middle",
      "lastName">: string "Doe",
      "age">: int32 30])
    (record TestTypes.testTypePersonName [
      "firstName">: letsTyped [("first", string "John", T.mono T.string),
                                   ("middle", string "Q", T.mono T.string)] $
                         primitive _strings_cat2 @@ var "first" @@ var "middle",
      "lastName">: string "Doe",
      "age">: int32 30])
    (Core.typeVariable TestTypes.testTypePersonName),
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
      tyapps (pair (tyapp (var "id") T.int32 @@ int32 42) (tyapp (var "id") T.string @@ string "hello")) [T.int32, T.string])
    (T.pair T.int32 T.string),
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

------ Literals ------

literalsTests :: TBinding TestGroup
literalsTests = define "literalsTests" $
  supergroup "Literals" [
  booleanLiteralsTests,
  stringLiteralsTests,
  integerLiteralsTests,
  floatLiteralsTests,
--  binaryLiteralsTests, -- TODO: restore this group
  literalsInComplexContextsTests]

booleanLiteralsTests :: TBinding TestGroup
booleanLiteralsTests = define "booleanLiteralsTests" $
  subgroup "Boolean literals" [
  noChange "true" (boolean True) T.boolean,
  noChange "false" (boolean False) T.boolean]

stringLiteralsTests :: TBinding TestGroup
stringLiteralsTests = define "stringLiteralsTests" $
  subgroup "String literals" [
  noChange "simple string" (string "hello") T.string,
  noChange "empty string" (string "") T.string,
  noChange "unicode string" (string "café") T.string]

integerLiteralsTests :: TBinding TestGroup
integerLiteralsTests = define "integerLiteralsTests" $
  subgroup "Integer literals" [
  noChange "bigint" (bigint 42) T.bigint,
  noChange "int8" (int8 127) T.int8,
  noChange "int16" (int16 32767) T.int16,
  noChange "int32" (int32 2147483647) T.int32,
  noChange "int64" (int64 9223372036854775807) T.int64,
  noChange "uint8" (uint8 255) T.uint8,
  noChange "uint16" (uint16 65535) T.uint16,
  noChange "uint32" (uint32 4294967295) T.uint32,
  noChange "uint64" (uint64 18446744073709551615) T.uint64]

floatLiteralsTests :: TBinding TestGroup
floatLiteralsTests = define "floatLiteralsTests" $
  subgroup "Float literals" [
  noChange "bigfloat" (bigfloat 3.14159) T.bigfloat,
  noChange "float32" (float32 2.71828) T.float32,
  noChange "float64" (float64 1.41421) T.float64]

binaryLiteralsTests :: TBinding TestGroup
binaryLiteralsTests = define "binaryLiteralsTests" $
  subgroup "Binary literals" [
  noChange "binary" (binary "SGVsbG8gV29ybGQ=") T.binary]

literalsInComplexContextsTests :: TBinding TestGroup
literalsInComplexContextsTests = define "literalsInComplexContextsTests" $
  subgroup "Literals in complex contexts" [
  checkTest "literals in tuple" []
    (tuple [boolean True, tuple [string "test", tuple [int32 42, float32 3.14]]])
    (tyapps (pair (boolean True) (tyapps (pair (string "test") (tyapps (pair (int32 42) (float32 3.14)) [T.int32, T.float32])) [T.string, T.pair T.int32 T.float32])) [T.boolean, T.pair T.string (T.pair T.int32 T.float32)])
    (T.pair T.boolean (T.pair T.string (T.pair T.int32 T.float32))),
  noChange "literals in list"
    (list [string "one", string "two", string "three"])
    (T.list T.string)]

------ Primitives ------

primitivesTests :: TBinding TestGroup
primitivesTests = define "primitivesTests" $
  supergroup "Primitives" [
  nullaryPrimitivesTests,
  unaryPrimitivesTests,
  binaryPrimitivesTests,
  ternaryPrimitivesTests,
  monomorphicVsPolymorphicTests,
  higherOrderPrimitivesTests,
  primitivesInComplexContextsTests]

nullaryPrimitivesTests :: TBinding TestGroup
nullaryPrimitivesTests = define "nullaryPrimitivesTests" $
  subgroup "Nullary primitives" [
  checkTest "empty map" []
    (primitive _maps_empty)
    (tylams ["t0", "t1"] $ tyapps (primitive _maps_empty) [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.map (T.var "t0") (T.var "t1")),
  checkTest "empty set" []
    (primitive _sets_empty)
    (tylam "t0" $ tyapp (primitive _sets_empty) (T.var "t0"))
    (T.forAll "t0" $ T.set $ T.var "t0")]

unaryPrimitivesTests :: TBinding TestGroup
unaryPrimitivesTests = define "unaryPrimitivesTests" $
  subgroup "Unary primitives" [
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

binaryPrimitivesTests :: TBinding TestGroup
binaryPrimitivesTests = define "binaryPrimitivesTests" $
  subgroup "Binary primitives" [
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

ternaryPrimitivesTests :: TBinding TestGroup
ternaryPrimitivesTests = define "ternaryPrimitivesTests" $
  subgroup "Ternary primitives" [
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

monomorphicVsPolymorphicTests :: TBinding TestGroup
monomorphicVsPolymorphicTests = define "monomorphicVsPolymorphicTests" $
  subgroup "Monomorphic vs polymorphic" [
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

higherOrderPrimitivesTests :: TBinding TestGroup
higherOrderPrimitivesTests = define "higherOrderPrimitivesTests" $
  subgroup "Higher-order primitives" [
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

primitivesInComplexContextsTests :: TBinding TestGroup
primitivesInComplexContextsTests = define "primitivesInComplexContextsTests" $
  subgroup "Primitives in complex contexts" [
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

------ Variables ------

variablesTests :: TBinding TestGroup
variablesTests = define "variablesTests" $
  supergroup "Variables" [
  simpleVariableLookupTests,
  variableScopingTests,
  polymorphicVariablesTests,
  variablesInComplexContextsTests,
  recursiveVariablesTests]

simpleVariableLookupTests :: TBinding TestGroup
simpleVariableLookupTests = define "simpleVariableLookupTests" $
  subgroup "Simple variable lookup" [
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
      tyapps (pair (var "x") (var "y")) [T.string, T.int32])
    (T.pair T.string T.int32)]

variableScopingTests :: TBinding TestGroup
variableScopingTests = define "variableScopingTests" $
  subgroup "Variable scoping" [
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
      tyapps (pair (var "x") (tyapps (pair (var "y") (var "z")) [T.var "t0", T.var "t1"])) [T.var "t0", T.pair (T.var "t0") (T.var "t1")])
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.function (T.var "t1") (T.pair (T.var "t0") (T.pair (T.var "t0") (T.var "t1")))))]

polymorphicVariablesTests :: TBinding TestGroup
polymorphicVariablesTests = define "polymorphicVariablesTests" $
  subgroup "Polymorphic variables" [
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
      tyapps (pair (tyapp (var "id") T.int32 @@ int32 42) (tyapp (var "id") T.string @@ string "test")) [T.int32, T.string])
    (T.pair T.int32 T.string),
  checkTest "higher order polymorphic" []
    (lets ["apply">: lambda "f" $ lambda "x" $ var "f" @@ var "x"] $
          var "apply")
    (tylams ["t0", "t1"] $ letsTyped [("apply", tylams ["t2", "t3"] $ lambdaTyped "f" (T.function (T.var "t2") (T.var "t3")) $ lambdaTyped "x" (T.var "t2") $ var "f" @@ var "x",
                                       T.poly ["t2", "t3"] $ T.function (T.function (T.var "t2") (T.var "t3")) (T.function (T.var "t2") (T.var "t3")))] $
      tyapps (var "apply") [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $
      T.function (T.function (T.var "t0") (T.var "t1")) (T.function (T.var "t0") (T.var "t1")))]

variablesInComplexContextsTests :: TBinding TestGroup
variablesInComplexContextsTests = define "variablesInComplexContextsTests" $
  subgroup "Variables in complex contexts" [
  checkTest "variable in record" []
    (lambda "name" $
     record TestTypes.testTypePersonName [
       "firstName">: (var "name"),
       "lastName">: (string "Doe"),
       "age">: (int32 25)])
    (lambdaTyped "name" T.string $
     record TestTypes.testTypePersonName [
       "firstName">: (var "name"),
       "lastName">: (string "Doe"),
       "age">: (int32 25)])
    (T.function T.string (Core.typeVariable TestTypes.testTypePersonName)),
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

recursiveVariablesTests :: TBinding TestGroup
recursiveVariablesTests = define "recursiveVariablesTests" $
  subgroup "Recursive variables" [
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
