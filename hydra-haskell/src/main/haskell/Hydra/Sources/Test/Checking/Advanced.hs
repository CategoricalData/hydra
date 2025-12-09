
-- | Advanced type checking test cases: annotated terms and flows
module Hydra.Sources.Test.Checking.Advanced where

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
module_ = Module (Namespace "hydra.test.checking.advanced") elements
    [TestGraph.module_]
    kernelTypesModules
    (Just "Advanced type checking test cases: annotated terms and flows")
  where
    elements = [
      Phantoms.toBinding allTests,
      Phantoms.toBinding annotatedTermsTests,
      Phantoms.toBinding topLevelAnnotationsTests,
      Phantoms.toBinding nestedAnnotationsTests,
      Phantoms.toBinding annotationsInComplexContextsTests,
      Phantoms.toBinding flowsTests,
      Phantoms.toBinding flowsWithFailureAcrossLetBindingsTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
  Phantoms.doc "Advanced type checking test cases" $
  supergroup "Advanced" [
    annotatedTermsTests,
    flowsTests]

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

------ Annotated terms ------

annotatedTermsTests :: TBinding TestGroup
annotatedTermsTests = define "annotatedTermsTests" $
  supergroup "Annotated terms" [
    topLevelAnnotationsTests,
    nestedAnnotationsTests,
    annotationsInComplexContextsTests]

topLevelAnnotationsTests :: TBinding TestGroup
topLevelAnnotationsTests = define "topLevelAnnotationsTests" $
  subgroup "Top-level annotations" [
    noChange "annotated literal"
      (annotated (int32 42) mapTermEmpty)
      T.int32,
    noChange "annotated list"
      (annotated (list [string "a", string "b"]) mapTermEmpty)
      (T.list T.string),
    noChange "annotated record"
      (annotated (record TestTypes.testTypePersonName [
        "firstName">: string "John",
        "lastName">: string "Doe",
        "age">: int32 25]) mapTermEmpty)
      (Core.typeVariable TestTypes.testTypePersonName),
    checkTest "annotated lambda" []
      (annotated (lambda "x" $ var "x") mapTermEmpty)
      (annotated (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ var "x") mapTermEmpty)
      (T.forAlls ["t0"] $ T.function (T.var "t0") (T.var "t0"))]

nestedAnnotationsTests :: TBinding TestGroup
nestedAnnotationsTests = define "nestedAnnotationsTests" $
  subgroup "Nested annotations" [
    noChange "annotation within annotation"
      (annotated (annotated (int32 100) mapTermEmpty) mapTermEmpty)
      T.int32,
    checkTest "annotated terms in tuple" []
      (tuple [annotated (int32 1) mapTermEmpty,
              annotated (string "hello") mapTermEmpty])
      (tyapps (pair (annotated (int32 1) mapTermEmpty) (annotated (string "hello") mapTermEmpty)) [T.int32, T.string])
      (T.pair T.int32 T.string),
    checkTest "annotated term in function application" []
      (annotated (lambda "x" $ var "x") mapTermEmpty @@ annotated (int32 42) mapTermEmpty)
      (annotated (lambdaTyped "x" T.int32 $ var "x") mapTermEmpty @@ annotated (int32 42) mapTermEmpty)
      T.int32]

annotationsInComplexContextsTests :: TBinding TestGroup
annotationsInComplexContextsTests = define "annotationsInComplexContextsTests" $
  subgroup "Annotations in complex contexts" [
    checkTest "annotated let binding" []
      (lets ["x">: annotated (int32 5) mapTermEmpty,
             "y">: annotated (string "world") mapTermEmpty] $
        annotated (tuple [var "x", var "y"]) mapTermEmpty)
      (letsTyped [("x", annotated (int32 5) mapTermEmpty, T.mono T.int32),
                  ("y", annotated (string "world") mapTermEmpty, T.mono T.string)] $
        annotated (tyapps (pair (var "x") (var "y")) [T.int32, T.string]) mapTermEmpty)
      (T.pair T.int32 T.string),
    noChange "annotated record fields"
      (record TestTypes.testTypePersonName [
        "firstName">: annotated (string "Alice") mapTermEmpty,
        "lastName">: annotated (string "Smith") mapTermEmpty,
        "age">: annotated (int32 30) mapTermEmpty])
      (Core.typeVariable TestTypes.testTypePersonName),
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

------ Flows ------

flowsTests :: TBinding TestGroup
flowsTests = define "flowsTests" $
  supergroup "Flows" [
    flowsWithFailureAcrossLetBindingsTests]

flowsWithFailureAcrossLetBindingsTests :: TBinding TestGroup
flowsWithFailureAcrossLetBindingsTests = define "flowsWithFailureAcrossLetBindingsTests" $
  subgroup "Flows with failure across let bindings" [
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
