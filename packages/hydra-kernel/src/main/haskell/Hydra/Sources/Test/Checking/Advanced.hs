
-- | Advanced type checking test cases: annotated terms and flows
module Hydra.Sources.Test.Checking.Advanced where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M


ns :: Namespace
ns = Namespace "hydra.test.checking.advanced"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [TestGraph.ns, Namespace "hydra.rewriting"],
            moduleTypeDependencies = kernelTypesNamespaces,
            moduleDescription = (Just "Advanced type checking test cases: annotated terms and flows")}
  where
    definitions = [
      Phantoms.toDefinition allTests,
      Phantoms.toDefinition annotatedTermsTests,
      Phantoms.toDefinition topLevelAnnotationsTests,
      Phantoms.toDefinition nestedAnnotationsTests,
      Phantoms.toDefinition annotationsInComplexContextsTests]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

allTests :: TTermDefinition TestGroup
allTests = define "allTests" $
  Phantoms.doc "Advanced type checking test cases" $
  supergroup "Advanced" [
    annotatedTermsTests]

------ Helper functions ------

------ Annotated terms ------

annotatedTermsTests :: TTermDefinition TestGroup
annotatedTermsTests = define "annotatedTermsTests" $
  supergroup "Annotated terms" [
    topLevelAnnotationsTests,
    nestedAnnotationsTests,
    annotationsInComplexContextsTests]

topLevelAnnotationsTests :: TTermDefinition TestGroup
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

nestedAnnotationsTests :: TTermDefinition TestGroup
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

annotationsInComplexContextsTests :: TTermDefinition TestGroup
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


