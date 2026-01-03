module Hydra.Sources.Test.Inference.KernelExamples where

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


ns :: Namespace
ns = Namespace "hydra.test.inference.kernelExamples"

module_ :: Module
module_ = Module ns elements
    [TestGraph.ns]
    kernelTypesNamespaces
    (Just "Inference tests for examples from the Hydra kernel")
  where
    elements = [
      Phantoms.toBinding allTests,
      Phantoms.toBinding testGroupForNestedLet]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
  Phantoms.doc "Examples from the Hydra kernel" $
  supergroup "Examples from the Hydra kernel" [
    testGroupForNestedLet]

testGroupForNestedLet :: TBinding TestGroup
testGroupForNestedLet = define "testGroupForNestedLet" $
  supergroup "Nested let" [
    subgroup "hydra.formatting.mapFirstLetter" [
      expectMono 1 [tag_disabledForMinimalInference, tag_disabledForPython]  -- Very slow in Python (56+ seconds)
        (lambda "mapping" $ lambda "s" $ lets [
          "firstLetter">: var "mapping" @@ (primitive _strings_fromList @@ (primitive _lists_pure @@ (primitive _lists_head @@ var "list"))),
          "list">: primitive _strings_toList @@ var "s"] $
          primitive _logic_ifElse
            @@ (primitive _strings_null @@ var "s")
            @@ (var "s")
            @@ (primitive _strings_cat2 @@ var "firstLetter" @@ (primitive _strings_fromList @@ (primitive _lists_tail @@ var "list"))))
        (T.functionMany [T.function T.string T.string, T.string, T.string])]]
