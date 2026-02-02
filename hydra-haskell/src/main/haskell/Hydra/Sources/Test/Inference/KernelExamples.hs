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
      expectMono 1 [tag_disabledForMinimalInference]
        (lambda "mapping" $ lambda "s" $ lets [
          "firstLetter">: var "mapping" @@ (primitive _strings_fromList @@ (primitive _lists_pure @@ (primitive _lists_head @@ var "list"))),
          "list">: primitive _strings_toList @@ var "s"] $
          primitive _logic_ifElse
            @@ (primitive _strings_null @@ var "s")
            @@ (var "s")
            @@ (primitive _strings_cat2 @@ var "firstLetter" @@ (primitive _strings_fromList @@ (primitive _lists_tail @@ var "list"))))
        (T.functionMany [T.function T.string T.string, T.string, T.string])],

    -- Simplified reproduction of fullyStripAndNormalizeType's 'go' binding using ifElse.
    -- Tests that inference gives go a monomorphic type when all types are constrained
    -- by primitive calls (Maps.insert, Math.add).
    subgroup "Recursive let with pair return (ifElse)" [
      expectMono 2 [tag_disabledForMinimalInference]
        (lambda "input" $ lets [
          "go">:
            lambda "depth" $ lambda "subst" $ lambda "s" $
              primitive _logic_ifElse
                @@ (primitive _strings_null @@ var "s")
                @@ (pair (var "subst") (var "s"))
                @@ (var "go"
                      @@ (primitive _math_add @@ var "depth" @@ int32 1)
                      @@ (primitive _maps_insert @@ string "key" @@ string "val" @@ var "subst")
                      @@ var "s")] $
          lets [
            "result">: var "go" @@ int32 0 @@ primitive _maps_empty @@ var "input",
            "subst">: primitive _pairs_first @@ var "result",
            "body">: primitive _pairs_second @@ var "result"] $
          pair (var "subst") (var "body"))
        (T.function T.string
          (T.pair (T.map T.string T.string) T.string))],

    -- Closer reproduction of fullyStripAndNormalizeType's 'go' binding using
    -- case/match on the Type union (with a default case).
    -- The default case returns (subst, t) directly; the forall case recurses
    -- with Maps.insert constraining subst to Map Name Name.
    -- This tests whether union elimination with a default case causes
    -- inference to over-generalize the pair-first type variable.
    subgroup "Recursive let with pair return (case on Type)" [
      expectMono 3 [tag_disabledForMinimalInference]
        (lambda "typ" $ lets [
          "go">:
            lambda "depth" $ lambda "subst" $ lambda "t" $
              Terms.match (Core.nameLift _Type) (just $ pair (var "subst") (var "t")) [
                _Type_forall >>: lambda "ft" $
                  var "go"
                    @@ (primitive _math_add @@ var "depth" @@ int32 1)
                    @@ (primitive _maps_insert
                          @@ (project (Core.nameLift _ForallType) (Core.nameLift _ForallType_parameter) @@ var "ft")
                          @@ (Terms.wrap (Core.nameLift _Name) (primitive _strings_cat2 @@ string "_" @@ (primitive _literals_showInt32 @@ var "depth")))
                          @@ var "subst")
                    @@ (project (Core.nameLift _ForallType) (Core.nameLift _ForallType_body) @@ var "ft")]
              @@ var "t"] $
          lets [
            "result">: var "go" @@ int32 0 @@ primitive _maps_empty @@ var "typ"] $
          pair (primitive _pairs_first @@ var "result") (primitive _pairs_second @@ var "result"))
        -- Expected: Type -> (Map Name Name, Type)
        -- Named types appear as TypeVariable in schemes (e.g., hydra.core.Name, hydra.core.Type)
        (T.function (T.variable "hydra.core.Type")
          (T.pair (T.map (T.variable "hydra.core.Name") (T.variable "hydra.core.Name")) (T.variable "hydra.core.Type")))]]
