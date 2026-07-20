module Hydra.Sources.Test.Inference.KernelExamples where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Overlay.Haskell.Dsl.Typed.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M
import qualified Hydra.Overlay.Haskell.Dsl.Prims as Prims
import qualified Hydra.Lib.Lists as DefLists
import qualified Hydra.Lib.Literals as DefLiterals
import qualified Hydra.Lib.Logic as DefLogic
import qualified Hydra.Lib.Maps as DefMaps
import qualified Hydra.Lib.Math as DefMath
import qualified Hydra.Lib.Optionals as DefOptionals
import qualified Hydra.Lib.Pairs as DefPairs
import qualified Hydra.Lib.Strings as DefStrings


ns :: ModuleName
ns = ModuleName "hydra.test.inference.kernelExamples"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([TestGraph.ns, ModuleName "hydra.formatting", ModuleName "hydra.inference", ModuleName "hydra.print.core"] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Inference tests for examples from the Hydra kernel"))}
  where
    definitions = [
      Phantoms.toDefinition allTests,
      Phantoms.toDefinition testGroupForNestedLet]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
  Phantoms.doc "Examples from the Hydra kernel" $
  supergroup "Examples from the Hydra kernel" [
    testGroupForNestedLet]

testGroupForNestedLet :: TypedTermDefinition TestGroup
testGroupForNestedLet = define "testGroupForNestedLet" $
  supergroup "Nested let" [
    subgroup "hydra.formatting.mapFirstLetter" [
      expectMono 1 [tag_disabledForMinimalInference]
        (lambda "mapping" $ lambda "s" $ lets [
          "list">: primitive DefStrings.toList @@ var "s",
          "firstLetter">: var "mapping" @@ (primitive DefStrings.fromList @@ (primitive DefOptionals.cat @@ list [primitive DefLists.maybeHead @@ var "list"]))] $
          primitive DefLogic.ifElse
            @@ (primitive DefStrings.null @@ var "s")
            @@ (var "s")
            @@ (primitive DefStrings.cat2 @@ var "firstLetter" @@ (primitive DefStrings.fromList @@ (primitive DefLists.drop @@ int32 1 @@ var "list"))))
        (T.functionMany [T.function T.string T.string, T.string, T.string])],

    -- Simplified reproduction of fullyStripAndNormalizeType's 'go' binding using ifElse.
    -- Tests that inference gives go a monomorphic type when all types are constrained
    -- by primitive calls (Maps.insert, Math.add).
    subgroup "Recursive let with pair return (ifElse)" [
      expectMono 2 [tag_disabledForMinimalInference]
        (lambda "input" $ lets [
          "go">:
            lambda "depth" $ lambda "subst" $ lambda "s" $
              primitive DefLogic.ifElse
                @@ (primitive DefStrings.null @@ var "s")
                @@ (pair (var "subst") (var "s"))
                @@ (var "go"
                      @@ (primitive DefMath.add @@ var "depth" @@ int32 1)
                      @@ (primitive DefMaps.insert @@ string "key" @@ string "val" @@ var "subst")
                      @@ var "s")] $
          lets [
            "result">: var "go" @@ int32 0 @@ primitive DefMaps.empty @@ var "input",
            "subst">: primitive DefPairs.first @@ var "result",
            "body">: primitive DefPairs.second @@ var "result"] $
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
                    @@ (primitive DefMath.add @@ var "depth" @@ int32 1)
                    @@ (primitive DefMaps.insert
                          @@ (project (Core.nameLift _ForallType) (Core.nameLift _ForallType_parameter) @@ var "ft")
                          @@ (Terms.wrap (Core.nameLift _Name) (primitive DefStrings.cat2 @@ string "_" @@ (primitive DefLiterals.showInt32 @@ var "depth")))
                          @@ var "subst")
                    @@ (project (Core.nameLift _ForallType) (Core.nameLift _ForallType_body) @@ var "ft")]
              @@ var "t"] $
          lets [
            "result">: var "go" @@ int32 0 @@ primitive DefMaps.empty @@ var "typ"] $
          pair (primitive DefPairs.first @@ var "result") (primitive DefPairs.second @@ var "result"))
        -- Expected: Type -> (Map Name Name, Type)
        -- Named types appear as TypeVariable in schemes (e.g., hydra.core.Name, hydra.core.Type)
        (T.function (T.variable "hydra.core.Type")
          (T.pair (T.map (T.variable "hydra.core.Name") (T.variable "hydra.core.Name")) (T.variable "hydra.core.Type")))]]
