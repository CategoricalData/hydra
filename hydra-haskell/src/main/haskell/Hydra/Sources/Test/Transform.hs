-- | Transform test cases for code generation, filtering to tests that can be compiled to target languages

module Hydra.Sources.Test.Transform where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (transformModule)
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Accessors                  as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Errors                     as Error
import qualified Hydra.Dsl.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_


ns :: Namespace
ns = Namespace "hydra.test.transform"


module_ :: Module
module_ = Module ns elements
    [Formatting.ns, Sorting.ns]
    KernelTypes.kernelTypesNamespaces $
    Just "Transform test cases for code generation, filtering to tests that can be compiled to target languages"
  where
    elements = [
      toTermDefinition transformToCompiledTests,
      toTermDefinition transformTestCase,
      toTermDefinition buildConvertCaseCall,
      toTermDefinition encodeCaseConvention,
      toTermDefinition addGenerationPrefix,
      toTermDefinition transformModule,
      toTermDefinition collectTestCases,
      toTermDefinition buildTopologicalSortCall,
      toTermDefinition buildTopologicalSortSCCCall,
      toTermDefinition encodeAdjacencyList,
      toTermDefinition encodeInt,
      toTermDefinition encodeEitherListList,
      toTermDefinition encodeListList,
      toTermDefinition encodeIntList]


-- | Transform test group hierarchy to only include delegated evaluation tests
transformToCompiledTests :: TBinding (TestGroup -> Maybe TestGroup)
transformToCompiledTests = define "transformToCompiledTests" $
  doc "Transform test group hierarchy to only include delegated evaluation tests" $
  lambda "tg" $ lets [
    "name_">: project _TestGroup _TestGroup_name @@ var "tg",
    "desc">: project _TestGroup _TestGroup_description @@ var "tg",
    "subgroups">: project _TestGroup _TestGroup_subgroups @@ var "tg",
    "cases_">: project _TestGroup _TestGroup_cases @@ var "tg",
    "transformedCases">: Maybes.cat (Lists.map (lambda "tc" $ transformTestCase @@ var "tc") (var "cases_")),
    "transformedSubgroups">: Maybes.cat (Lists.map (lambda "sg" $ transformToCompiledTests @@ var "sg") (var "subgroups"))] $
    Logic.ifElse
      (Logic.and (Lists.null (var "transformedCases")) (Lists.null (var "transformedSubgroups")))
      nothing
      (just $ Testing.testGroup (var "name_") (var "desc") (var "transformedSubgroups") (var "transformedCases"))


-- | Transform a test case to DelegatedEvaluationTestCase if applicable
transformTestCase :: TBinding (TestCaseWithMetadata -> Maybe TestCaseWithMetadata)
transformTestCase = define "transformTestCase" $
  doc "Transform a test case to DelegatedEvaluationTestCase if applicable" $
  lambda "tcm" $ lets [
    "name_">: project _TestCaseWithMetadata _TestCaseWithMetadata_name @@ var "tcm",
    "tc">: project _TestCaseWithMetadata _TestCaseWithMetadata_case @@ var "tcm",
    "desc">: project _TestCaseWithMetadata _TestCaseWithMetadata_description @@ var "tcm",
    "tags_">: project _TestCaseWithMetadata _TestCaseWithMetadata_tags @@ var "tcm"] $
    cases _TestCase (var "tc") (Just nothing) [
      _TestCase_caseConversion>>: lambda "ccase" $ lets [
        "fromConv">: project _CaseConversionTestCase _CaseConversionTestCase_fromConvention @@ var "ccase",
        "toConv">: project _CaseConversionTestCase _CaseConversionTestCase_toConvention @@ var "ccase",
        "fromStr">: project _CaseConversionTestCase _CaseConversionTestCase_fromString @@ var "ccase",
        "toStr">: project _CaseConversionTestCase _CaseConversionTestCase_toString @@ var "ccase"] $
        just $ Testing.testCaseWithMetadata (var "name_")
          (Testing.testCaseDelegatedEvaluation $ Testing.delegatedEvaluationTestCase
            (buildConvertCaseCall @@ var "fromConv" @@ var "toConv" @@ var "fromStr")
            (Core.termLiteral $ Core.literalString (var "toStr")))
          (var "desc") (var "tags_"),

      _TestCase_evaluation>>: lambda "ecase" $ lets [
        "input_">: project _EvaluationTestCase _EvaluationTestCase_input @@ var "ecase",
        "output_">: project _EvaluationTestCase _EvaluationTestCase_output @@ var "ecase"] $
        just $ Testing.testCaseWithMetadata (var "name_")
          (Testing.testCaseDelegatedEvaluation $ Testing.delegatedEvaluationTestCase
            (var "input_") (var "output_"))
          (var "desc") (var "tags_"),

      _TestCase_delegatedEvaluation>>: lambda "_" $
        just (var "tcm"),

      _TestCase_topologicalSort>>: lambda "tscase" $ lets [
        "adjList">: project _TopologicalSortTestCase _TopologicalSortTestCase_adjacencyList @@ var "tscase",
        "expected">: project _TopologicalSortTestCase _TopologicalSortTestCase_expected @@ var "tscase"] $
        just $ Testing.testCaseWithMetadata (var "name_")
          (Testing.testCaseDelegatedEvaluation $ Testing.delegatedEvaluationTestCase
            (buildTopologicalSortCall @@ var "adjList")
            (encodeEitherListList @@ var "expected"))
          (var "desc") (var "tags_"),

      _TestCase_topologicalSortSCC>>: lambda "scccase" $ lets [
        "adjList">: project _TopologicalSortSCCTestCase _TopologicalSortSCCTestCase_adjacencyList @@ var "scccase",
        "expected">: project _TopologicalSortSCCTestCase _TopologicalSortSCCTestCase_expected @@ var "scccase"] $
        just $ Testing.testCaseWithMetadata (var "name_")
          (Testing.testCaseDelegatedEvaluation $ Testing.delegatedEvaluationTestCase
            (buildTopologicalSortSCCCall @@ var "adjList")
            (encodeListList @@ var "expected"))
          (var "desc") (var "tags_"),

      _TestCase_validateCoreTerm>>: lambda "_" $
        just (var "tcm")]


-- | Build a Term representing a convertCase function call
buildConvertCaseCall :: TBinding (CaseConvention -> CaseConvention -> String -> Term)
buildConvertCaseCall = define "buildConvertCaseCall" $
  doc "Build a Term representing a convertCase function call" $
  lambdas ["fromConv", "toConv", "input_"] $
    Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termVariable $ Core.name (string "hydra.formatting.convertCase"))
          (encodeCaseConvention @@ var "fromConv"))
        (encodeCaseConvention @@ var "toConv"))
      (Core.termLiteral $ Core.literalString (var "input_"))


-- | Encode CaseConvention as a Term (unit variant)
encodeCaseConvention :: TBinding (CaseConvention -> Term)
encodeCaseConvention = define "encodeCaseConvention" $
  doc "Encode CaseConvention as a Term (unit variant)" $
  lambda "conv" $
    Core.termUnion $ Core.injection
      (Core.nameLift _CaseConvention)
      (Core.field
        (cases _CaseConvention (var "conv") Nothing [
          _CaseConvention_lowerSnake>>: constant (Core.nameLift _CaseConvention_lowerSnake),
          _CaseConvention_upperSnake>>: constant (Core.nameLift _CaseConvention_upperSnake),
          _CaseConvention_camel>>: constant (Core.nameLift _CaseConvention_camel),
          _CaseConvention_pascal>>: constant (Core.nameLift _CaseConvention_pascal)])
        Core.termUnit)


-- | Add "generation" namespace prefix
addGenerationPrefix :: TBinding (Namespace -> Namespace)
addGenerationPrefix = define "addGenerationPrefix" $
  doc "Add generation namespace prefix" $
  lambda "ns_" $
    wrap _Namespace (Strings.cat2 (string "generation.") (unwrap _Namespace @@ var "ns_"))


-- | Transform module with generation namespace
transformModule :: TBinding (Module -> Module)
transformModule = define "transformModule" $
  doc "Transform module with generation namespace" $
  lambda "m" $
    Module.module_
      (addGenerationPrefix @@ (project _Module _Module_namespace @@ var "m"))
      (project _Module _Module_definitions @@ var "m")
      (project _Module _Module_termDependencies @@ var "m")
      (project _Module _Module_typeDependencies @@ var "m")
      (project _Module _Module_description @@ var "m")


-- | Collect all test cases from a test group (flattening hierarchy)
collectTestCases :: TBinding (TestGroup -> [TestCaseWithMetadata])
collectTestCases = define "collectTestCases" $
  doc "Collect all test cases from a test group (flattening hierarchy)" $
  lambda "tg" $
    Lists.concat2
      (project _TestGroup _TestGroup_cases @@ var "tg")
      (Lists.concat (Lists.map (lambda "sg" $ collectTestCases @@ var "sg")
        (project _TestGroup _TestGroup_subgroups @@ var "tg")))


-- | Build a Term representing a topologicalSort function call
buildTopologicalSortCall :: TBinding ([(Int, [Int])] -> Term)
buildTopologicalSortCall = define "buildTopologicalSortCall" $
  doc "Build a Term representing a topologicalSort function call" $
  lambda "adjList" $
    Core.termApplication $ Core.application
      (Core.termVariable $ Core.name (string "hydra.sorting.topologicalSort"))
      (encodeAdjacencyList @@ var "adjList")


-- | Build a Term representing a topologicalSortComponents function call
buildTopologicalSortSCCCall :: TBinding ([(Int, [Int])] -> Term)
buildTopologicalSortSCCCall = define "buildTopologicalSortSCCCall" $
  doc "Build a Term representing a topologicalSortComponents function call" $
  lambda "adjList" $
    Core.termApplication $ Core.application
      (Core.termVariable $ Core.name (string "hydra.sorting.topologicalSortComponents"))
      (encodeAdjacencyList @@ var "adjList")


-- | Encode an adjacency list as a Term
encodeAdjacencyList :: TBinding ([(Int, [Int])] -> Term)
encodeAdjacencyList = define "encodeAdjacencyList" $
  doc "Encode an adjacency list as a Term" $
  lambda "pairs" $
    Core.termList (Lists.map
      (lambda "p" $
        Core.termPair (pair
          (encodeInt @@ (Pairs.first (var "p")))
          (Core.termList (Lists.map (lambda "d" $ encodeInt @@ var "d") (Pairs.second (var "p"))))))
      (var "pairs"))


-- | Encode an Int as a Term
encodeInt :: TBinding (Int -> Term)
encodeInt = define "encodeInt" $
  doc "Encode an Int as a Term" $
  lambda "n" $
    Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 (var "n")


-- | Encode Either [[Int]] [Int] as a Term
encodeEitherListList :: TBinding (Either [[Int]] [Int] -> Term)
encodeEitherListList = define "encodeEitherListList" $
  doc "Encode Either [[Int]] [Int] as a Term" $
  lambda "e" $
    Core.termEither (Eithers.bimap
      (lambda "cycles" $ encodeListList @@ var "cycles")
      (lambda "sorted" $ encodeIntList @@ var "sorted")
      (var "e"))


-- | Encode [[Int]] as a Term
encodeListList :: TBinding ([[Int]] -> Term)
encodeListList = define "encodeListList" $
  doc "Encode [[Int]] as a Term" $
  lambda "lists" $
    Core.termList (Lists.map (lambda "l" $ encodeIntList @@ var "l") (var "lists"))


-- | Encode [Int] as a Term
encodeIntList :: TBinding ([Int] -> Term)
encodeIntList = define "encodeIntList" $
  doc "Encode [Int] as a Term" $
  lambda "ints" $
    Core.termList (Lists.map (lambda "n" $ encodeInt @@ var "n") (var "ints"))

