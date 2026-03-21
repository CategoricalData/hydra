-- Note: this is an automatically generated file. Do not edit.

-- | Transform test cases for code generation, filtering to tests that can be compiled to target languages

module Hydra.Test.Transform where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Testing as Testing
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Transform test group hierarchy to only include delegated evaluation tests
transformToCompiledTests :: Testing.TestGroup -> Maybe Testing.TestGroup
transformToCompiledTests tg =

      let name_ = Testing.testGroupName tg
          desc = Testing.testGroupDescription tg
          subgroups = Testing.testGroupSubgroups tg
          cases_ = Testing.testGroupCases tg
          transformedCases = Maybes.cat (Lists.map (\tc -> transformTestCase tc) cases_)
          transformedSubgroups = Maybes.cat (Lists.map (\sg -> transformToCompiledTests sg) subgroups)
      in (Logic.ifElse (Logic.and (Lists.null transformedCases) (Lists.null transformedSubgroups)) Nothing (Just (Testing.TestGroup {
        Testing.testGroupName = name_,
        Testing.testGroupDescription = desc,
        Testing.testGroupSubgroups = transformedSubgroups,
        Testing.testGroupCases = transformedCases})))

-- | Transform a test case to DelegatedEvaluationTestCase if applicable
transformTestCase :: Testing.TestCaseWithMetadata -> Maybe Testing.TestCaseWithMetadata
transformTestCase tcm =

      let name_ = Testing.testCaseWithMetadataName tcm
          tc = Testing.testCaseWithMetadataCase tcm
          desc = Testing.testCaseWithMetadataDescription tcm
          tags_ = Testing.testCaseWithMetadataTags tcm
      in case tc of
        Testing.TestCaseCaseConversion v0 ->
          let fromConv = Testing.caseConversionTestCaseFromConvention v0
              toConv = Testing.caseConversionTestCaseToConvention v0
              fromStr = Testing.caseConversionTestCaseFromString v0
              toStr = Testing.caseConversionTestCaseToString v0
          in (Just (Testing.TestCaseWithMetadata {
            Testing.testCaseWithMetadataName = name_,
            Testing.testCaseWithMetadataCase = (Testing.TestCaseDelegatedEvaluation (Testing.DelegatedEvaluationTestCase {
              Testing.delegatedEvaluationTestCaseInput = (buildConvertCaseCall fromConv toConv fromStr),
              Testing.delegatedEvaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString toStr))})),
            Testing.testCaseWithMetadataDescription = desc,
            Testing.testCaseWithMetadataTags = tags_}))
        Testing.TestCaseEvaluation v0 ->
          let input_ = Testing.evaluationTestCaseInput v0
              output_ = Testing.evaluationTestCaseOutput v0
          in (Just (Testing.TestCaseWithMetadata {
            Testing.testCaseWithMetadataName = name_,
            Testing.testCaseWithMetadataCase = (Testing.TestCaseDelegatedEvaluation (Testing.DelegatedEvaluationTestCase {
              Testing.delegatedEvaluationTestCaseInput = input_,
              Testing.delegatedEvaluationTestCaseOutput = output_})),
            Testing.testCaseWithMetadataDescription = desc,
            Testing.testCaseWithMetadataTags = tags_}))
        Testing.TestCaseDelegatedEvaluation _ -> Just tcm
        Testing.TestCaseTopologicalSort v0 ->
          let adjList = Testing.topologicalSortTestCaseAdjacencyList v0
              expected = Testing.topologicalSortTestCaseExpected v0
          in (Just (Testing.TestCaseWithMetadata {
            Testing.testCaseWithMetadataName = name_,
            Testing.testCaseWithMetadataCase = (Testing.TestCaseDelegatedEvaluation (Testing.DelegatedEvaluationTestCase {
              Testing.delegatedEvaluationTestCaseInput = (buildTopologicalSortCall adjList),
              Testing.delegatedEvaluationTestCaseOutput = (encodeEitherListList expected)})),
            Testing.testCaseWithMetadataDescription = desc,
            Testing.testCaseWithMetadataTags = tags_}))
        Testing.TestCaseTopologicalSortSCC v0 ->
          let adjList = Testing.topologicalSortSCCTestCaseAdjacencyList v0
              expected = Testing.topologicalSortSCCTestCaseExpected v0
          in (Just (Testing.TestCaseWithMetadata {
            Testing.testCaseWithMetadataName = name_,
            Testing.testCaseWithMetadataCase = (Testing.TestCaseDelegatedEvaluation (Testing.DelegatedEvaluationTestCase {
              Testing.delegatedEvaluationTestCaseInput = (buildTopologicalSortSCCCall adjList),
              Testing.delegatedEvaluationTestCaseOutput = (encodeListList expected)})),
            Testing.testCaseWithMetadataDescription = desc,
            Testing.testCaseWithMetadataTags = tags_}))
        Testing.TestCaseValidateCoreTerm _ -> Just tcm
        _ -> Nothing

-- | Build a Term representing a convertCase function call
buildConvertCaseCall :: Util.CaseConvention -> Util.CaseConvention -> String -> Core.Term
buildConvertCaseCall fromConv toConv input_ =
    Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.formatting.convertCase")),
          Core.applicationArgument = (encodeCaseConvention fromConv)})),
        Core.applicationArgument = (encodeCaseConvention toConv)})),
      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString input_))})

-- | Encode CaseConvention as a Term (unit variant)
encodeCaseConvention :: Util.CaseConvention -> Core.Term
encodeCaseConvention conv =
    Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = case conv of
          Util.CaseConventionLowerSnake -> Core.Name "lowerSnake"
          Util.CaseConventionUpperSnake -> Core.Name "upperSnake"
          Util.CaseConventionCamel -> Core.Name "camel"
          Util.CaseConventionPascal -> Core.Name "pascal",
        Core.fieldTerm = Core.TermUnit}})

-- | Add generation namespace prefix
addGenerationPrefix :: Module.Namespace -> Module.Namespace
addGenerationPrefix ns_ = Module.Namespace (Strings.cat2 "generation." (Module.unNamespace ns_))

-- | Transform module with generation namespace
transformModule :: Module.Module -> Module.Module
transformModule m =
    Module.Module {
      Module.moduleNamespace = (addGenerationPrefix (Module.moduleNamespace m)),
      Module.moduleElements = (Module.moduleElements m),
      Module.moduleTermDependencies = (Module.moduleTermDependencies m),
      Module.moduleTypeDependencies = (Module.moduleTypeDependencies m),
      Module.moduleDescription = (Module.moduleDescription m)}

-- | Collect all test cases from a test group (flattening hierarchy)
collectTestCases :: Testing.TestGroup -> [Testing.TestCaseWithMetadata]
collectTestCases tg =
    Lists.concat2 (Testing.testGroupCases tg) (Lists.concat (Lists.map (\sg -> collectTestCases sg) (Testing.testGroupSubgroups tg)))

-- | Build a Term representing a topologicalSort function call
buildTopologicalSortCall :: [(Int, [Int])] -> Core.Term
buildTopologicalSortCall adjList =
    Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.sorting.topologicalSort")),
      Core.applicationArgument = (encodeAdjacencyList adjList)})

-- | Build a Term representing a topologicalSortComponents function call
buildTopologicalSortSCCCall :: [(Int, [Int])] -> Core.Term
buildTopologicalSortSCCCall adjList =
    Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.sorting.topologicalSortComponents")),
      Core.applicationArgument = (encodeAdjacencyList adjList)})

-- | Encode an adjacency list as a Term
encodeAdjacencyList :: [(Int, [Int])] -> Core.Term
encodeAdjacencyList pairs =
    Core.TermList (Lists.map (\p -> Core.TermPair (encodeInt (Pairs.first p), (Core.TermList (Lists.map (\d -> encodeInt d) (Pairs.second p))))) pairs)

-- | Encode an Int as a Term
encodeInt :: Int -> Core.Term
encodeInt n = Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 n))

-- | Encode Either [[Int]] [Int] as a Term
encodeEitherListList :: Either [[Int]] [Int] -> Core.Term
encodeEitherListList e =
    Core.TermEither (Eithers.bimap (\cycles -> encodeListList cycles) (\sorted -> encodeIntList sorted) e)

-- | Encode [[Int]] as a Term
encodeListList :: [[Int]] -> Core.Term
encodeListList lists = Core.TermList (Lists.map (\l -> encodeIntList l) lists)

-- | Encode [Int] as a Term
encodeIntList :: [Int] -> Core.Term
encodeIntList ints = Core.TermList (Lists.map (\n -> encodeInt n) ints)
