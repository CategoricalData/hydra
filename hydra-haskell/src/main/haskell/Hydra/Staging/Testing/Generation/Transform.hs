module Hydra.Staging.Testing.Generation.Transform where

import Hydra.Kernel
import Hydra.Testing
import qualified Hydra.Dsl.Terms as Terms
import qualified Data.Maybe as Y
import qualified Data.Map as M
import qualified Data.List as L

-- | Tag for tests that require the interpreter and cannot be compiled
tag_requiresInterp :: Tag
tag_requiresInterp = Tag "requiresInterp"

-- | Tag for tests that use references to kernel definitions
tag_usesKernelRefs :: Tag
tag_usesKernelRefs = Tag "usesKernelRefs"

-- | Transform test group hierarchy to only include delegated evaluation tests
-- Returns Nothing if the group becomes empty after filtering
transformToCompiledTests :: TestGroup -> Maybe TestGroup
transformToCompiledTests (TestGroup name desc subgroups cases) =
  let transformedCases = Y.mapMaybe transformTestCase cases
      transformedSubgroups = Y.mapMaybe transformToCompiledTests subgroups
  in if null transformedCases && null transformedSubgroups
     then Nothing
     else Just $ TestGroup name desc transformedSubgroups transformedCases

-- | Transform a test case to DelegatedEvaluationTestCase if applicable
-- Returns Nothing if the test case type cannot be translated or uses kernel references
transformTestCase :: TestCaseWithMetadata -> Maybe TestCaseWithMetadata
transformTestCase tcase@(TestCaseWithMetadata name tc desc tags)
  -- Filter out tests that use references to kernel definitions (these can't be compiled)
  -- Note: tag_requiresInterp tests ARE included here; they can be compiled but not interpreted
  | tag_usesKernelRefs `L.elem` tags = Nothing
  | otherwise = case tc of
    -- Case conversion: create delegated evaluation with convertCase call
    TestCaseCaseConversion (CaseConversionTestCase fromConv toConv fromStr toStr) ->
      Just $ TestCaseWithMetadata name delegated desc tags
      where
        delegated = TestCaseDelegatedEvaluation $ DelegatedEvaluationTestCase {
          delegatedEvaluationTestCaseInput = buildConvertCaseCall fromConv toConv fromStr,
          delegatedEvaluationTestCaseOutput = Terms.string toStr
        }

    -- Evaluation: convert directly to delegated evaluation
    TestCaseEvaluation (EvaluationTestCase _style input output) ->
      Just $ TestCaseWithMetadata name delegated desc tags
      where
        delegated = TestCaseDelegatedEvaluation $ DelegatedEvaluationTestCase {
          delegatedEvaluationTestCaseInput = input,
          delegatedEvaluationTestCaseOutput = output
        }

    -- Already delegated evaluation: keep as-is
    TestCaseDelegatedEvaluation _ ->
      Just tcase

    TestCaseTopologicalSort (TopologicalSortTestCase adjList expected) ->
      Just $ TestCaseWithMetadata name delegated desc tags
      where
        delegated = TestCaseDelegatedEvaluation $ DelegatedEvaluationTestCase {
          delegatedEvaluationTestCaseInput = buildTopologicalSortCall adjList,
          delegatedEvaluationTestCaseOutput = encodeEitherListList expected
        }

    TestCaseTopologicalSortSCC (TopologicalSortSCCTestCase adjList expected) ->
      Just $ TestCaseWithMetadata name delegated desc tags
      where
        delegated = TestCaseDelegatedEvaluation $ DelegatedEvaluationTestCase {
          delegatedEvaluationTestCaseInput = buildTopologicalSortSCCCall adjList,
          delegatedEvaluationTestCaseOutput = encodeListList expected
        }

    -- Other test types: exclude (not applicable for compiled tests)
    TestCaseAlphaConversion _ -> Nothing
    TestCaseInference _ -> Nothing
    TestCaseInferenceFailure _ -> Nothing
    TestCaseTypeChecking _ -> Nothing
    TestCaseTypeCheckingFailure _ -> Nothing
    TestCaseTypeReduction _ -> Nothing
    TestCaseEtaExpansion _ -> Nothing
    TestCaseJsonCoder _ -> Nothing
    TestCaseJsonWriter _ -> Nothing
    TestCaseJsonParser _ -> Nothing
    TestCaseSerialization _ -> Nothing
    TestCaseFlattenLetTerms _ -> Nothing
    TestCaseFreeVariables _ -> Nothing
    TestCaseLiftLambdaAboveLet _ -> Nothing
    TestCaseSimplifyTerm _ -> Nothing
    TestCaseDeannotateTerm _ -> Nothing
    TestCaseDeannotateType _ -> Nothing
    TestCaseTopologicalSortBindings _ -> Nothing
    TestCaseNormalizeTypeVariables _ -> Nothing
    TestCaseFoldOverTerm _ -> Nothing
    TestCaseRewriteTerm _ -> Nothing
    TestCaseRewriteType _ -> Nothing

-- | Build a Term representing a convertCase function call
buildConvertCaseCall :: CaseConvention -> CaseConvention -> String -> Term
buildConvertCaseCall fromConv toConv input =
  TermApplication $ Application {
    applicationFunction = TermApplication $ Application {
      applicationFunction = TermApplication $ Application {
        applicationFunction = TermVariable convertCaseName,
        applicationArgument = encodeCaseConvention fromConv
      },
      applicationArgument = encodeCaseConvention toConv
    },
    applicationArgument = Terms.string input
  }
  where
    convertCaseName = Name "hydra.formatting.convertCase"

-- | Encode CaseConvention as a Term (unit variant)
encodeCaseConvention :: CaseConvention -> Term
encodeCaseConvention conv =
  TermUnion $ Injection _CaseConvention (Field fieldName TermUnit)
  where
    fieldName = case conv of
      CaseConventionLowerSnake -> _CaseConvention_lowerSnake
      CaseConventionUpperSnake -> _CaseConvention_upperSnake
      CaseConventionCamel -> _CaseConvention_camel
      CaseConventionPascal -> _CaseConvention_pascal

-- | Add "generation" namespace prefix
addGenerationPrefix :: Namespace -> Namespace
addGenerationPrefix (Namespace ns) = Namespace ("generation." ++ ns)

-- | Transform module with generation namespace
transformModule :: Module -> Module
transformModule (Module ns elements deps schemaDeps desc) =
  Module (addGenerationPrefix ns) elements deps schemaDeps desc

-- | Collect all test cases from a test group (flattening hierarchy)
collectTestCases :: TestGroup -> [TestCaseWithMetadata]
collectTestCases (TestGroup _ _ subgroups cases) =
  cases ++ concatMap collectTestCases subgroups

-- | Build a Term representing a topologicalSort function call
buildTopologicalSortCall :: [(Int, [Int])] -> Term
buildTopologicalSortCall adjList =
  TermApplication $ Application {
    applicationFunction = TermVariable topologicalSortName,
    applicationArgument = encodeAdjacencyList adjList
  }
  where
    topologicalSortName = Name "hydra.sorting.topologicalSort"

-- | Build a Term representing a topologicalSortComponents function call
buildTopologicalSortSCCCall :: [(Int, [Int])] -> Term
buildTopologicalSortSCCCall adjList =
  TermApplication $ Application {
    applicationFunction = TermVariable topologicalSortSCCName,
    applicationArgument = encodeAdjacencyList adjList
  }
  where
    topologicalSortSCCName = Name "hydra.sorting.topologicalSortComponents"

-- | Encode an adjacency list as a Term
encodeAdjacencyList :: [(Int, [Int])] -> Term
encodeAdjacencyList pairs = Terms.list (encodePair <$> pairs)
  where
    encodePair (node, deps) = Terms.pair (encodeInt node) (Terms.list (encodeInt <$> deps))

-- | Encode an Int as a Term
encodeInt :: Int -> Term
encodeInt = Terms.int32 . fromIntegral

-- | Encode Either [[Int]] [Int] as a Term
encodeEitherListList :: Either [[Int]] [Int] -> Term
encodeEitherListList (Left cycles) = Terms.left (encodeListList cycles)
encodeEitherListList (Right sorted) = Terms.right (encodeIntList sorted)

-- | Encode [[Int]] as a Term
encodeListList :: [[Int]] -> Term
encodeListList = Terms.list . fmap encodeIntList

-- | Encode [Int] as a Term
encodeIntList :: [Int] -> Term
encodeIntList = Terms.list . fmap encodeInt
