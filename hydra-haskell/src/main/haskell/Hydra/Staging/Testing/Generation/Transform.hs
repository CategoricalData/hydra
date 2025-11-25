module Hydra.Staging.Testing.Generation.Transform where

import Hydra.Kernel
import Hydra.Testing
import qualified Hydra.Dsl.Terms as Terms
import qualified Data.Maybe as Y
import qualified Data.Map as M

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
-- Returns Nothing if the test case type cannot be translated
transformTestCase :: TestCaseWithMetadata -> Maybe TestCaseWithMetadata
transformTestCase tcase@(TestCaseWithMetadata name tc desc tags) =
  case tc of
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

    -- Other test types: exclude (not applicable for compiled tests)
    TestCaseInference _ -> Nothing
    TestCaseInferenceFailure _ -> Nothing
    TestCaseTypeChecking _ -> Nothing
    TestCaseTypeCheckingFailure _ -> Nothing
    TestCaseEtaExpansion _ -> Nothing

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
