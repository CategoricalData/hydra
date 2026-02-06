-- Note: this is an automatically generated file. Do not edit.

-- | A model for unit testing

module Hydra.Testing where

import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Module as Module
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A test case which performs alpha conversion (variable renaming) on a term and compares the result with the expected term
data AlphaConversionTestCase = 
  AlphaConversionTestCase {
    -- | The term on which to perform alpha conversion
    alphaConversionTestCaseTerm :: Core.Term,
    -- | The variable name to replace
    alphaConversionTestCaseOldVariable :: Core.Name,
    -- | The new variable name
    alphaConversionTestCaseNewVariable :: Core.Name,
    -- | The expected result term after alpha conversion
    alphaConversionTestCaseResult :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_AlphaConversionTestCase = (Core.Name "hydra.testing.AlphaConversionTestCase")

_AlphaConversionTestCase_term = (Core.Name "term")

_AlphaConversionTestCase_oldVariable = (Core.Name "oldVariable")

_AlphaConversionTestCase_newVariable = (Core.Name "newVariable")

_AlphaConversionTestCase_result = (Core.Name "result")

-- | One of two evaluation styles: eager or lazy
data EvaluationStyle = 
  EvaluationStyleEager  |
  EvaluationStyleLazy 
  deriving (Eq, Ord, Read, Show)

_EvaluationStyle = (Core.Name "hydra.testing.EvaluationStyle")

_EvaluationStyle_eager = (Core.Name "eager")

_EvaluationStyle_lazy = (Core.Name "lazy")

-- | A test case which checks that strings are converted between different case conventions correctly
data CaseConversionTestCase = 
  CaseConversionTestCase {
    -- | The source case convention
    caseConversionTestCaseFromConvention :: Util.CaseConvention,
    -- | The target case convention
    caseConversionTestCaseToConvention :: Util.CaseConvention,
    -- | The input string
    caseConversionTestCaseFromString :: String,
    -- | The expected output string
    caseConversionTestCaseToString :: String}
  deriving (Eq, Ord, Read, Show)

_CaseConversionTestCase = (Core.Name "hydra.testing.CaseConversionTestCase")

_CaseConversionTestCase_fromConvention = (Core.Name "fromConvention")

_CaseConversionTestCase_toConvention = (Core.Name "toConvention")

_CaseConversionTestCase_fromString = (Core.Name "fromString")

_CaseConversionTestCase_toString = (Core.Name "toString")

-- | A test case in which we delegate evaluation of an input term and an expected output term to a target programming language like Haskell, Java, or Python, checking whether the term evaluates as expected when translated into that language
data DelegatedEvaluationTestCase = 
  DelegatedEvaluationTestCase {
    -- | The first of two terms which should evaluate to the same expression
    delegatedEvaluationTestCaseInput :: Core.Term,
    -- | The second of two terms which should evaluate to the same expression
    delegatedEvaluationTestCaseOutput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_DelegatedEvaluationTestCase = (Core.Name "hydra.testing.DelegatedEvaluationTestCase")

_DelegatedEvaluationTestCase_input = (Core.Name "input")

_DelegatedEvaluationTestCase_output = (Core.Name "output")

-- | A test case which performs eta expansion (adding missing lambda abstractions) on a given term and compares the result with the expected result
data EtaExpansionTestCase = 
  EtaExpansionTestCase {
    -- | The term to eta expand
    etaExpansionTestCaseInput :: Core.Term,
    -- | The expected result
    etaExpansionTestCaseOutput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_EtaExpansionTestCase = (Core.Name "hydra.testing.EtaExpansionTestCase")

_EtaExpansionTestCase_input = (Core.Name "input")

_EtaExpansionTestCase_output = (Core.Name "output")

-- | A test case which strips all annotations from a term and compares the result with the expected term
data DeannotateTermTestCase = 
  DeannotateTermTestCase {
    -- | The term to deannotate
    deannotateTermTestCaseInput :: Core.Term,
    -- | The expected deannotated term
    deannotateTermTestCaseOutput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_DeannotateTermTestCase = (Core.Name "hydra.testing.DeannotateTermTestCase")

_DeannotateTermTestCase_input = (Core.Name "input")

_DeannotateTermTestCase_output = (Core.Name "output")

-- | A test case which strips all annotations from a type and compares the result with the expected type
data DeannotateTypeTestCase = 
  DeannotateTypeTestCase {
    -- | The type to deannotate
    deannotateTypeTestCaseInput :: Core.Type,
    -- | The expected deannotated type
    deannotateTypeTestCaseOutput :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_DeannotateTypeTestCase = (Core.Name "hydra.testing.DeannotateTypeTestCase")

_DeannotateTypeTestCase_input = (Core.Name "input")

_DeannotateTypeTestCase_output = (Core.Name "output")

-- | A test case which flattens nested let terms, lifting inner bindings to the outer let, and compares the result with the expected term
data FlattenLetTermsTestCase = 
  FlattenLetTermsTestCase {
    -- | The term to flatten
    flattenLetTermsTestCaseInput :: Core.Term,
    -- | The expected flattened term
    flattenLetTermsTestCaseOutput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_FlattenLetTermsTestCase = (Core.Name "hydra.testing.FlattenLetTermsTestCase")

_FlattenLetTermsTestCase_input = (Core.Name "input")

_FlattenLetTermsTestCase_output = (Core.Name "output")

-- | A predefined fold operation for testing foldOverTerm
data FoldOperation = 
  -- | Sum all Int32 literals in a term
  FoldOperationSumInt32Literals  |
  -- | Collect the lengths of all list terms (returns list of integers in traversal order)
  FoldOperationCollectListLengths  |
  -- | Collect labels (first element of pairs where first is a string literal)
  FoldOperationCollectLabels 
  deriving (Eq, Ord, Read, Show)

_FoldOperation = (Core.Name "hydra.testing.FoldOperation")

_FoldOperation_sumInt32Literals = (Core.Name "sumInt32Literals")

_FoldOperation_collectListLengths = (Core.Name "collectListLengths")

_FoldOperation_collectLabels = (Core.Name "collectLabels")

-- | A test case which applies a fold operation over a term and compares the result
data FoldOverTermTestCase = 
  FoldOverTermTestCase {
    -- | The term to fold over
    foldOverTermTestCaseInput :: Core.Term,
    -- | The traversal order (pre or post)
    foldOverTermTestCaseTraversalOrder :: Coders.TraversalOrder,
    -- | The fold operation to apply
    foldOverTermTestCaseOperation :: FoldOperation,
    -- | The expected result of the fold
    foldOverTermTestCaseOutput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_FoldOverTermTestCase = (Core.Name "hydra.testing.FoldOverTermTestCase")

_FoldOverTermTestCase_input = (Core.Name "input")

_FoldOverTermTestCase_traversalOrder = (Core.Name "traversalOrder")

_FoldOverTermTestCase_operation = (Core.Name "operation")

_FoldOverTermTestCase_output = (Core.Name "output")

-- | A test case which computes the free variables of a term and compares the result with an expected set of names
data FreeVariablesTestCase = 
  FreeVariablesTestCase {
    -- | The term to analyze
    freeVariablesTestCaseInput :: Core.Term,
    -- | The expected set of free variable names
    freeVariablesTestCaseOutput :: (S.Set Core.Name)}
  deriving (Eq, Ord, Read, Show)

_FreeVariablesTestCase = (Core.Name "hydra.testing.FreeVariablesTestCase")

_FreeVariablesTestCase_input = (Core.Name "input")

_FreeVariablesTestCase_output = (Core.Name "output")

-- | A predefined predicate for testing hoistSubterms. Each predicate determines which subterms should be hoisted into let bindings.
data HoistPredicate = 
  -- | Hoist case statements (elimination unions) that appear in non-top-level positions
  HoistPredicateCaseStatements  |
  -- | Hoist function applications that appear in non-top-level positions
  HoistPredicateApplications  |
  -- | Hoist list terms that appear in non-top-level positions
  HoistPredicateLists  |
  -- | Never hoist anything (identity transformation for let terms)
  HoistPredicateNothing 
  deriving (Eq, Ord, Read, Show)

_HoistPredicate = (Core.Name "hydra.testing.HoistPredicate")

_HoistPredicate_caseStatements = (Core.Name "caseStatements")

_HoistPredicate_applications = (Core.Name "applications")

_HoistPredicate_lists = (Core.Name "lists")

_HoistPredicate_nothing = (Core.Name "nothing")

-- | A test case for hoistLetBindings with hoistAll=True, which hoists ALL nested let bindings to the top level of a let term, not just polymorphic ones. This is used for targets like Java that cannot have let expressions in arbitrary positions.
data HoistLetBindingsTestCase = 
  HoistLetBindingsTestCase {
    -- | The input let term
    hoistLetBindingsTestCaseInput :: Core.Let,
    -- | The expected output let term with all nested bindings hoisted to top
    hoistLetBindingsTestCaseOutput :: Core.Let}
  deriving (Eq, Ord, Read, Show)

_HoistLetBindingsTestCase = (Core.Name "hydra.testing.HoistLetBindingsTestCase")

_HoistLetBindingsTestCase_input = (Core.Name "input")

_HoistLetBindingsTestCase_output = (Core.Name "output")

-- | A test case for the hoistPolymorphicLetBindings function, which hoists polymorphic let bindings to the top level of a let term. This is used for targets like Java which don't support polymorphic lambdas.
data HoistPolymorphicLetBindingsTestCase = 
  HoistPolymorphicLetBindingsTestCase {
    -- | The input let term
    hoistPolymorphicLetBindingsTestCaseInput :: Core.Let,
    -- | The expected output let term with polymorphic bindings hoisted to top
    hoistPolymorphicLetBindingsTestCaseOutput :: Core.Let}
  deriving (Eq, Ord, Read, Show)

_HoistPolymorphicLetBindingsTestCase = (Core.Name "hydra.testing.HoistPolymorphicLetBindingsTestCase")

_HoistPolymorphicLetBindingsTestCase_input = (Core.Name "input")

_HoistPolymorphicLetBindingsTestCase_output = (Core.Name "output")

-- | A test case which hoists subterms into let bindings based on a predicate, and compares the result with the expected term. The predicate decides which subterms at which positions should be extracted into new bindings.
data HoistSubtermsTestCase = 
  HoistSubtermsTestCase {
    -- | The predicate that determines which subterms to hoist
    hoistSubtermsTestCasePredicate :: HoistPredicate,
    -- | The input term (must contain a let expression for hoisting to occur)
    hoistSubtermsTestCaseInput :: Core.Term,
    -- | The expected output term with hoisted subterms as new bindings
    hoistSubtermsTestCaseOutput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_HoistSubtermsTestCase = (Core.Name "hydra.testing.HoistSubtermsTestCase")

_HoistSubtermsTestCase_predicate = (Core.Name "predicate")

_HoistSubtermsTestCase_input = (Core.Name "input")

_HoistSubtermsTestCase_output = (Core.Name "output")

-- | A test case for the hoistCaseStatements function, which hoists case statements into let bindings, but only when they appear inside a lambda body. This is used for targets like Python which don't support inline match expressions.
data HoistCaseStatementsTestCase = 
  HoistCaseStatementsTestCase {
    -- | The input term
    hoistCaseStatementsTestCaseInput :: Core.Term,
    -- | The expected output term with hoisted case statements
    hoistCaseStatementsTestCaseOutput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_HoistCaseStatementsTestCase = (Core.Name "hydra.testing.HoistCaseStatementsTestCase")

_HoistCaseStatementsTestCase_input = (Core.Name "input")

_HoistCaseStatementsTestCase_output = (Core.Name "output")

-- | A predefined term rewriter for testing rewriteTerm
data TermRewriter = 
  -- | Replace all string literal 'foo' with 'bar'
  TermRewriterReplaceFooWithBar  |
  -- | Replace all Int32 literals with Int64 literals of the same value
  TermRewriterReplaceInt32WithInt64 
  deriving (Eq, Ord, Read, Show)

_TermRewriter = (Core.Name "hydra.testing.TermRewriter")

_TermRewriter_replaceFooWithBar = (Core.Name "replaceFooWithBar")

_TermRewriter_replaceInt32WithInt64 = (Core.Name "replaceInt32WithInt64")

-- | A test case which applies a term rewriter and compares the result
data RewriteTermTestCase = 
  RewriteTermTestCase {
    -- | The term to rewrite
    rewriteTermTestCaseInput :: Core.Term,
    -- | The rewriter to apply
    rewriteTermTestCaseRewriter :: TermRewriter,
    -- | The expected rewritten term
    rewriteTermTestCaseOutput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_RewriteTermTestCase = (Core.Name "hydra.testing.RewriteTermTestCase")

_RewriteTermTestCase_input = (Core.Name "input")

_RewriteTermTestCase_rewriter = (Core.Name "rewriter")

_RewriteTermTestCase_output = (Core.Name "output")

-- | A predefined type rewriter for testing rewriteType
data TypeRewriter = 
  -- | Replace all String types with Int32 types
  TypeRewriterReplaceStringWithInt32 
  deriving (Eq, Ord, Read, Show)

_TypeRewriter = (Core.Name "hydra.testing.TypeRewriter")

_TypeRewriter_replaceStringWithInt32 = (Core.Name "replaceStringWithInt32")

-- | A test case which applies a type rewriter and compares the result
data RewriteTypeTestCase = 
  RewriteTypeTestCase {
    -- | The type to rewrite
    rewriteTypeTestCaseInput :: Core.Type,
    -- | The rewriter to apply
    rewriteTypeTestCaseRewriter :: TypeRewriter,
    -- | The expected rewritten type
    rewriteTypeTestCaseOutput :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_RewriteTypeTestCase = (Core.Name "hydra.testing.RewriteTypeTestCase")

_RewriteTypeTestCase_input = (Core.Name "input")

_RewriteTypeTestCase_rewriter = (Core.Name "rewriter")

_RewriteTypeTestCase_output = (Core.Name "output")

-- | A test case which evaluates (reduces) a given term and compares it with the expected result
data EvaluationTestCase = 
  EvaluationTestCase {
    -- | The evaluation style (eager or lazy)
    evaluationTestCaseEvaluationStyle :: EvaluationStyle,
    -- | The term to evaluate
    evaluationTestCaseInput :: Core.Term,
    -- | The expected result
    evaluationTestCaseOutput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_EvaluationTestCase = (Core.Name "hydra.testing.EvaluationTestCase")

_EvaluationTestCase_evaluationStyle = (Core.Name "evaluationStyle")

_EvaluationTestCase_input = (Core.Name "input")

_EvaluationTestCase_output = (Core.Name "output")

-- | A test case providing a term for which type inference is expected to fail
data InferenceFailureTestCase = 
  InferenceFailureTestCase {
    -- | The term for which inference should fail
    inferenceFailureTestCaseInput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_InferenceFailureTestCase = (Core.Name "hydra.testing.InferenceFailureTestCase")

_InferenceFailureTestCase_input = (Core.Name "input")

-- | A test case which performs type inference on a given term and compares the result with an expected type scheme
data InferenceTestCase = 
  InferenceTestCase {
    -- | The term to infer
    inferenceTestCaseInput :: Core.Term,
    -- | The expected type scheme
    inferenceTestCaseOutput :: Core.TypeScheme}
  deriving (Eq, Ord, Read, Show)

_InferenceTestCase = (Core.Name "hydra.testing.InferenceTestCase")

_InferenceTestCase_input = (Core.Name "input")

_InferenceTestCase_output = (Core.Name "output")

-- | A test case which encodes a Hydra term to JSON using a type-directed coder, and verifies that decoding produces the original term (round-trip)
data JsonCoderTestCase = 
  JsonCoderTestCase {
    -- | The Hydra type that determines how the term is encoded/decoded
    jsonCoderTestCaseType :: Core.Type,
    -- | The Hydra term to encode
    jsonCoderTestCaseTerm :: Core.Term,
    -- | The expected JSON value
    jsonCoderTestCaseJson :: Model.Value}
  deriving (Eq, Ord, Read, Show)

_JsonCoderTestCase = (Core.Name "hydra.testing.JsonCoderTestCase")

_JsonCoderTestCase_type = (Core.Name "type")

_JsonCoderTestCase_term = (Core.Name "term")

_JsonCoderTestCase_json = (Core.Name "json")

-- | A test case for the Either-based JSON decoder. Takes a type, input JSON, and expected result (Either String Term).
data JsonDecodeTestCase = 
  JsonDecodeTestCase {
    -- | The Hydra type to decode into
    jsonDecodeTestCaseType :: Core.Type,
    -- | The input JSON value
    jsonDecodeTestCaseJson :: Model.Value,
    -- | The expected result: Left for error, Right for decoded term
    jsonDecodeTestCaseExpected :: (Either String Core.Term)}
  deriving (Eq, Ord, Read, Show)

_JsonDecodeTestCase = (Core.Name "hydra.testing.JsonDecodeTestCase")

_JsonDecodeTestCase_type = (Core.Name "type")

_JsonDecodeTestCase_json = (Core.Name "json")

_JsonDecodeTestCase_expected = (Core.Name "expected")

-- | A test case for the Either-based JSON encoder. Takes an input term and expected result (Either String Value).
data JsonEncodeTestCase = 
  JsonEncodeTestCase {
    -- | The Hydra term to encode
    jsonEncodeTestCaseTerm :: Core.Term,
    -- | The expected result: Left for error, Right for encoded JSON
    jsonEncodeTestCaseExpected :: (Either String Model.Value)}
  deriving (Eq, Ord, Read, Show)

_JsonEncodeTestCase = (Core.Name "hydra.testing.JsonEncodeTestCase")

_JsonEncodeTestCase_term = (Core.Name "term")

_JsonEncodeTestCase_expected = (Core.Name "expected")

-- | A test case which parses a JSON string and compares the result with an expected JSON value
type JsonParserTestCase = (ParserTestCase Model.Value)

_JsonParserTestCase = (Core.Name "hydra.testing.JsonParserTestCase")

-- | A test case for round-trip encoding/decoding using the Either-based JSON functions. Encodes a term, then decodes it back, verifying the result equals the original.
data JsonRoundtripTestCase = 
  JsonRoundtripTestCase {
    -- | The Hydra type for encoding/decoding
    jsonRoundtripTestCaseType :: Core.Type,
    -- | The Hydra term to round-trip
    jsonRoundtripTestCaseTerm :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_JsonRoundtripTestCase = (Core.Name "hydra.testing.JsonRoundtripTestCase")

_JsonRoundtripTestCase_type = (Core.Name "type")

_JsonRoundtripTestCase_term = (Core.Name "term")

-- | A test case which lifts lambda abstractions above let expressions and compares the result with the expected term
data LiftLambdaAboveLetTestCase = 
  LiftLambdaAboveLetTestCase {
    -- | The term to transform
    liftLambdaAboveLetTestCaseInput :: Core.Term,
    -- | The expected transformed term
    liftLambdaAboveLetTestCaseOutput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_LiftLambdaAboveLetTestCase = (Core.Name "hydra.testing.LiftLambdaAboveLetTestCase")

_LiftLambdaAboveLetTestCase_input = (Core.Name "input")

_LiftLambdaAboveLetTestCase_output = (Core.Name "output")

-- | A test case which serializes a JSON value to a string and compares it to the expected string
type JsonWriterTestCase = (WriterTestCase Model.Value)

_JsonWriterTestCase = (Core.Name "hydra.testing.JsonWriterTestCase")

-- | A test case which parses an input string and compares the result with an expected value
data ParserTestCase a = 
  ParserTestCase {
    -- | The input string to parse
    parserTestCaseInput :: String,
    -- | The expected parse result
    parserTestCaseOutput :: (Parsing.ParseResult a)}
  deriving (Eq, Ord, Read, Show)

_ParserTestCase = (Core.Name "hydra.testing.ParserTestCase")

_ParserTestCase_input = (Core.Name "input")

_ParserTestCase_output = (Core.Name "output")

-- | A tag for categorizing test cases
newtype Tag = 
  Tag {
    unTag :: String}
  deriving (Eq, Ord, Read, Show)

_Tag = (Core.Name "hydra.testing.Tag")

-- | A codec for generating compiled test files from test groups into a target programming language
data TestCodec = 
  TestCodec {
    -- | The name of the target programming language
    testCodecLanguage :: Coders.LanguageName,
    -- | The file extension for test files (e.g., 'hs', 'java', 'py')
    testCodecFileExtension :: Module.FileExtension,
    -- | A function for encoding Hydra terms into the target language
    testCodecEncodeTerm :: (Core.Term -> Compute.Flow Graph.Graph String),
    -- | A function for encoding Hydra types into the target language
    testCodecEncodeType :: (Core.Type -> Compute.Flow Graph.Graph String),
    -- | A function for formatting test case names according to the target language's conventions
    testCodecFormatTestName :: (String -> String),
    -- | A function for formatting module names according to the target language's conventions
    testCodecFormatModuleName :: (Module.Namespace -> String),
    -- | A template string for individual test case assertions
    testCodecTestCaseTemplate :: String,
    -- | A template string for wrapping a group of test cases
    testCodecTestGroupTemplate :: String,
    -- | A template string for the overall test module structure
    testCodecModuleTemplate :: String,
    -- | A template string for import/include statements
    testCodecImportTemplate :: String,
    -- | A function that determines the necessary imports for a given set of dependencies
    testCodecFindImports :: (S.Set Core.Name -> [String])}

_TestCodec = (Core.Name "hydra.testing.TestCodec")

_TestCodec_language = (Core.Name "language")

_TestCodec_fileExtension = (Core.Name "fileExtension")

_TestCodec_encodeTerm = (Core.Name "encodeTerm")

_TestCodec_encodeType = (Core.Name "encodeType")

_TestCodec_formatTestName = (Core.Name "formatTestName")

_TestCodec_formatModuleName = (Core.Name "formatModuleName")

_TestCodec_testCaseTemplate = (Core.Name "testCaseTemplate")

_TestCodec_testGroupTemplate = (Core.Name "testGroupTemplate")

_TestCodec_moduleTemplate = (Core.Name "moduleTemplate")

_TestCodec_importTemplate = (Core.Name "importTemplate")

_TestCodec_findImports = (Core.Name "findImports")

-- | A simple test case with an input and an expected output
data TestCase = 
  -- | An alpha conversion test
  TestCaseAlphaConversion AlphaConversionTestCase |
  -- | A case conversion test
  TestCaseCaseConversion CaseConversionTestCase |
  -- | A deannotate term test
  TestCaseDeannotateTerm DeannotateTermTestCase |
  -- | A deannotate type test
  TestCaseDeannotateType DeannotateTypeTestCase |
  -- | A delegated evaluation test
  TestCaseDelegatedEvaluation DelegatedEvaluationTestCase |
  -- | An eta expansion test
  TestCaseEtaExpansion EtaExpansionTestCase |
  -- | A flatten let terms test
  TestCaseFlattenLetTerms FlattenLetTermsTestCase |
  -- | A free variables test
  TestCaseFreeVariables FreeVariablesTestCase |
  -- | A term evaluation test
  TestCaseEvaluation EvaluationTestCase |
  -- | A type inference test
  TestCaseInference InferenceTestCase |
  -- | A type inference failure test
  TestCaseInferenceFailure InferenceFailureTestCase |
  -- | A JSON coder (round-trip) test using Flow-based coder
  TestCaseJsonCoder JsonCoderTestCase |
  -- | A JSON decode test using Either-based decoder
  TestCaseJsonDecode JsonDecodeTestCase |
  -- | A JSON encode test using Either-based encoder
  TestCaseJsonEncode JsonEncodeTestCase |
  -- | A JSON parser test
  TestCaseJsonParser JsonParserTestCase |
  -- | A JSON round-trip test using Either-based encoder/decoder
  TestCaseJsonRoundtrip JsonRoundtripTestCase |
  -- | A JSON writer test
  TestCaseJsonWriter JsonWriterTestCase |
  -- | A lift lambda above let test
  TestCaseLiftLambdaAboveLet LiftLambdaAboveLetTestCase |
  -- | An AST serialization test
  TestCaseSerialization SerializationTestCase |
  -- | A simplify term test
  TestCaseSimplifyTerm SimplifyTermTestCase |
  -- | A topological sort test
  TestCaseTopologicalSort TopologicalSortTestCase |
  -- | A topological sort bindings test
  TestCaseTopologicalSortBindings TopologicalSortBindingsTestCase |
  -- | A topological sort with SCC detection test
  TestCaseTopologicalSortSCC TopologicalSortSCCTestCase |
  -- | A type checking test
  TestCaseTypeChecking TypeCheckingTestCase |
  -- | A type checking failure test (currently unused)
  TestCaseTypeCheckingFailure TypeCheckingFailureTestCase |
  -- | A type reduction test
  TestCaseTypeReduction TypeReductionTestCase |
  -- | A normalize type variables test
  TestCaseNormalizeTypeVariables NormalizeTypeVariablesTestCase |
  -- | A fold over term test
  TestCaseFoldOverTerm FoldOverTermTestCase |
  -- | A rewrite term test
  TestCaseRewriteTerm RewriteTermTestCase |
  -- | A rewrite type test
  TestCaseRewriteType RewriteTypeTestCase |
  -- | A hoist subterms test
  TestCaseHoistSubterms HoistSubtermsTestCase |
  -- | A hoist case statements test
  TestCaseHoistCaseStatements HoistCaseStatementsTestCase |
  -- | A hoist all let bindings test (hoistAll=True, for Java)
  TestCaseHoistLetBindings HoistLetBindingsTestCase |
  -- | A hoist polymorphic let bindings test
  TestCaseHoistPolymorphicLetBindings HoistPolymorphicLetBindingsTestCase |
  -- | A type substitution test
  TestCaseSubstInType SubstInTypeTestCase |
  -- | An occur check test for type unification
  TestCaseVariableOccursInType VariableOccursInTypeTestCase |
  -- | A type unification test
  TestCaseUnifyTypes UnifyTypesTestCase |
  -- | A join types test (produce type constraints)
  TestCaseJoinTypes JoinTypesTestCase
  deriving (Eq, Ord, Read, Show)

_TestCase = (Core.Name "hydra.testing.TestCase")

_TestCase_alphaConversion = (Core.Name "alphaConversion")

_TestCase_caseConversion = (Core.Name "caseConversion")

_TestCase_deannotateTerm = (Core.Name "deannotateTerm")

_TestCase_deannotateType = (Core.Name "deannotateType")

_TestCase_delegatedEvaluation = (Core.Name "delegatedEvaluation")

_TestCase_etaExpansion = (Core.Name "etaExpansion")

_TestCase_flattenLetTerms = (Core.Name "flattenLetTerms")

_TestCase_freeVariables = (Core.Name "freeVariables")

_TestCase_evaluation = (Core.Name "evaluation")

_TestCase_inference = (Core.Name "inference")

_TestCase_inferenceFailure = (Core.Name "inferenceFailure")

_TestCase_jsonCoder = (Core.Name "jsonCoder")

_TestCase_jsonDecode = (Core.Name "jsonDecode")

_TestCase_jsonEncode = (Core.Name "jsonEncode")

_TestCase_jsonParser = (Core.Name "jsonParser")

_TestCase_jsonRoundtrip = (Core.Name "jsonRoundtrip")

_TestCase_jsonWriter = (Core.Name "jsonWriter")

_TestCase_liftLambdaAboveLet = (Core.Name "liftLambdaAboveLet")

_TestCase_serialization = (Core.Name "serialization")

_TestCase_simplifyTerm = (Core.Name "simplifyTerm")

_TestCase_topologicalSort = (Core.Name "topologicalSort")

_TestCase_topologicalSortBindings = (Core.Name "topologicalSortBindings")

_TestCase_topologicalSortSCC = (Core.Name "topologicalSortSCC")

_TestCase_typeChecking = (Core.Name "typeChecking")

_TestCase_typeCheckingFailure = (Core.Name "typeCheckingFailure")

_TestCase_typeReduction = (Core.Name "typeReduction")

_TestCase_normalizeTypeVariables = (Core.Name "normalizeTypeVariables")

_TestCase_foldOverTerm = (Core.Name "foldOverTerm")

_TestCase_rewriteTerm = (Core.Name "rewriteTerm")

_TestCase_rewriteType = (Core.Name "rewriteType")

_TestCase_hoistSubterms = (Core.Name "hoistSubterms")

_TestCase_hoistCaseStatements = (Core.Name "hoistCaseStatements")

_TestCase_hoistLetBindings = (Core.Name "hoistLetBindings")

_TestCase_hoistPolymorphicLetBindings = (Core.Name "hoistPolymorphicLetBindings")

_TestCase_substInType = (Core.Name "substInType")

_TestCase_variableOccursInType = (Core.Name "variableOccursInType")

_TestCase_unifyTypes = (Core.Name "unifyTypes")

_TestCase_joinTypes = (Core.Name "joinTypes")

-- | One of a number of test case variants, together with metadata including a test name, an optional description, and optional tags
data TestCaseWithMetadata = 
  TestCaseWithMetadata {
    -- | The name of the test case
    testCaseWithMetadataName :: String,
    -- | The test case itself
    testCaseWithMetadataCase :: TestCase,
    -- | An optional description of the test
    testCaseWithMetadataDescription :: (Maybe String),
    -- | Zero or more tags for categorizing the test
    testCaseWithMetadataTags :: [Tag]}
  deriving (Eq, Ord, Read, Show)

_TestCaseWithMetadata = (Core.Name "hydra.testing.TestCaseWithMetadata")

_TestCaseWithMetadata_name = (Core.Name "name")

_TestCaseWithMetadata_case = (Core.Name "case")

_TestCaseWithMetadata_description = (Core.Name "description")

_TestCaseWithMetadata_tags = (Core.Name "tags")

-- | A collection of test cases with a name and optional description
data TestGroup = 
  TestGroup {
    -- | The name of the test group
    testGroupName :: String,
    -- | An optional description of the group
    testGroupDescription :: (Maybe String),
    -- | Nested test groups
    testGroupSubgroups :: [TestGroup],
    -- | The test cases in this group
    testGroupCases :: [TestCaseWithMetadata]}
  deriving (Eq, Ord, Read, Show)

_TestGroup = (Core.Name "hydra.testing.TestGroup")

_TestGroup_name = (Core.Name "name")

_TestGroup_description = (Core.Name "description")

_TestGroup_subgroups = (Core.Name "subgroups")

_TestGroup_cases = (Core.Name "cases")

-- | A test case which performs type checking on a given term and compares the result with an expected annotated term and type
data TypeCheckingTestCase = 
  TypeCheckingTestCase {
    -- | An untyped term on which to perform inference, then type check
    typeCheckingTestCaseInput :: Core.Term,
    -- | The expected fully annotated System F term after type inference
    typeCheckingTestCaseOutputTerm :: Core.Term,
    -- | The expected inferred type
    typeCheckingTestCaseOutputType :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_TypeCheckingTestCase = (Core.Name "hydra.testing.TypeCheckingTestCase")

_TypeCheckingTestCase_input = (Core.Name "input")

_TypeCheckingTestCase_outputTerm = (Core.Name "outputTerm")

_TypeCheckingTestCase_outputType = (Core.Name "outputType")

-- | A test case providing a term for which type checking is expected to fail. Note: there are currently no such test cases.
data TypeCheckingFailureTestCase = 
  TypeCheckingFailureTestCase {
    -- | The term for which type checking should fail
    typeCheckingFailureTestCaseInput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_TypeCheckingFailureTestCase = (Core.Name "hydra.testing.TypeCheckingFailureTestCase")

_TypeCheckingFailureTestCase_input = (Core.Name "input")

-- | A test case which performs topological sort on a map of bindings (name -> term) and compares the result with expected groups of bindings in topological order
data TopologicalSortBindingsTestCase = 
  TopologicalSortBindingsTestCase {
    -- | The bindings as a list of (name, term) pairs
    topologicalSortBindingsTestCaseBindings :: [(Core.Name, Core.Term)],
    -- | The expected groups of bindings in topological order
    topologicalSortBindingsTestCaseExpected :: [[(Core.Name, Core.Term)]]}
  deriving (Eq, Ord, Read, Show)

_TopologicalSortBindingsTestCase = (Core.Name "hydra.testing.TopologicalSortBindingsTestCase")

_TopologicalSortBindingsTestCase_bindings = (Core.Name "bindings")

_TopologicalSortBindingsTestCase_expected = (Core.Name "expected")

-- | A test case which performs topological sort on a directed graph and compares the result with either an expected sorted list or expected cycles
data TopologicalSortTestCase = 
  TopologicalSortTestCase {
    -- | The directed graph as an adjacency list (node to list of dependencies)
    topologicalSortTestCaseAdjacencyList :: [(Int, [Int])],
    -- | The expected result: Left for cycles, Right for sorted nodes
    topologicalSortTestCaseExpected :: (Either [[Int]] [Int])}
  deriving (Eq, Ord, Read, Show)

_TopologicalSortTestCase = (Core.Name "hydra.testing.TopologicalSortTestCase")

_TopologicalSortTestCase_adjacencyList = (Core.Name "adjacencyList")

_TopologicalSortTestCase_expected = (Core.Name "expected")

-- | A test case which performs topological sort with strongly connected component detection and compares the result with expected components
data TopologicalSortSCCTestCase = 
  TopologicalSortSCCTestCase {
    -- | The directed graph as an adjacency list
    topologicalSortSCCTestCaseAdjacencyList :: [(Int, [Int])],
    -- | The expected strongly connected components in topological order
    topologicalSortSCCTestCaseExpected :: [[Int]]}
  deriving (Eq, Ord, Read, Show)

_TopologicalSortSCCTestCase = (Core.Name "hydra.testing.TopologicalSortSCCTestCase")

_TopologicalSortSCCTestCase_adjacencyList = (Core.Name "adjacencyList")

_TopologicalSortSCCTestCase_expected = (Core.Name "expected")

-- | A test case which serializes an AST expression to a string and compares it with the expected output
data SerializationTestCase = 
  SerializationTestCase {
    -- | The AST expression to serialize
    serializationTestCaseInput :: Ast.Expr,
    -- | The expected serialized string
    serializationTestCaseOutput :: String}
  deriving (Eq, Ord, Read, Show)

_SerializationTestCase = (Core.Name "hydra.testing.SerializationTestCase")

_SerializationTestCase_input = (Core.Name "input")

_SerializationTestCase_output = (Core.Name "output")

-- | A test case which performs term simplification (beta reduction and optimization) and compares the result with the expected term
data SimplifyTermTestCase = 
  SimplifyTermTestCase {
    -- | The term to simplify
    simplifyTermTestCaseInput :: Core.Term,
    -- | The expected simplified term
    simplifyTermTestCaseOutput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_SimplifyTermTestCase = (Core.Name "hydra.testing.SimplifyTermTestCase")

_SimplifyTermTestCase_input = (Core.Name "input")

_SimplifyTermTestCase_output = (Core.Name "output")

-- | A test case which normalizes type variables in a term (renaming them to t0, t1, t2, etc.) and compares the result with the expected term
data NormalizeTypeVariablesTestCase = 
  NormalizeTypeVariablesTestCase {
    -- | The term with type annotations to normalize
    normalizeTypeVariablesTestCaseInput :: Core.Term,
    -- | The expected term with normalized type variable names
    normalizeTypeVariablesTestCaseOutput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_NormalizeTypeVariablesTestCase = (Core.Name "hydra.testing.NormalizeTypeVariablesTestCase")

_NormalizeTypeVariablesTestCase_input = (Core.Name "input")

_NormalizeTypeVariablesTestCase_output = (Core.Name "output")

-- | A test case which performs beta reduction on a type (reducing type applications) and compares the result with the expected type
data TypeReductionTestCase = 
  TypeReductionTestCase {
    -- | The type to reduce
    typeReductionTestCaseInput :: Core.Type,
    -- | The expected reduced type
    typeReductionTestCaseOutput :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_TypeReductionTestCase = (Core.Name "hydra.testing.TypeReductionTestCase")

_TypeReductionTestCase_input = (Core.Name "input")

_TypeReductionTestCase_output = (Core.Name "output")

-- | A test case which writes a value to a string and compares it to the expected string
data WriterTestCase a = 
  WriterTestCase {
    -- | The input value to write
    writerTestCaseInput :: a,
    -- | The expected string
    writerTestCaseOutput :: String}
  deriving (Eq, Ord, Read, Show)

_WriterTestCase = (Core.Name "hydra.testing.WriterTestCase")

_WriterTestCase_input = (Core.Name "input")

_WriterTestCase_output = (Core.Name "output")

-- | A test case which applies a type substitution to a type and compares the result. The substitution is provided as a list of (variable name, replacement type) pairs.
data SubstInTypeTestCase = 
  SubstInTypeTestCase {
    -- | The type substitution as a list of (name, type) pairs
    substInTypeTestCaseSubstitution :: [(Core.Name, Core.Type)],
    -- | The type to substitute into
    substInTypeTestCaseInput :: Core.Type,
    -- | The expected result type
    substInTypeTestCaseOutput :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_SubstInTypeTestCase = (Core.Name "hydra.testing.SubstInTypeTestCase")

_SubstInTypeTestCase_substitution = (Core.Name "substitution")

_SubstInTypeTestCase_input = (Core.Name "input")

_SubstInTypeTestCase_output = (Core.Name "output")

-- | A test case which checks whether a type variable occurs in a type expression. This is the occur check used in type unification.
data VariableOccursInTypeTestCase = 
  VariableOccursInTypeTestCase {
    -- | The variable name to search for
    variableOccursInTypeTestCaseVariable :: Core.Name,
    -- | The type to search within
    variableOccursInTypeTestCaseType :: Core.Type,
    -- | Whether the variable occurs in the type
    variableOccursInTypeTestCaseExpected :: Bool}
  deriving (Eq, Ord, Read, Show)

_VariableOccursInTypeTestCase = (Core.Name "hydra.testing.VariableOccursInTypeTestCase")

_VariableOccursInTypeTestCase_variable = (Core.Name "variable")

_VariableOccursInTypeTestCase_type = (Core.Name "type")

_VariableOccursInTypeTestCase_expected = (Core.Name "expected")

-- | A test case which attempts to unify two types and compares the result. The expected result is either Left (failure message substring) or Right (substitution).
data UnifyTypesTestCase = 
  UnifyTypesTestCase {
    -- | The schema types map (type variable names that should not be bound)
    unifyTypesTestCaseSchemaTypes :: [Core.Name],
    -- | The left type to unify
    unifyTypesTestCaseLeft :: Core.Type,
    -- | The right type to unify
    unifyTypesTestCaseRight :: Core.Type,
    -- | The expected result: Left for failure (substring of error), Right for substitution
    unifyTypesTestCaseExpected :: (Either String Typing.TypeSubst)}
  deriving (Eq, Ord, Read, Show)

_UnifyTypesTestCase = (Core.Name "hydra.testing.UnifyTypesTestCase")

_UnifyTypesTestCase_schemaTypes = (Core.Name "schemaTypes")

_UnifyTypesTestCase_left = (Core.Name "left")

_UnifyTypesTestCase_right = (Core.Name "right")

_UnifyTypesTestCase_expected = (Core.Name "expected")

-- | A test case which joins two types (producing type constraints or failing). The expected result is either Left (failure) or Right (list of constraints).
data JoinTypesTestCase = 
  JoinTypesTestCase {
    -- | The left type to join
    joinTypesTestCaseLeft :: Core.Type,
    -- | The right type to join
    joinTypesTestCaseRight :: Core.Type,
    -- | The expected result: Left for failure, Right for constraints
    joinTypesTestCaseExpected :: (Either () [Typing.TypeConstraint])}
  deriving (Eq, Ord, Read, Show)

_JoinTypesTestCase = (Core.Name "hydra.testing.JoinTypesTestCase")

_JoinTypesTestCase_left = (Core.Name "left")

_JoinTypesTestCase_right = (Core.Name "right")

_JoinTypesTestCase_expected = (Core.Name "expected")
