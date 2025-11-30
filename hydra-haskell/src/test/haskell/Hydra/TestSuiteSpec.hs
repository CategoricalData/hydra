{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.TestSuiteSpec.spec
-}

module Hydra.TestSuiteSpec where

import Hydra.Kernel
import Hydra.TestUtils
import Hydra.Testing
import Hydra.Inference
import Hydra.Parsing (ParseResult(..))
import Hydra.Test.TestSuite
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Dsl.Meta.Testing as Testing
import qualified Hydra.Json.Writer as JsonWriter
import qualified Hydra.Json.Parser as JsonParser
import qualified Hydra.Ext.Org.Json.Coder as JsonCoder
import qualified Hydra.Json as Json
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Coders as Coders

import qualified Control.Monad as CM
import qualified Test.Hspec as H
import qualified Test.HUnit.Lang as HL
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


type TestRunner = String -> TestCaseWithMetadata -> Y.Maybe (H.SpecWith ())

defaultTestRunner :: TestRunner
defaultTestRunner desc tcase = if Testing.isDisabled tcase || Testing.isRequiresInterp tcase
  then Nothing
  else Just $ case testCaseWithMetadataCase tcase of
    TestCaseAlphaConversion (AlphaConversionTestCase term oldVar newVar result) ->
      H.it "alpha conversion" $ H.shouldBe
        (alphaConvert oldVar newVar term)
        result
    TestCaseCaseConversion (CaseConversionTestCase fromConvention toConvention fromString toString) ->
      H.it "case conversion" $ H.shouldBe
        (convertCase fromConvention toConvention fromString)
        toString
    TestCaseDelegatedEvaluation _ ->
      H.it "delegated evaluation (skipped - runs in target language)" $ H.shouldBe True True
    TestCaseEtaExpansion (EtaExpansionTestCase input output) -> expectEtaExpansionResult desc input output
    TestCaseEvaluation (EvaluationTestCase _ input output) ->
      H.it "evaluation" $ shouldSucceedWith
        (eval input)
        output
    TestCaseInference (InferenceTestCase input output) -> expectInferenceResult desc input output
    TestCaseInferenceFailure (InferenceFailureTestCase input) ->
      H.it "inference failure" $ expectInferenceFailure desc input
    TestCaseJsonCoder (JsonCoderTestCase typ term expectedJson) ->
      H.it "JSON coder" $ checkJsonCoder typ term expectedJson
    TestCaseJsonParser (ParserTestCase input expectedResult) ->
      H.it "JSON parser" $ H.shouldBe
        (JsonParser.parseJson input)
        expectedResult
    TestCaseJsonWriter (WriterTestCase input expectedOutput) ->
      H.it "JSON writer" $ H.shouldBe
        (JsonWriter.printJson input)
        expectedOutput
    TestCaseTypeChecking (TypeCheckingTestCase input outputTerm outputType) ->
      expectTypeCheckingResult desc input outputTerm outputType
    TestCaseTypeCheckingFailure (TypeCheckingFailureTestCase input) ->
      H.it "type checking failure" $ H.shouldBe True False  -- TODO: implement
    TestCaseTypeReduction (TypeReductionTestCase input output) ->
      H.it "type reduction" $ H.shouldBe
        (fromFlow input (schemaContext testGraph) (betaReduceType input))
        output
    TestCaseTopologicalSort (TopologicalSortTestCase adjList expected) ->
      H.it "topological sort" $ H.shouldBe
        (Sorting.topologicalSort adjList)
        expected
    TestCaseTopologicalSortSCC (TopologicalSortSCCTestCase adjList expected) ->
      H.it "topological sort SCC" $ H.shouldBe
        (Sorting.topologicalSortComponents adjList)
        expected
    TestCaseSerialization (SerializationTestCase input output) ->
      H.it "serialization" $ H.shouldBe
        (Serialization.printExpr (Serialization.parenthesize input))
        output
    TestCaseFlattenLetTerms (FlattenLetTermsTestCase input output) ->
      H.it "flatten let terms" $ H.shouldBe
        (Rewriting.flattenLetTerms input)
        output
    TestCaseFreeVariables (FreeVariablesTestCase input output) ->
      H.it "free variables" $ H.shouldBe
        (Rewriting.freeVariablesInTerm input)
        output
    TestCaseLiftLambdaAboveLet (LiftLambdaAboveLetTestCase input output) ->
      H.it "lift lambda above let" $ H.shouldBe
        (Rewriting.liftLambdaAboveLet input)
        output
    TestCaseSimplifyTerm (SimplifyTermTestCase input output) ->
      H.it "simplify term" $ H.shouldBe
        (Rewriting.simplifyTerm input)
        output
    TestCaseDeannotateTerm (DeannotateTermTestCase input output) ->
      H.it "deannotate term" $ H.shouldBe
        (Rewriting.deannotateTerm input)
        output
    TestCaseDeannotateType (DeannotateTypeTestCase input output) ->
      H.it "deannotate type" $ H.shouldBe
        (Rewriting.deannotateType input)
        output
    TestCaseTopologicalSortBindings (TopologicalSortBindingsTestCase bindings expected) ->
      H.it "topological sort bindings" $ H.shouldBe
        (S.fromList (fmap S.fromList (Rewriting.topologicalSortBindingMap (M.fromList bindings))))
        (S.fromList (fmap S.fromList expected))
    TestCaseNormalizeTypeVariables (NormalizeTypeVariablesTestCase input output) ->
      H.it "normalize type variables" $ H.shouldBe
        (Rewriting.normalizeTypeVariablesInTerm input)
        output
    TestCaseFoldOverTerm (FoldOverTermTestCase input traversalOrder op output) ->
      H.it "fold over term" $ H.shouldBe
        (runFoldOperation traversalOrder op input)
        output
    TestCaseRewriteTerm (RewriteTermTestCase input rewriter output) ->
      H.it "rewrite term" $ H.shouldBe
        (runTermRewriter rewriter input)
        output
    TestCaseRewriteType (RewriteTypeTestCase input rewriter output) ->
      H.it "rewrite type" $ H.shouldBe
        (runTypeRewriter rewriter input)
        output
  where
    cx = fromFlow emptyInferenceContext () $ graphToInferenceContext testGraph

runTestCase :: String -> TestRunner -> TestCaseWithMetadata -> H.SpecWith ()
runTestCase pdesc runner tcase@(TestCaseWithMetadata name _ mdesc _) =
  case runner cdesc tcase of
    Nothing -> return ()
    Just spec -> H.describe desc spec
  where
    desc = name ++ Y.maybe ("") (\d -> ": " ++ d) mdesc
    cdesc = if L.null pdesc then desc else pdesc ++ ", " ++ desc

runTestGroup :: String -> TestRunner -> TestGroup -> H.SpecWith ()
runTestGroup pdesc runner tg = do
    H.describe desc $ do
      CM.mapM (runTestCase cdesc runner) $ testGroupCases tg
      CM.sequence (runTestGroup cdesc runner <$> (testGroupSubgroups tg))
      return ()
  where
    desc = testGroupName tg ++ descSuffix
    cdesc = if L.null pdesc then desc else pdesc ++ ", " ++ desc
    descSuffix = case testGroupDescription tg of
      Nothing -> ""
      Just d -> " (" ++ d ++ ")"

spec :: H.Spec
spec = runTestGroup "" defaultTestRunner allTests

-- | Check that the JSON coder correctly encodes a term to the expected JSON value
-- and that decoding and re-encoding produces the same term (round-trip)
checkJsonCoder :: Type -> Term -> Json.Value -> H.Expectation
checkJsonCoder typ term expectedJson = case mstep of
    Nothing -> HL.assertFailure (traceSummary trace)
    Just step -> do
      shouldSucceedWith (coderEncode step term) expectedJson
      shouldSucceedWith (coderEncode step term >>= coderDecode step) term
  where
    FlowState mstep _ trace = unFlow (JsonCoder.jsonCoder typ) testGraph emptyTrace

-- | Run a fold operation over a term
runFoldOperation :: Coders.TraversalOrder -> FoldOperation -> Term -> Term
runFoldOperation order op = case op of
    FoldOperationSumInt32Literals -> TermLiteral . LiteralInteger . IntegerValueInt32 . sumInt32
    FoldOperationCollectListLengths -> TermList . fmap (TermLiteral . LiteralInteger . IntegerValueInt32) . collectListLengths
    FoldOperationCollectLabels -> TermList . fmap TermLiteral . collectLabels
  where
    sumInt32 :: Term -> Int
    sumInt32 = Rewriting.foldOverTerm order (\acc t -> acc + getInt32 t) 0
    collectListLengths :: Term -> [Int]
    collectListLengths = Rewriting.foldOverTerm order (\acc t -> acc ++ getListLength t) []
    collectLabels = Rewriting.foldOverTerm order (\acc t -> acc ++ getLabel t) []

    getInt32 :: Term -> Int
    getInt32 (TermLiteral (LiteralInteger (IntegerValueInt32 n))) = n
    getInt32 _ = 0

    getListLength (TermList elems) = [length elems]
    getListLength _ = []

    getLabel (TermPair (TermLiteral (LiteralString s), _)) = [LiteralString s]
    getLabel _ = []

-- | Run a term rewriter
runTermRewriter :: TermRewriter -> Term -> Term
runTermRewriter rewriter = Rewriting.rewriteTerm rewrite
  where
    rewrite recurse term = case (rewriter, term) of
      (TermRewriterReplaceFooWithBar, TermLiteral (LiteralString "foo")) ->
        TermLiteral (LiteralString "bar")
      (TermRewriterReplaceInt32WithInt64, TermLiteral (LiteralInteger (IntegerValueInt32 n))) ->
        TermLiteral (LiteralInteger (IntegerValueInt64 (fromIntegral n)))
      _ -> recurse term

-- | Run a type rewriter
runTypeRewriter :: TypeRewriter -> Type -> Type
runTypeRewriter TypeRewriterReplaceStringWithInt32 = Rewriting.rewriteType rewrite
  where
    rewrite recurse typ = case typ of
      TypeLiteral LiteralTypeString -> TypeLiteral (LiteralTypeInteger IntegerTypeInt32)
      _ -> recurse typ
