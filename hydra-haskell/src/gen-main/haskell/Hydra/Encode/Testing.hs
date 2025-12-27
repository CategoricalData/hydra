-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.testing

module Hydra.Encode.Testing where

import qualified Hydra.Core as Core
import qualified Hydra.Encode.Ast as Ast
import qualified Hydra.Encode.Coders as Coders
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Encode.Json as Json
import qualified Hydra.Encode.Parsing as Parsing
import qualified Hydra.Encode.Util as Util
import qualified Hydra.Json as Json_
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

alphaConversionTestCase :: (Testing.AlphaConversionTestCase -> Core.Term)
alphaConversionTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (Core_.term (Testing.alphaConversionTestCaseTerm x))},
    Core.Field {
      Core.fieldName = (Core.Name "oldVariable"),
      Core.fieldTerm = (Core_.name (Testing.alphaConversionTestCaseOldVariable x))},
    Core.Field {
      Core.fieldName = (Core.Name "newVariable"),
      Core.fieldTerm = (Core_.name (Testing.alphaConversionTestCaseNewVariable x))},
    Core.Field {
      Core.fieldName = (Core.Name "result"),
      Core.fieldTerm = (Core_.term (Testing.alphaConversionTestCaseResult x))}]}))

evaluationStyle :: (Testing.EvaluationStyle -> Core.Term)
evaluationStyle x = case x of
  Testing.EvaluationStyleEager -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.EvaluationStyle"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "eager"),
      Core.fieldTerm = Core.TermUnit}}))
  Testing.EvaluationStyleLazy -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.EvaluationStyle"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "lazy"),
      Core.fieldTerm = Core.TermUnit}}))

caseConversionTestCase :: (Testing.CaseConversionTestCase -> Core.Term)
caseConversionTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fromConvention"),
      Core.fieldTerm = (Util.caseConvention (Testing.caseConversionTestCaseFromConvention x))},
    Core.Field {
      Core.fieldName = (Core.Name "toConvention"),
      Core.fieldTerm = (Util.caseConvention (Testing.caseConversionTestCaseToConvention x))},
    Core.Field {
      Core.fieldName = (Core.Name "fromString"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Testing.caseConversionTestCaseFromString x))},
    Core.Field {
      Core.fieldName = (Core.Name "toString"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Testing.caseConversionTestCaseToString x))}]}))

delegatedEvaluationTestCase :: (Testing.DelegatedEvaluationTestCase -> Core.Term)
delegatedEvaluationTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.DelegatedEvaluationTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.term (Testing.delegatedEvaluationTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = (Core_.term (Testing.delegatedEvaluationTestCaseOutput x))}]}))

etaExpansionTestCase :: (Testing.EtaExpansionTestCase -> Core.Term)
etaExpansionTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.EtaExpansionTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.term (Testing.etaExpansionTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = (Core_.term (Testing.etaExpansionTestCaseOutput x))}]}))

deannotateTermTestCase :: (Testing.DeannotateTermTestCase -> Core.Term)
deannotateTermTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.DeannotateTermTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.term (Testing.deannotateTermTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = (Core_.term (Testing.deannotateTermTestCaseOutput x))}]}))

deannotateTypeTestCase :: (Testing.DeannotateTypeTestCase -> Core.Term)
deannotateTypeTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.DeannotateTypeTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.type_ (Testing.deannotateTypeTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = (Core_.type_ (Testing.deannotateTypeTestCaseOutput x))}]}))

flattenLetTermsTestCase :: (Testing.FlattenLetTermsTestCase -> Core.Term)
flattenLetTermsTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.FlattenLetTermsTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.term (Testing.flattenLetTermsTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = (Core_.term (Testing.flattenLetTermsTestCaseOutput x))}]}))

foldOperation :: (Testing.FoldOperation -> Core.Term)
foldOperation x = case x of
  Testing.FoldOperationSumInt32Literals -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.FoldOperation"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "sumInt32Literals"),
      Core.fieldTerm = Core.TermUnit}}))
  Testing.FoldOperationCollectListLengths -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.FoldOperation"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "collectListLengths"),
      Core.fieldTerm = Core.TermUnit}}))
  Testing.FoldOperationCollectLabels -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.FoldOperation"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "collectLabels"),
      Core.fieldTerm = Core.TermUnit}}))

foldOverTermTestCase :: (Testing.FoldOverTermTestCase -> Core.Term)
foldOverTermTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.term (Testing.foldOverTermTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "traversalOrder"),
      Core.fieldTerm = (Coders.traversalOrder (Testing.foldOverTermTestCaseTraversalOrder x))},
    Core.Field {
      Core.fieldName = (Core.Name "operation"),
      Core.fieldTerm = (foldOperation (Testing.foldOverTermTestCaseOperation x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = (Core_.term (Testing.foldOverTermTestCaseOutput x))}]}))

freeVariablesTestCase :: (Testing.FreeVariablesTestCase -> Core.Term)
freeVariablesTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.FreeVariablesTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.term (Testing.freeVariablesTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = ((\s -> Core.TermSet (Sets.map Core_.name s)) (Testing.freeVariablesTestCaseOutput x))}]}))

termRewriter :: (Testing.TermRewriter -> Core.Term)
termRewriter x = case x of
  Testing.TermRewriterReplaceFooWithBar -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TermRewriter"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "replaceFooWithBar"),
      Core.fieldTerm = Core.TermUnit}}))
  Testing.TermRewriterReplaceInt32WithInt64 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TermRewriter"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "replaceInt32WithInt64"),
      Core.fieldTerm = Core.TermUnit}}))

rewriteTermTestCase :: (Testing.RewriteTermTestCase -> Core.Term)
rewriteTermTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.RewriteTermTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.term (Testing.rewriteTermTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "rewriter"),
      Core.fieldTerm = (termRewriter (Testing.rewriteTermTestCaseRewriter x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = (Core_.term (Testing.rewriteTermTestCaseOutput x))}]}))

typeRewriter :: (Testing.TypeRewriter -> Core.Term)
typeRewriter x = case x of
  Testing.TypeRewriterReplaceStringWithInt32 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TypeRewriter"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "replaceStringWithInt32"),
      Core.fieldTerm = Core.TermUnit}}))

rewriteTypeTestCase :: (Testing.RewriteTypeTestCase -> Core.Term)
rewriteTypeTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.RewriteTypeTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.type_ (Testing.rewriteTypeTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "rewriter"),
      Core.fieldTerm = (typeRewriter (Testing.rewriteTypeTestCaseRewriter x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = (Core_.type_ (Testing.rewriteTypeTestCaseOutput x))}]}))

evaluationTestCase :: (Testing.EvaluationTestCase -> Core.Term)
evaluationTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.EvaluationTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "evaluationStyle"),
      Core.fieldTerm = (evaluationStyle (Testing.evaluationTestCaseEvaluationStyle x))},
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.term (Testing.evaluationTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = (Core_.term (Testing.evaluationTestCaseOutput x))}]}))

inferenceFailureTestCase :: (Testing.InferenceFailureTestCase -> Core.Term)
inferenceFailureTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.InferenceFailureTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.term (Testing.inferenceFailureTestCaseInput x))}]}))

inferenceTestCase :: (Testing.InferenceTestCase -> Core.Term)
inferenceTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.InferenceTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.term (Testing.inferenceTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = (Core_.typeScheme (Testing.inferenceTestCaseOutput x))}]}))

jsonCoderTestCase :: (Testing.JsonCoderTestCase -> Core.Term)
jsonCoderTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.JsonCoderTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core_.type_ (Testing.jsonCoderTestCaseType x))},
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (Core_.term (Testing.jsonCoderTestCaseTerm x))},
    Core.Field {
      Core.fieldName = (Core.Name "json"),
      Core.fieldTerm = (Json.value (Testing.jsonCoderTestCaseJson x))}]}))

jsonParserTestCase :: (Testing.ParserTestCase Json_.Value -> Core.Term)
jsonParserTestCase = (parserTestCase Json.value)

liftLambdaAboveLetTestCase :: (Testing.LiftLambdaAboveLetTestCase -> Core.Term)
liftLambdaAboveLetTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.LiftLambdaAboveLetTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.term (Testing.liftLambdaAboveLetTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = (Core_.term (Testing.liftLambdaAboveLetTestCaseOutput x))}]}))

jsonWriterTestCase :: (Testing.WriterTestCase Json_.Value -> Core.Term)
jsonWriterTestCase = (writerTestCase Json.value)

parserTestCase :: ((t0 -> Core.Term) -> Testing.ParserTestCase t0 -> Core.Term)
parserTestCase a x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.ParserTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Testing.parserTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = (Parsing.parseResult a (Testing.parserTestCaseOutput x))}]}))

tag :: (Testing.Tag -> Core.Term)
tag x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.testing.Tag"),
  Core.wrappedTermBody = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Testing.unTag x))}))

testCase :: (Testing.TestCase -> Core.Term)
testCase x = case x of
  Testing.TestCaseAlphaConversion v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "alphaConversion"),
      Core.fieldTerm = (alphaConversionTestCase v1)}}))
  Testing.TestCaseCaseConversion v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "caseConversion"),
      Core.fieldTerm = (caseConversionTestCase v1)}}))
  Testing.TestCaseDeannotateTerm v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "deannotateTerm"),
      Core.fieldTerm = (deannotateTermTestCase v1)}}))
  Testing.TestCaseDeannotateType v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "deannotateType"),
      Core.fieldTerm = (deannotateTypeTestCase v1)}}))
  Testing.TestCaseDelegatedEvaluation v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "delegatedEvaluation"),
      Core.fieldTerm = (delegatedEvaluationTestCase v1)}}))
  Testing.TestCaseEtaExpansion v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "etaExpansion"),
      Core.fieldTerm = (etaExpansionTestCase v1)}}))
  Testing.TestCaseFlattenLetTerms v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "flattenLetTerms"),
      Core.fieldTerm = (flattenLetTermsTestCase v1)}}))
  Testing.TestCaseFreeVariables v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "freeVariables"),
      Core.fieldTerm = (freeVariablesTestCase v1)}}))
  Testing.TestCaseEvaluation v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "evaluation"),
      Core.fieldTerm = (evaluationTestCase v1)}}))
  Testing.TestCaseInference v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "inference"),
      Core.fieldTerm = (inferenceTestCase v1)}}))
  Testing.TestCaseInferenceFailure v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "inferenceFailure"),
      Core.fieldTerm = (inferenceFailureTestCase v1)}}))
  Testing.TestCaseJsonCoder v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "jsonCoder"),
      Core.fieldTerm = (jsonCoderTestCase v1)}}))
  Testing.TestCaseJsonParser v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "jsonParser"),
      Core.fieldTerm = (jsonParserTestCase v1)}}))
  Testing.TestCaseJsonWriter v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "jsonWriter"),
      Core.fieldTerm = (jsonWriterTestCase v1)}}))
  Testing.TestCaseLiftLambdaAboveLet v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "liftLambdaAboveLet"),
      Core.fieldTerm = (liftLambdaAboveLetTestCase v1)}}))
  Testing.TestCaseSerialization v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "serialization"),
      Core.fieldTerm = (serializationTestCase v1)}}))
  Testing.TestCaseSimplifyTerm v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "simplifyTerm"),
      Core.fieldTerm = (simplifyTermTestCase v1)}}))
  Testing.TestCaseTopologicalSort v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "topologicalSort"),
      Core.fieldTerm = (topologicalSortTestCase v1)}}))
  Testing.TestCaseTopologicalSortBindings v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "topologicalSortBindings"),
      Core.fieldTerm = (topologicalSortBindingsTestCase v1)}}))
  Testing.TestCaseTopologicalSortSCC v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "topologicalSortSCC"),
      Core.fieldTerm = (topologicalSortSCCTestCase v1)}}))
  Testing.TestCaseTypeChecking v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "typeChecking"),
      Core.fieldTerm = (typeCheckingTestCase v1)}}))
  Testing.TestCaseTypeCheckingFailure v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "typeCheckingFailure"),
      Core.fieldTerm = (typeCheckingFailureTestCase v1)}}))
  Testing.TestCaseTypeReduction v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "typeReduction"),
      Core.fieldTerm = (typeReductionTestCase v1)}}))
  Testing.TestCaseNormalizeTypeVariables v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "normalizeTypeVariables"),
      Core.fieldTerm = (normalizeTypeVariablesTestCase v1)}}))
  Testing.TestCaseFoldOverTerm v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "foldOverTerm"),
      Core.fieldTerm = (foldOverTermTestCase v1)}}))
  Testing.TestCaseRewriteTerm v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "rewriteTerm"),
      Core.fieldTerm = (rewriteTermTestCase v1)}}))
  Testing.TestCaseRewriteType v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "rewriteType"),
      Core.fieldTerm = (rewriteTypeTestCase v1)}}))

testCaseWithMetadata :: (Testing.TestCaseWithMetadata -> Core.Term)
testCaseWithMetadata x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Testing.testCaseWithMetadataName x))},
    Core.Field {
      Core.fieldName = (Core.Name "case"),
      Core.fieldTerm = (testCase (Testing.testCaseWithMetadataCase x))},
    Core.Field {
      Core.fieldName = (Core.Name "description"),
      Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map (\x -> Core.TermLiteral (Core.LiteralString x)) opt)) (Testing.testCaseWithMetadataDescription x))},
    Core.Field {
      Core.fieldName = (Core.Name "tags"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map tag xs)) (Testing.testCaseWithMetadataTags x))}]}))

testGroup :: (Testing.TestGroup -> Core.Term)
testGroup x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Testing.testGroupName x))},
    Core.Field {
      Core.fieldName = (Core.Name "description"),
      Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map (\x -> Core.TermLiteral (Core.LiteralString x)) opt)) (Testing.testGroupDescription x))},
    Core.Field {
      Core.fieldName = (Core.Name "subgroups"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map testGroup xs)) (Testing.testGroupSubgroups x))},
    Core.Field {
      Core.fieldName = (Core.Name "cases"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map testCaseWithMetadata xs)) (Testing.testGroupCases x))}]}))

typeCheckingTestCase :: (Testing.TypeCheckingTestCase -> Core.Term)
typeCheckingTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.TypeCheckingTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.term (Testing.typeCheckingTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "outputTerm"),
      Core.fieldTerm = (Core_.term (Testing.typeCheckingTestCaseOutputTerm x))},
    Core.Field {
      Core.fieldName = (Core.Name "outputType"),
      Core.fieldTerm = (Core_.type_ (Testing.typeCheckingTestCaseOutputType x))}]}))

typeCheckingFailureTestCase :: (Testing.TypeCheckingFailureTestCase -> Core.Term)
typeCheckingFailureTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.TypeCheckingFailureTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.term (Testing.typeCheckingFailureTestCaseInput x))}]}))

topologicalSortBindingsTestCase :: (Testing.TopologicalSortBindingsTestCase -> Core.Term)
topologicalSortBindingsTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.TopologicalSortBindingsTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "bindings"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (\p -> Core.TermPair (Pairs.bimap Core_.name Core_.term p)) xs)) (Testing.topologicalSortBindingsTestCaseBindings x))},
    Core.Field {
      Core.fieldName = (Core.Name "expected"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (\xs -> Core.TermList (Lists.map (\p -> Core.TermPair (Pairs.bimap Core_.name Core_.term p)) xs)) xs)) (Testing.topologicalSortBindingsTestCaseExpected x))}]}))

topologicalSortTestCase :: (Testing.TopologicalSortTestCase -> Core.Term)
topologicalSortTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.TopologicalSortTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "adjacencyList"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (\p -> Core.TermPair (Pairs.bimap (\x -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x))) (\xs -> Core.TermList (Lists.map (\x -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x))) xs)) p)) xs)) (Testing.topologicalSortTestCaseAdjacencyList x))},
    Core.Field {
      Core.fieldName = (Core.Name "expected"),
      Core.fieldTerm = ((\e -> Core.TermEither (Eithers.bimap (\xs -> Core.TermList (Lists.map (\xs -> Core.TermList (Lists.map (\x -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x))) xs)) xs)) (\xs -> Core.TermList (Lists.map (\x -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x))) xs)) e)) (Testing.topologicalSortTestCaseExpected x))}]}))

topologicalSortSCCTestCase :: (Testing.TopologicalSortSCCTestCase -> Core.Term)
topologicalSortSCCTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.TopologicalSortSCCTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "adjacencyList"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (\p -> Core.TermPair (Pairs.bimap (\x -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x))) (\xs -> Core.TermList (Lists.map (\x -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x))) xs)) p)) xs)) (Testing.topologicalSortSCCTestCaseAdjacencyList x))},
    Core.Field {
      Core.fieldName = (Core.Name "expected"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (\xs -> Core.TermList (Lists.map (\x -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x))) xs)) xs)) (Testing.topologicalSortSCCTestCaseExpected x))}]}))

serializationTestCase :: (Testing.SerializationTestCase -> Core.Term)
serializationTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.SerializationTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Ast.expr (Testing.serializationTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Testing.serializationTestCaseOutput x))}]}))

simplifyTermTestCase :: (Testing.SimplifyTermTestCase -> Core.Term)
simplifyTermTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.SimplifyTermTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.term (Testing.simplifyTermTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = (Core_.term (Testing.simplifyTermTestCaseOutput x))}]}))

normalizeTypeVariablesTestCase :: (Testing.NormalizeTypeVariablesTestCase -> Core.Term)
normalizeTypeVariablesTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.NormalizeTypeVariablesTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.term (Testing.normalizeTypeVariablesTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = (Core_.term (Testing.normalizeTypeVariablesTestCaseOutput x))}]}))

typeReductionTestCase :: (Testing.TypeReductionTestCase -> Core.Term)
typeReductionTestCase x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.TypeReductionTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (Core_.type_ (Testing.typeReductionTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = (Core_.type_ (Testing.typeReductionTestCaseOutput x))}]}))

writerTestCase :: ((t0 -> Core.Term) -> Testing.WriterTestCase t0 -> Core.Term)
writerTestCase a x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.testing.WriterTestCase"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "input"),
      Core.fieldTerm = (a (Testing.writerTestCaseInput x))},
    Core.Field {
      Core.fieldName = (Core.Name "output"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Testing.writerTestCaseOutput x))}]}))
