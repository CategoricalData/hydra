package hydra.testing

import hydra.ast.*

import hydra.coders.*

import hydra.core.*

import hydra.graph.*

import hydra.json.model.*

import hydra.module.*

import hydra.parsing.*

import hydra.typing.*

import hydra.util.*

import hydra.ast

import hydra.coders

import hydra.core

import hydra.graph

import hydra.json.model

import hydra.module

import hydra.parsing

import hydra.typing

import hydra.util

case class AlphaConversionTestCase(term: hydra.core.Term, oldVariable: hydra.core.Name, newVariable: hydra.core.Name, result: hydra.core.Term)

enum EvaluationStyle :
   case eager extends EvaluationStyle
   case `lazy` extends EvaluationStyle

case class CaseConversionTestCase(fromConvention: hydra.util.CaseConvention, toConvention: hydra.util.CaseConvention,
   fromString: scala.Predef.String, `toString_`: scala.Predef.String)

case class DelegatedEvaluationTestCase(input: hydra.core.Term, output: hydra.core.Term)

case class EtaExpansionTestCase(input: hydra.core.Term, output: hydra.core.Term)

case class DeannotateTermTestCase(input: hydra.core.Term, output: hydra.core.Term)

case class DeannotateTypeTestCase(input: hydra.core.Type, output: hydra.core.Type)

case class FlattenLetTermsTestCase(input: hydra.core.Term, output: hydra.core.Term)

enum FoldOperation :
   case sumInt32Literals extends FoldOperation
   case collectListLengths extends FoldOperation
   case collectLabels extends FoldOperation

case class FoldOverTermTestCase(input: hydra.core.Term, traversalOrder: hydra.coders.TraversalOrder, operation: hydra.testing.FoldOperation,
   output: hydra.core.Term)

case class FreeVariablesTestCase(input: hydra.core.Term, output: scala.collection.immutable.Set[hydra.core.Name])

enum HoistPredicate :
   case caseStatements extends HoistPredicate
   case applications extends HoistPredicate
   case lists extends HoistPredicate
   case nothing extends HoistPredicate

case class HoistLetBindingsTestCase(input: hydra.core.Let, output: hydra.core.Let)

case class HoistPolymorphicLetBindingsTestCase(input: hydra.core.Let, output: hydra.core.Let)

case class HoistSubtermsTestCase(predicate: hydra.testing.HoistPredicate, input: hydra.core.Term, output: hydra.core.Term)

case class HoistCaseStatementsTestCase(input: hydra.core.Term, output: hydra.core.Term)

enum TermRewriter :
   case replaceFooWithBar extends TermRewriter
   case replaceInt32WithInt64 extends TermRewriter

case class RewriteTermTestCase(input: hydra.core.Term, rewriter: hydra.testing.TermRewriter, output: hydra.core.Term)

enum TypeRewriter :
   case replaceStringWithInt32 extends TypeRewriter

case class RewriteTypeTestCase(input: hydra.core.Type, rewriter: hydra.testing.TypeRewriter, output: hydra.core.Type)

case class EvaluationTestCase(evaluationStyle: hydra.testing.EvaluationStyle, input: hydra.core.Term, output: hydra.core.Term)

case class InferenceFailureTestCase(input: hydra.core.Term)

case class InferenceTestCase(input: hydra.core.Term, output: hydra.core.TypeScheme)

case class JsonDecodeTestCase(`type`: hydra.core.Type, json: hydra.json.model.Value, expected: Either[scala.Predef.String, hydra.core.Term])

case class JsonEncodeTestCase(term: hydra.core.Term, expected: Either[scala.Predef.String, hydra.json.model.Value])

type JsonParserTestCase = hydra.testing.ParserTestCase[hydra.json.model.Value]

case class JsonRoundtripTestCase(`type`: hydra.core.Type, term: hydra.core.Term)

case class LiftLambdaAboveLetTestCase(input: hydra.core.Term, output: hydra.core.Term)

type JsonWriterTestCase = hydra.testing.WriterTestCase[hydra.json.model.Value]

case class ParserTestCase[A](input: scala.Predef.String, output: hydra.parsing.ParseResult[A])

type Tag = scala.Predef.String

case class TestCodec(language: hydra.coders.LanguageName, fileExtension: hydra.module.FileExtension, encodeTerm: (hydra.core.Term => hydra.graph.Graph => Either[scala.Predef.String,
   scala.Predef.String]), encodeType: (hydra.core.Type => hydra.graph.Graph => Either[scala.Predef.String,
   scala.Predef.String]), formatTestName: (scala.Predef.String => scala.Predef.String), formatModuleName: (hydra.module.Namespace => scala.Predef.String),
   testCaseTemplate: scala.Predef.String, testGroupTemplate: scala.Predef.String, moduleTemplate: scala.Predef.String,
   importTemplate: scala.Predef.String, findImports: (scala.collection.immutable.Set[hydra.core.Name] => Seq[scala.Predef.String]))

case class TestGenerator[A](namespacesForModule: (hydra.module.Module => hydra.graph.Graph => Either[scala.Predef.String,
   hydra.module.Namespaces[A]]), createCodec: (hydra.module.Namespaces[A] => hydra.testing.TestCodec),
   generateTestFile: (hydra.module.Module => hydra.testing.TestGroup => hydra.graph.Graph => Either[scala.Predef.String,
   Tuple2[scala.Predef.String, scala.Predef.String]]), aggregatorFile: Option[scala.Predef.String => Seq[hydra.module.Module] => Tuple2[scala.Predef.String,
   scala.Predef.String]])

enum TestCase :
   case alphaConversion(value: hydra.testing.AlphaConversionTestCase) extends TestCase
   case caseConversion(value: hydra.testing.CaseConversionTestCase) extends TestCase
   case deannotateTerm(value: hydra.testing.DeannotateTermTestCase) extends TestCase
   case deannotateType(value: hydra.testing.DeannotateTypeTestCase) extends TestCase
   case delegatedEvaluation(value: hydra.testing.DelegatedEvaluationTestCase) extends TestCase
   case etaExpansion(value: hydra.testing.EtaExpansionTestCase) extends TestCase
   case flattenLetTerms(value: hydra.testing.FlattenLetTermsTestCase) extends TestCase
   case freeVariables(value: hydra.testing.FreeVariablesTestCase) extends TestCase
   case evaluation(value: hydra.testing.EvaluationTestCase) extends TestCase
   case inference(value: hydra.testing.InferenceTestCase) extends TestCase
   case inferenceFailure(value: hydra.testing.InferenceFailureTestCase) extends TestCase
   case jsonDecode(value: hydra.testing.JsonDecodeTestCase) extends TestCase
   case jsonEncode(value: hydra.testing.JsonEncodeTestCase) extends TestCase
   case jsonParser(value: hydra.testing.JsonParserTestCase) extends TestCase
   case jsonRoundtrip(value: hydra.testing.JsonRoundtripTestCase) extends TestCase
   case jsonWriter(value: hydra.testing.JsonWriterTestCase) extends TestCase
   case liftLambdaAboveLet(value: hydra.testing.LiftLambdaAboveLetTestCase) extends TestCase
   case serialization(value: hydra.testing.SerializationTestCase) extends TestCase
   case simplifyTerm(value: hydra.testing.SimplifyTermTestCase) extends TestCase
   case topologicalSort(value: hydra.testing.TopologicalSortTestCase) extends TestCase
   case topologicalSortBindings(value: hydra.testing.TopologicalSortBindingsTestCase) extends TestCase
   case topologicalSortSCC(value: hydra.testing.TopologicalSortSCCTestCase) extends TestCase
   case typeChecking(value: hydra.testing.TypeCheckingTestCase) extends TestCase
   case typeCheckingFailure(value: hydra.testing.TypeCheckingFailureTestCase) extends TestCase
   case typeReduction(value: hydra.testing.TypeReductionTestCase) extends TestCase
   case normalizeTypeVariables(value: hydra.testing.NormalizeTypeVariablesTestCase) extends TestCase
   case foldOverTerm(value: hydra.testing.FoldOverTermTestCase) extends TestCase
   case rewriteTerm(value: hydra.testing.RewriteTermTestCase) extends TestCase
   case rewriteType(value: hydra.testing.RewriteTypeTestCase) extends TestCase
   case hoistSubterms(value: hydra.testing.HoistSubtermsTestCase) extends TestCase
   case hoistCaseStatements(value: hydra.testing.HoistCaseStatementsTestCase) extends TestCase
   case hoistLetBindings(value: hydra.testing.HoistLetBindingsTestCase) extends TestCase
   case hoistPolymorphicLetBindings(value: hydra.testing.HoistPolymorphicLetBindingsTestCase) extends TestCase
   case substInType(value: hydra.testing.SubstInTypeTestCase) extends TestCase
   case variableOccursInType(value: hydra.testing.VariableOccursInTypeTestCase) extends TestCase
   case unifyTypes(value: hydra.testing.UnifyTypesTestCase) extends TestCase
   case joinTypes(value: hydra.testing.JoinTypesTestCase) extends TestCase
   case unshadowVariables(value: hydra.testing.UnshadowVariablesTestCase) extends TestCase

case class TestCaseWithMetadata(name: scala.Predef.String, `case`: hydra.testing.TestCase, description: Option[scala.Predef.String],
   tags: Seq[hydra.testing.Tag])

case class TestGroup(name: scala.Predef.String, description: Option[scala.Predef.String], subgroups: Seq[hydra.testing.TestGroup],
   cases: Seq[hydra.testing.TestCaseWithMetadata])

case class TypeCheckingTestCase(input: hydra.core.Term, outputTerm: hydra.core.Term, outputType: hydra.core.Type)

case class TypeCheckingFailureTestCase(input: hydra.core.Term)

case class TopologicalSortBindingsTestCase(bindings: Seq[Tuple2[hydra.core.Name, hydra.core.Term]], expected: Seq[Seq[Tuple2[hydra.core.Name,
   hydra.core.Term]]])

case class TopologicalSortTestCase(adjacencyList: Seq[Tuple2[Int, Seq[Int]]], expected: Either[Seq[Seq[Int]], Seq[Int]])

case class TopologicalSortSCCTestCase(adjacencyList: Seq[Tuple2[Int, Seq[Int]]], expected: Seq[Seq[Int]])

case class SerializationTestCase(input: hydra.ast.Expr, output: scala.Predef.String)

case class SimplifyTermTestCase(input: hydra.core.Term, output: hydra.core.Term)

case class NormalizeTypeVariablesTestCase(input: hydra.core.Term, output: hydra.core.Term)

case class TypeReductionTestCase(input: hydra.core.Type, output: hydra.core.Type)

case class WriterTestCase[A](input: A, output: scala.Predef.String)

case class SubstInTypeTestCase(substitution: Seq[Tuple2[hydra.core.Name, hydra.core.Type]], input: hydra.core.Type, output: hydra.core.Type)

case class VariableOccursInTypeTestCase(variable: hydra.core.Name, `type`: hydra.core.Type, expected: Boolean)

case class UnshadowVariablesTestCase(input: hydra.core.Term, output: hydra.core.Term)

case class UnifyTypesTestCase(schemaTypes: Seq[hydra.core.Name], left: hydra.core.Type, right: hydra.core.Type,
   expected: Either[scala.Predef.String, hydra.typing.TypeSubst])

case class JoinTypesTestCase(left: hydra.core.Type, right: hydra.core.Type, expected: Either[Unit, Seq[hydra.typing.TypeConstraint]])
