package hydra.encode.testing

import hydra.core.*

import hydra.testing.*

import hydra.lib.eithers

import hydra.lib.lists

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

def alphaConversionTestCase(x: hydra.testing.AlphaConversionTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.AlphaConversionTestCase", Seq(hydra.core.Field("term", hydra.encode.core.term(x.term)), hydra.core.Field("oldVariable", hydra.encode.core.name(x.oldVariable)), hydra.core.Field("newVariable", hydra.encode.core.name(x.newVariable)), hydra.core.Field("result", hydra.encode.core.term(x.result)))))

def evaluationStyle(v1: hydra.testing.EvaluationStyle): hydra.core.Term =
  v1 match
  case hydra.testing.EvaluationStyle.eager() => hydra.core.Term.union(hydra.core.Injection("hydra.testing.EvaluationStyle", hydra.core.Field("eager", hydra.core.Term.unit)))
  case hydra.testing.EvaluationStyle.`lazy`() => hydra.core.Term.union(hydra.core.Injection("hydra.testing.EvaluationStyle", hydra.core.Field("lazy", hydra.core.Term.unit)))

def caseConversionTestCase(x: hydra.testing.CaseConversionTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.CaseConversionTestCase", Seq(hydra.core.Field("fromConvention", hydra.encode.util.caseConvention(x.fromConvention)), hydra.core.Field("toConvention", hydra.encode.util.caseConvention(x.toConvention)), hydra.core.Field("fromString", hydra.core.Term.literal(hydra.core.Literal.string(x.fromString))), hydra.core.Field("toString", hydra.core.Term.literal(hydra.core.Literal.string(x.`toString_`))))))

def delegatedEvaluationTestCase(x: hydra.testing.DelegatedEvaluationTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.DelegatedEvaluationTestCase", Seq(hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("output", hydra.encode.core.term(x.output)))))

def etaExpansionTestCase(x: hydra.testing.EtaExpansionTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.EtaExpansionTestCase", Seq(hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("output", hydra.encode.core.term(x.output)))))

def deannotateTermTestCase(x: hydra.testing.DeannotateTermTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.DeannotateTermTestCase", Seq(hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("output", hydra.encode.core.term(x.output)))))

def deannotateTypeTestCase(x: hydra.testing.DeannotateTypeTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.DeannotateTypeTestCase", Seq(hydra.core.Field("input", hydra.encode.core.`type`(x.input)), hydra.core.Field("output", hydra.encode.core.`type`(x.output)))))

def flattenLetTermsTestCase(x: hydra.testing.FlattenLetTermsTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.FlattenLetTermsTestCase", Seq(hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("output", hydra.encode.core.term(x.output)))))

def foldOperation(v1: hydra.testing.FoldOperation): hydra.core.Term =
  v1 match
  case hydra.testing.FoldOperation.sumInt32Literals() => hydra.core.Term.union(hydra.core.Injection("hydra.testing.FoldOperation", hydra.core.Field("sumInt32Literals", hydra.core.Term.unit)))
  case hydra.testing.FoldOperation.collectListLengths() => hydra.core.Term.union(hydra.core.Injection("hydra.testing.FoldOperation", hydra.core.Field("collectListLengths", hydra.core.Term.unit)))
  case hydra.testing.FoldOperation.collectLabels() => hydra.core.Term.union(hydra.core.Injection("hydra.testing.FoldOperation", hydra.core.Field("collectLabels", hydra.core.Term.unit)))

def foldOverTermTestCase(x: hydra.testing.FoldOverTermTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.FoldOverTermTestCase", Seq(hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("traversalOrder", hydra.encode.coders.traversalOrder(x.traversalOrder)), hydra.core.Field("operation", hydra.encode.testing.foldOperation(x.operation)), hydra.core.Field("output", hydra.encode.core.term(x.output)))))

def freeVariablesTestCase(x: hydra.testing.FreeVariablesTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.FreeVariablesTestCase", Seq(hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("output", hydra.core.Term.set(hydra.lib.sets.map[hydra.core.Name, hydra.core.Term](hydra.encode.core.name)(x.output))))))

def hoistPredicate(v1: hydra.testing.HoistPredicate): hydra.core.Term =
  v1 match
  case hydra.testing.HoistPredicate.caseStatements() => hydra.core.Term.union(hydra.core.Injection("hydra.testing.HoistPredicate", hydra.core.Field("caseStatements", hydra.core.Term.unit)))
  case hydra.testing.HoistPredicate.applications() => hydra.core.Term.union(hydra.core.Injection("hydra.testing.HoistPredicate", hydra.core.Field("applications", hydra.core.Term.unit)))
  case hydra.testing.HoistPredicate.lists() => hydra.core.Term.union(hydra.core.Injection("hydra.testing.HoistPredicate", hydra.core.Field("lists", hydra.core.Term.unit)))
  case hydra.testing.HoistPredicate.nothing() => hydra.core.Term.union(hydra.core.Injection("hydra.testing.HoistPredicate", hydra.core.Field("nothing", hydra.core.Term.unit)))

def hoistLetBindingsTestCase(x: hydra.testing.HoistLetBindingsTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.HoistLetBindingsTestCase", Seq(hydra.core.Field("input", hydra.encode.core.let(x.input)), hydra.core.Field("output", hydra.encode.core.let(x.output)))))

def hoistPolymorphicLetBindingsTestCase(x: hydra.testing.HoistPolymorphicLetBindingsTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.HoistPolymorphicLetBindingsTestCase", Seq(hydra.core.Field("input", hydra.encode.core.let(x.input)), hydra.core.Field("output", hydra.encode.core.let(x.output)))))

def hoistSubtermsTestCase(x: hydra.testing.HoistSubtermsTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.HoistSubtermsTestCase", Seq(hydra.core.Field("predicate", hydra.encode.testing.hoistPredicate(x.predicate)), hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("output", hydra.encode.core.term(x.output)))))

def hoistCaseStatementsTestCase(x: hydra.testing.HoistCaseStatementsTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.HoistCaseStatementsTestCase", Seq(hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("output", hydra.encode.core.term(x.output)))))

def termRewriter(v1: hydra.testing.TermRewriter): hydra.core.Term =
  v1 match
  case hydra.testing.TermRewriter.replaceFooWithBar() => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TermRewriter", hydra.core.Field("replaceFooWithBar", hydra.core.Term.unit)))
  case hydra.testing.TermRewriter.replaceInt32WithInt64() => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TermRewriter", hydra.core.Field("replaceInt32WithInt64", hydra.core.Term.unit)))

def rewriteTermTestCase(x: hydra.testing.RewriteTermTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.RewriteTermTestCase", Seq(hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("rewriter", hydra.encode.testing.termRewriter(x.rewriter)), hydra.core.Field("output", hydra.encode.core.term(x.output)))))

def typeRewriter(v1: hydra.testing.TypeRewriter): hydra.core.Term =
  v1 match
  case hydra.testing.TypeRewriter.replaceStringWithInt32() => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TypeRewriter", hydra.core.Field("replaceStringWithInt32", hydra.core.Term.unit)))

def rewriteTypeTestCase(x: hydra.testing.RewriteTypeTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.RewriteTypeTestCase", Seq(hydra.core.Field("input", hydra.encode.core.`type`(x.input)), hydra.core.Field("rewriter", hydra.encode.testing.typeRewriter(x.rewriter)), hydra.core.Field("output", hydra.encode.core.`type`(x.output)))))

def evaluationTestCase(x: hydra.testing.EvaluationTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.EvaluationTestCase", Seq(hydra.core.Field("evaluationStyle", hydra.encode.testing.evaluationStyle(x.evaluationStyle)), hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("output", hydra.encode.core.term(x.output)))))

def inferenceFailureTestCase(x: hydra.testing.InferenceFailureTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.InferenceFailureTestCase", Seq(hydra.core.Field("input", hydra.encode.core.term(x.input)))))

def inferenceTestCase(x: hydra.testing.InferenceTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.InferenceTestCase", Seq(hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("output", hydra.encode.core.typeScheme(x.output)))))

def jsonDecodeTestCase(x: hydra.testing.JsonDecodeTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.JsonDecodeTestCase", Seq(hydra.core.Field("type", hydra.encode.core.`type`(x.`type`)), hydra.core.Field("json", hydra.encode.json.model.value(x.json)), hydra.core.Field("expected", hydra.core.Term.either(hydra.lib.eithers.bimap[scala.Predef.String, hydra.core.Term, hydra.core.Term, hydra.core.Term]((x2: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(x2)))(hydra.encode.core.term)(x.expected))))))

def jsonEncodeTestCase(x: hydra.testing.JsonEncodeTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.JsonEncodeTestCase", Seq(hydra.core.Field("term", hydra.encode.core.term(x.term)), hydra.core.Field("expected", hydra.core.Term.either(hydra.lib.eithers.bimap[scala.Predef.String, hydra.json.model.Value, hydra.core.Term, hydra.core.Term]((x2: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(x2)))(hydra.encode.json.model.value)(x.expected))))))

def jsonParserTestCase(v1: hydra.testing.ParserTestCase[hydra.json.model.Value]): hydra.core.Term = hydra.encode.testing.parserTestCase(hydra.encode.json.model.value)(v1)

def jsonRoundtripTestCase(x: hydra.testing.JsonRoundtripTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.JsonRoundtripTestCase", Seq(hydra.core.Field("type", hydra.encode.core.`type`(x.`type`)), hydra.core.Field("term", hydra.encode.core.term(x.term)))))

def liftLambdaAboveLetTestCase(x: hydra.testing.LiftLambdaAboveLetTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.LiftLambdaAboveLetTestCase", Seq(hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("output", hydra.encode.core.term(x.output)))))

def jsonWriterTestCase(v1: hydra.testing.WriterTestCase[hydra.json.model.Value]): hydra.core.Term = hydra.encode.testing.writerTestCase(hydra.encode.json.model.value)(v1)

def parserTestCase[T0](a: (T0 => hydra.core.Term))(x: hydra.testing.ParserTestCase[T0]): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.ParserTestCase", Seq(hydra.core.Field("input", hydra.core.Term.literal(hydra.core.Literal.string(x.input))), hydra.core.Field("output", hydra.encode.parsing.parseResult(a)(x.output)))))

def tag(x: hydra.testing.Tag): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.testing.Tag", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def testCase(v1: hydra.testing.TestCase): hydra.core.Term =
  v1 match
  case hydra.testing.TestCase.alphaConversion(v_TestCase_alphaConversion_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("alphaConversion", hydra.encode.testing.alphaConversionTestCase(v_TestCase_alphaConversion_y))))
  case hydra.testing.TestCase.caseConversion(v_TestCase_caseConversion_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("caseConversion", hydra.encode.testing.caseConversionTestCase(v_TestCase_caseConversion_y))))
  case hydra.testing.TestCase.deannotateTerm(v_TestCase_deannotateTerm_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("deannotateTerm", hydra.encode.testing.deannotateTermTestCase(v_TestCase_deannotateTerm_y))))
  case hydra.testing.TestCase.deannotateType(v_TestCase_deannotateType_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("deannotateType", hydra.encode.testing.deannotateTypeTestCase(v_TestCase_deannotateType_y))))
  case hydra.testing.TestCase.delegatedEvaluation(v_TestCase_delegatedEvaluation_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("delegatedEvaluation", hydra.encode.testing.delegatedEvaluationTestCase(v_TestCase_delegatedEvaluation_y))))
  case hydra.testing.TestCase.etaExpansion(v_TestCase_etaExpansion_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("etaExpansion", hydra.encode.testing.etaExpansionTestCase(v_TestCase_etaExpansion_y))))
  case hydra.testing.TestCase.flattenLetTerms(v_TestCase_flattenLetTerms_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("flattenLetTerms", hydra.encode.testing.flattenLetTermsTestCase(v_TestCase_flattenLetTerms_y))))
  case hydra.testing.TestCase.freeVariables(v_TestCase_freeVariables_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("freeVariables", hydra.encode.testing.freeVariablesTestCase(v_TestCase_freeVariables_y))))
  case hydra.testing.TestCase.evaluation(v_TestCase_evaluation_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("evaluation", hydra.encode.testing.evaluationTestCase(v_TestCase_evaluation_y))))
  case hydra.testing.TestCase.inference(v_TestCase_inference_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("inference", hydra.encode.testing.inferenceTestCase(v_TestCase_inference_y))))
  case hydra.testing.TestCase.inferenceFailure(v_TestCase_inferenceFailure_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("inferenceFailure", hydra.encode.testing.inferenceFailureTestCase(v_TestCase_inferenceFailure_y))))
  case hydra.testing.TestCase.jsonDecode(v_TestCase_jsonDecode_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("jsonDecode", hydra.encode.testing.jsonDecodeTestCase(v_TestCase_jsonDecode_y))))
  case hydra.testing.TestCase.jsonEncode(v_TestCase_jsonEncode_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("jsonEncode", hydra.encode.testing.jsonEncodeTestCase(v_TestCase_jsonEncode_y))))
  case hydra.testing.TestCase.jsonParser(v_TestCase_jsonParser_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("jsonParser", hydra.encode.testing.jsonParserTestCase(v_TestCase_jsonParser_y))))
  case hydra.testing.TestCase.jsonRoundtrip(v_TestCase_jsonRoundtrip_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("jsonRoundtrip", hydra.encode.testing.jsonRoundtripTestCase(v_TestCase_jsonRoundtrip_y))))
  case hydra.testing.TestCase.jsonWriter(v_TestCase_jsonWriter_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("jsonWriter", hydra.encode.testing.jsonWriterTestCase(v_TestCase_jsonWriter_y))))
  case hydra.testing.TestCase.liftLambdaAboveLet(v_TestCase_liftLambdaAboveLet_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("liftLambdaAboveLet", hydra.encode.testing.liftLambdaAboveLetTestCase(v_TestCase_liftLambdaAboveLet_y))))
  case hydra.testing.TestCase.serialization(v_TestCase_serialization_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("serialization", hydra.encode.testing.serializationTestCase(v_TestCase_serialization_y))))
  case hydra.testing.TestCase.simplifyTerm(v_TestCase_simplifyTerm_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("simplifyTerm", hydra.encode.testing.simplifyTermTestCase(v_TestCase_simplifyTerm_y))))
  case hydra.testing.TestCase.topologicalSort(v_TestCase_topologicalSort_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("topologicalSort", hydra.encode.testing.topologicalSortTestCase(v_TestCase_topologicalSort_y))))
  case hydra.testing.TestCase.topologicalSortBindings(v_TestCase_topologicalSortBindings_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("topologicalSortBindings", hydra.encode.testing.topologicalSortBindingsTestCase(v_TestCase_topologicalSortBindings_y))))
  case hydra.testing.TestCase.topologicalSortSCC(v_TestCase_topologicalSortSCC_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("topologicalSortSCC", hydra.encode.testing.topologicalSortSCCTestCase(v_TestCase_topologicalSortSCC_y))))
  case hydra.testing.TestCase.typeChecking(v_TestCase_typeChecking_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("typeChecking", hydra.encode.testing.typeCheckingTestCase(v_TestCase_typeChecking_y))))
  case hydra.testing.TestCase.typeCheckingFailure(v_TestCase_typeCheckingFailure_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("typeCheckingFailure", hydra.encode.testing.typeCheckingFailureTestCase(v_TestCase_typeCheckingFailure_y))))
  case hydra.testing.TestCase.typeReduction(v_TestCase_typeReduction_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("typeReduction", hydra.encode.testing.typeReductionTestCase(v_TestCase_typeReduction_y))))
  case hydra.testing.TestCase.normalizeTypeVariables(v_TestCase_normalizeTypeVariables_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("normalizeTypeVariables", hydra.encode.testing.normalizeTypeVariablesTestCase(v_TestCase_normalizeTypeVariables_y))))
  case hydra.testing.TestCase.foldOverTerm(v_TestCase_foldOverTerm_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("foldOverTerm", hydra.encode.testing.foldOverTermTestCase(v_TestCase_foldOverTerm_y))))
  case hydra.testing.TestCase.rewriteTerm(v_TestCase_rewriteTerm_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("rewriteTerm", hydra.encode.testing.rewriteTermTestCase(v_TestCase_rewriteTerm_y))))
  case hydra.testing.TestCase.rewriteType(v_TestCase_rewriteType_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("rewriteType", hydra.encode.testing.rewriteTypeTestCase(v_TestCase_rewriteType_y))))
  case hydra.testing.TestCase.hoistSubterms(v_TestCase_hoistSubterms_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("hoistSubterms", hydra.encode.testing.hoistSubtermsTestCase(v_TestCase_hoistSubterms_y))))
  case hydra.testing.TestCase.hoistCaseStatements(v_TestCase_hoistCaseStatements_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("hoistCaseStatements", hydra.encode.testing.hoistCaseStatementsTestCase(v_TestCase_hoistCaseStatements_y))))
  case hydra.testing.TestCase.hoistLetBindings(v_TestCase_hoistLetBindings_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("hoistLetBindings", hydra.encode.testing.hoistLetBindingsTestCase(v_TestCase_hoistLetBindings_y))))
  case hydra.testing.TestCase.hoistPolymorphicLetBindings(v_TestCase_hoistPolymorphicLetBindings_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("hoistPolymorphicLetBindings", hydra.encode.testing.hoistPolymorphicLetBindingsTestCase(v_TestCase_hoistPolymorphicLetBindings_y))))
  case hydra.testing.TestCase.substInType(v_TestCase_substInType_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("substInType", hydra.encode.testing.substInTypeTestCase(v_TestCase_substInType_y))))
  case hydra.testing.TestCase.variableOccursInType(v_TestCase_variableOccursInType_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("variableOccursInType", hydra.encode.testing.variableOccursInTypeTestCase(v_TestCase_variableOccursInType_y))))
  case hydra.testing.TestCase.unifyTypes(v_TestCase_unifyTypes_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("unifyTypes", hydra.encode.testing.unifyTypesTestCase(v_TestCase_unifyTypes_y))))
  case hydra.testing.TestCase.joinTypes(v_TestCase_joinTypes_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("joinTypes", hydra.encode.testing.joinTypesTestCase(v_TestCase_joinTypes_y))))
  case hydra.testing.TestCase.unshadowVariables(v_TestCase_unshadowVariables_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("unshadowVariables", hydra.encode.testing.unshadowVariablesTestCase(v_TestCase_unshadowVariables_y))))
  case hydra.testing.TestCase.validateCoreTerm(v_TestCase_validateCoreTerm_y) => hydra.core.Term.union(hydra.core.Injection("hydra.testing.TestCase", hydra.core.Field("validateCoreTerm", hydra.encode.testing.validateCoreTermTestCase(v_TestCase_validateCoreTerm_y))))

def testCaseWithMetadata(x: hydra.testing.TestCaseWithMetadata): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.TestCaseWithMetadata", Seq(hydra.core.Field("name", hydra.core.Term.literal(hydra.core.Literal.string(x.name))), hydra.core.Field("case", hydra.encode.testing.testCase(x.`case`)), hydra.core.Field("description", hydra.core.Term.maybe(hydra.lib.maybes.map[scala.Predef.String, hydra.core.Term]((x2: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(x2)))(x.description))), hydra.core.Field("tags", hydra.core.Term.list(hydra.lib.lists.map[hydra.testing.Tag, hydra.core.Term](hydra.encode.testing.tag)(x.tags))))))

def testGroup(x: hydra.testing.TestGroup): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.TestGroup", Seq(hydra.core.Field("name", hydra.core.Term.literal(hydra.core.Literal.string(x.name))), hydra.core.Field("description", hydra.core.Term.maybe(hydra.lib.maybes.map[scala.Predef.String, hydra.core.Term]((x2: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(x2)))(x.description))), hydra.core.Field("subgroups", hydra.core.Term.list(hydra.lib.lists.map[hydra.testing.TestGroup, hydra.core.Term](hydra.encode.testing.testGroup)(x.subgroups))), hydra.core.Field("cases", hydra.core.Term.list(hydra.lib.lists.map[hydra.testing.TestCaseWithMetadata, hydra.core.Term](hydra.encode.testing.testCaseWithMetadata)(x.cases))))))

def typeCheckingTestCase(x: hydra.testing.TypeCheckingTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.TypeCheckingTestCase", Seq(hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("outputTerm", hydra.encode.core.term(x.outputTerm)), hydra.core.Field("outputType", hydra.encode.core.`type`(x.outputType)))))

def typeCheckingFailureTestCase(x: hydra.testing.TypeCheckingFailureTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.TypeCheckingFailureTestCase", Seq(hydra.core.Field("input", hydra.encode.core.term(x.input)))))

def topologicalSortBindingsTestCase(x: hydra.testing.TopologicalSortBindingsTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.TopologicalSortBindingsTestCase", Seq(hydra.core.Field("bindings", hydra.core.Term.list(hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.Term], hydra.core.Term]((p: Tuple2[hydra.core.Name, hydra.core.Term]) =>
  hydra.core.Term.pair(hydra.lib.pairs.bimap[hydra.core.Name, hydra.core.Term, hydra.core.Term, hydra.core.Term](hydra.encode.core.name)(hydra.encode.core.term)(p)))(x.bindings))), hydra.core.Field("expected", hydra.core.Term.list(hydra.lib.lists.map[Seq[Tuple2[hydra.core.Name, hydra.core.Term]], hydra.core.Term]((xs2: Seq[Tuple2[hydra.core.Name, hydra.core.Term]]) =>
  hydra.core.Term.list(hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.Term], hydra.core.Term]((p: Tuple2[hydra.core.Name, hydra.core.Term]) =>
  hydra.core.Term.pair(hydra.lib.pairs.bimap[hydra.core.Name, hydra.core.Term, hydra.core.Term, hydra.core.Term](hydra.encode.core.name)(hydra.encode.core.term)(p)))(xs2)))(x.expected))))))

def topologicalSortTestCase(x: hydra.testing.TopologicalSortTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.TopologicalSortTestCase", Seq(hydra.core.Field("adjacencyList", hydra.core.Term.list(hydra.lib.lists.map[Tuple2[Int, Seq[Int]], hydra.core.Term]((p: Tuple2[Int, Seq[Int]]) =>
  hydra.core.Term.pair(hydra.lib.pairs.bimap[Int, Seq[Int], hydra.core.Term, hydra.core.Term]((x2: Int) =>
  hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(x2))))((xs2: Seq[Int]) =>
  hydra.core.Term.list(hydra.lib.lists.map[Int, hydra.core.Term]((x2: Int) =>
  hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(x2))))(xs2)))(p)))(x.adjacencyList))), hydra.core.Field("expected", hydra.core.Term.either(hydra.lib.eithers.bimap[Seq[Seq[Int]], Seq[Int], hydra.core.Term, hydra.core.Term]((xs: Seq[Seq[Int]]) =>
  hydra.core.Term.list(hydra.lib.lists.map[Seq[Int], hydra.core.Term]((xs2: Seq[Int]) =>
  hydra.core.Term.list(hydra.lib.lists.map[Int, hydra.core.Term]((x2: Int) =>
  hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(x2))))(xs2)))(xs)))((xs: Seq[Int]) =>
  hydra.core.Term.list(hydra.lib.lists.map[Int, hydra.core.Term]((x2: Int) =>
  hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(x2))))(xs)))(x.expected))))))

def topologicalSortSCCTestCase(x: hydra.testing.TopologicalSortSCCTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.TopologicalSortSCCTestCase", Seq(hydra.core.Field("adjacencyList", hydra.core.Term.list(hydra.lib.lists.map[Tuple2[Int, Seq[Int]], hydra.core.Term]((p: Tuple2[Int, Seq[Int]]) =>
  hydra.core.Term.pair(hydra.lib.pairs.bimap[Int, Seq[Int], hydra.core.Term, hydra.core.Term]((x2: Int) =>
  hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(x2))))((xs2: Seq[Int]) =>
  hydra.core.Term.list(hydra.lib.lists.map[Int, hydra.core.Term]((x2: Int) =>
  hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(x2))))(xs2)))(p)))(x.adjacencyList))), hydra.core.Field("expected", hydra.core.Term.list(hydra.lib.lists.map[Seq[Int], hydra.core.Term]((xs2: Seq[Int]) =>
  hydra.core.Term.list(hydra.lib.lists.map[Int, hydra.core.Term]((x2: Int) =>
  hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(x2))))(xs2)))(x.expected))))))

def serializationTestCase(x: hydra.testing.SerializationTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.SerializationTestCase", Seq(hydra.core.Field("input", hydra.encode.ast.expr(x.input)), hydra.core.Field("output", hydra.core.Term.literal(hydra.core.Literal.string(x.output))))))

def simplifyTermTestCase(x: hydra.testing.SimplifyTermTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.SimplifyTermTestCase", Seq(hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("output", hydra.encode.core.term(x.output)))))

def normalizeTypeVariablesTestCase(x: hydra.testing.NormalizeTypeVariablesTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.NormalizeTypeVariablesTestCase", Seq(hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("output", hydra.encode.core.term(x.output)))))

def typeReductionTestCase(x: hydra.testing.TypeReductionTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.TypeReductionTestCase", Seq(hydra.core.Field("input", hydra.encode.core.`type`(x.input)), hydra.core.Field("output", hydra.encode.core.`type`(x.output)))))

def writerTestCase[T0](a: (T0 => hydra.core.Term))(x: hydra.testing.WriterTestCase[T0]): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.WriterTestCase", Seq(hydra.core.Field("input", a(x.input)), hydra.core.Field("output", hydra.core.Term.literal(hydra.core.Literal.string(x.output))))))

def substInTypeTestCase(x: hydra.testing.SubstInTypeTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.SubstInTypeTestCase", Seq(hydra.core.Field("substitution", hydra.core.Term.list(hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.Type], hydra.core.Term]((p: Tuple2[hydra.core.Name, hydra.core.Type]) =>
  hydra.core.Term.pair(hydra.lib.pairs.bimap[hydra.core.Name, hydra.core.Type, hydra.core.Term, hydra.core.Term](hydra.encode.core.name)(hydra.encode.core.`type`)(p)))(x.substitution))), hydra.core.Field("input", hydra.encode.core.`type`(x.input)), hydra.core.Field("output", hydra.encode.core.`type`(x.output)))))

def variableOccursInTypeTestCase(x: hydra.testing.VariableOccursInTypeTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.VariableOccursInTypeTestCase", Seq(hydra.core.Field("variable", hydra.encode.core.name(x.variable)), hydra.core.Field("type", hydra.encode.core.`type`(x.`type`)), hydra.core.Field("expected", hydra.core.Term.literal(hydra.core.Literal.boolean(x.expected))))))

def unshadowVariablesTestCase(x: hydra.testing.UnshadowVariablesTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.UnshadowVariablesTestCase", Seq(hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("output", hydra.encode.core.term(x.output)))))

def unifyTypesTestCase(x: hydra.testing.UnifyTypesTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.UnifyTypesTestCase", Seq(hydra.core.Field("schemaTypes", hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Name, hydra.core.Term](hydra.encode.core.name)(x.schemaTypes))), hydra.core.Field("left", hydra.encode.core.`type`(x.left)), hydra.core.Field("right", hydra.encode.core.`type`(x.right)), hydra.core.Field("expected", hydra.core.Term.either(hydra.lib.eithers.bimap[scala.Predef.String, hydra.typing.TypeSubst, hydra.core.Term, hydra.core.Term]((x2: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(x2)))(hydra.encode.typing.typeSubst)(x.expected))))))

def joinTypesTestCase(x: hydra.testing.JoinTypesTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.JoinTypesTestCase", Seq(hydra.core.Field("left", hydra.encode.core.`type`(x.left)), hydra.core.Field("right", hydra.encode.core.`type`(x.right)), hydra.core.Field("expected", hydra.core.Term.either(hydra.lib.eithers.bimap[Unit, Seq[hydra.typing.TypeConstraint], hydra.core.Term, hydra.core.Term]((_x: Unit) => hydra.core.Term.unit)((xs: Seq[hydra.typing.TypeConstraint]) =>
  hydra.core.Term.list(hydra.lib.lists.map[hydra.typing.TypeConstraint, hydra.core.Term](hydra.encode.typing.typeConstraint)(xs)))(x.expected))))))

def validateCoreTermTestCase(x: hydra.testing.ValidateCoreTermTestCase): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.testing.ValidateCoreTermTestCase", Seq(hydra.core.Field("typed", hydra.core.Term.literal(hydra.core.Literal.boolean(x.typed))), hydra.core.Field("input", hydra.encode.core.term(x.input)), hydra.core.Field("output", hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.error.core.InvalidTermError, hydra.core.Term](hydra.encode.error.core.invalidTermError)(x.output))))))
