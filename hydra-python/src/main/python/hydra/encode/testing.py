# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.testing."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, Maybe, frozenlist
from typing import cast
import hydra.core
import hydra.encode.ast
import hydra.encode.coders
import hydra.encode.core
import hydra.encode.json
import hydra.encode.parsing
import hydra.encode.util
import hydra.json
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.testing

def alpha_conversion_test_case(x: hydra.testing.AlphaConversionTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.AlphaConversionTestCase"), (hydra.core.Field(hydra.core.Name("term"), hydra.encode.core.term(x.term)), hydra.core.Field(hydra.core.Name("oldVariable"), hydra.encode.core.name(x.old_variable)), hydra.core.Field(hydra.core.Name("newVariable"), hydra.encode.core.name(x.new_variable)), hydra.core.Field(hydra.core.Name("result"), hydra.encode.core.term(x.result))))))

def case_conversion_test_case(x: hydra.testing.CaseConversionTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.CaseConversionTestCase"), (hydra.core.Field(hydra.core.Name("fromConvention"), hydra.encode.util.case_convention(x.from_convention)), hydra.core.Field(hydra.core.Name("toConvention"), hydra.encode.util.case_convention(x.to_convention)), hydra.core.Field(hydra.core.Name("fromString"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.from_string)), hydra.core.Field(hydra.core.Name("toString"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.to_string))))))

def deannotate_term_test_case(x: hydra.testing.DeannotateTermTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.DeannotateTermTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.term(x.input)), hydra.core.Field(hydra.core.Name("output"), hydra.encode.core.term(x.output))))))

def deannotate_type_test_case(x: hydra.testing.DeannotateTypeTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.DeannotateTypeTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.type(x.input)), hydra.core.Field(hydra.core.Name("output"), hydra.encode.core.type(x.output))))))

def delegated_evaluation_test_case(x: hydra.testing.DelegatedEvaluationTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.DelegatedEvaluationTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.term(x.input)), hydra.core.Field(hydra.core.Name("output"), hydra.encode.core.term(x.output))))))

def eta_expansion_test_case(x: hydra.testing.EtaExpansionTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.EtaExpansionTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.term(x.input)), hydra.core.Field(hydra.core.Name("output"), hydra.encode.core.term(x.output))))))

def evaluation_style(v1: hydra.testing.EvaluationStyle) -> hydra.core.Type:
    match v1:
        case hydra.testing.EvaluationStyle.EAGER:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.EvaluationStyle"), hydra.core.Field(hydra.core.Name("eager"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v)))))
        
        case hydra.testing.EvaluationStyle.LAZY:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.EvaluationStyle"), hydra.core.Field(hydra.core.Name("lazy"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v2)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def evaluation_test_case(x: hydra.testing.EvaluationTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.EvaluationTestCase"), (hydra.core.Field(hydra.core.Name("evaluationStyle"), evaluation_style(x.evaluation_style)), hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.term(x.input)), hydra.core.Field(hydra.core.Name("output"), hydra.encode.core.term(x.output))))))

def flatten_let_terms_test_case(x: hydra.testing.FlattenLetTermsTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.FlattenLetTermsTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.term(x.input)), hydra.core.Field(hydra.core.Name("output"), hydra.encode.core.term(x.output))))))

def fold_operation(v1: hydra.testing.FoldOperation) -> hydra.core.Type:
    match v1:
        case hydra.testing.FoldOperation.SUM_INT32_LITERALS:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.FoldOperation"), hydra.core.Field(hydra.core.Name("sumInt32Literals"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v)))))
        
        case hydra.testing.FoldOperation.COLLECT_LIST_LENGTHS:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.FoldOperation"), hydra.core.Field(hydra.core.Name("collectListLengths"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v2)))))
        
        case hydra.testing.FoldOperation.COLLECT_LABELS:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.FoldOperation"), hydra.core.Field(hydra.core.Name("collectLabels"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v3)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def fold_over_term_test_case(x: hydra.testing.FoldOverTermTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.FoldOverTermTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.term(x.input)), hydra.core.Field(hydra.core.Name("traversalOrder"), hydra.encode.coders.traversal_order(x.traversal_order)), hydra.core.Field(hydra.core.Name("operation"), fold_operation(x.operation)), hydra.core.Field(hydra.core.Name("output"), hydra.encode.core.term(x.output))))))

def free_variables_test_case(x: hydra.testing.FreeVariablesTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.FreeVariablesTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.term(x.input)), hydra.core.Field(hydra.core.Name("output"), (lambda s: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map(hydra.encode.core.name, s))))(x.output))))))

def inference_failure_test_case(x: hydra.testing.InferenceFailureTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.InferenceFailureTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.term(x.input)),))))

def inference_test_case(x: hydra.testing.InferenceTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.InferenceTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.term(x.input)), hydra.core.Field(hydra.core.Name("output"), hydra.encode.core.type_scheme(x.output))))))

def json_coder_test_case(x: hydra.testing.JsonCoderTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.JsonCoderTestCase"), (hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.type(x.type)), hydra.core.Field(hydra.core.Name("term"), hydra.encode.core.term(x.term)), hydra.core.Field(hydra.core.Name("json"), hydra.encode.json.value(x.json))))))

def parser_test_case(a: Callable[[T0], hydra.core.Term], x: hydra.testing.ParserTestCase[T0]) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.ParserTestCase"), (hydra.core.Field(hydra.core.Name("input"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.input)), hydra.core.Field(hydra.core.Name("output"), hydra.encode.parsing.parse_result(a, x.output))))))

def json_parser_test_case(v1: hydra.testing.ParserTestCase[hydra.json.Value]) -> hydra.core.Type:
    return parser_test_case(hydra.encode.json.value, v1)

def writer_test_case(a: Callable[[T0], hydra.core.Term], x: hydra.testing.WriterTestCase[T0]) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.WriterTestCase"), (hydra.core.Field(hydra.core.Name("input"), a(x.input)), hydra.core.Field(hydra.core.Name("output"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.output))))))

def json_writer_test_case(v1: hydra.testing.WriterTestCase[hydra.json.Value]) -> hydra.core.Type:
    return writer_test_case(hydra.encode.json.value, v1)

def lift_lambda_above_let_test_case(x: hydra.testing.LiftLambdaAboveLetTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.LiftLambdaAboveLetTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.term(x.input)), hydra.core.Field(hydra.core.Name("output"), hydra.encode.core.term(x.output))))))

def normalize_type_variables_test_case(x: hydra.testing.NormalizeTypeVariablesTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.NormalizeTypeVariablesTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.term(x.input)), hydra.core.Field(hydra.core.Name("output"), hydra.encode.core.term(x.output))))))

def term_rewriter(v1: hydra.testing.TermRewriter) -> hydra.core.Type:
    match v1:
        case hydra.testing.TermRewriter.REPLACE_FOO_WITH_BAR:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TermRewriter"), hydra.core.Field(hydra.core.Name("replaceFooWithBar"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v)))))
        
        case hydra.testing.TermRewriter.REPLACE_INT32_WITH_INT64:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TermRewriter"), hydra.core.Field(hydra.core.Name("replaceInt32WithInt64"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v2)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def rewrite_term_test_case(x: hydra.testing.RewriteTermTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.RewriteTermTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.term(x.input)), hydra.core.Field(hydra.core.Name("rewriter"), term_rewriter(x.rewriter)), hydra.core.Field(hydra.core.Name("output"), hydra.encode.core.term(x.output))))))

def type_rewriter(v1: hydra.testing.TypeRewriter) -> hydra.core.Type:
    match v1:
        case hydra.testing.TypeRewriter.REPLACE_STRING_WITH_INT32:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TypeRewriter"), hydra.core.Field(hydra.core.Name("replaceStringWithInt32"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def rewrite_type_test_case(x: hydra.testing.RewriteTypeTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.RewriteTypeTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.type(x.input)), hydra.core.Field(hydra.core.Name("rewriter"), type_rewriter(x.rewriter)), hydra.core.Field(hydra.core.Name("output"), hydra.encode.core.type(x.output))))))

def serialization_test_case(x: hydra.testing.SerializationTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.SerializationTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.ast.expr(x.input)), hydra.core.Field(hydra.core.Name("output"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.output))))))

def simplify_term_test_case(x: hydra.testing.SimplifyTermTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.SimplifyTermTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.term(x.input)), hydra.core.Field(hydra.core.Name("output"), hydra.encode.core.term(x.output))))))

def tag(x: hydra.testing.Tag) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.testing.Tag"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.value))))

def topological_sort_bindings_test_case(x: hydra.testing.TopologicalSortBindingsTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TopologicalSortBindingsTestCase"), (hydra.core.Field(hydra.core.Name("bindings"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda p: cast(hydra.core.Term, hydra.core.TermPair(hydra.lib.pairs.bimap(hydra.encode.core.name, hydra.encode.core.term, p)))), xs))))(x.bindings)), hydra.core.Field(hydra.core.Name("expected"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda xs2: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda p: cast(hydra.core.Term, hydra.core.TermPair(hydra.lib.pairs.bimap(hydra.encode.core.name, hydra.encode.core.term, p)))), xs2)))), xs))))(x.expected))))))

def topological_sort_s_c_c_test_case(x: hydra.testing.TopologicalSortSCCTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TopologicalSortSCCTestCase"), (hydra.core.Field(hydra.core.Name("adjacencyList"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda p: cast(hydra.core.Term, hydra.core.TermPair(hydra.lib.pairs.bimap((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x2))))))), (lambda xs2: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x2))))))), xs2)))), p)))), xs))))(x.adjacency_list)), hydra.core.Field(hydra.core.Name("expected"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda xs2: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x2))))))), xs2)))), xs))))(x.expected))))))

def topological_sort_test_case(x: hydra.testing.TopologicalSortTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TopologicalSortTestCase"), (hydra.core.Field(hydra.core.Name("adjacencyList"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda p: cast(hydra.core.Term, hydra.core.TermPair(hydra.lib.pairs.bimap((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x2))))))), (lambda xs2: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x2))))))), xs2)))), p)))), xs))))(x.adjacency_list)), hydra.core.Field(hydra.core.Name("expected"), (lambda e: cast(hydra.core.Term, hydra.core.TermEither(hydra.lib.eithers.bimap((lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda xs2: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x2))))))), xs2)))), xs)))), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x2))))))), xs)))), e))))(x.expected))))))

def type_checking_failure_test_case(x: hydra.testing.TypeCheckingFailureTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TypeCheckingFailureTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.term(x.input)),))))

def type_checking_test_case(x: hydra.testing.TypeCheckingTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TypeCheckingTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.term(x.input)), hydra.core.Field(hydra.core.Name("outputTerm"), hydra.encode.core.term(x.output_term)), hydra.core.Field(hydra.core.Name("outputType"), hydra.encode.core.type(x.output_type))))))

def type_reduction_test_case(x: hydra.testing.TypeReductionTestCase) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TypeReductionTestCase"), (hydra.core.Field(hydra.core.Name("input"), hydra.encode.core.type(x.input)), hydra.core.Field(hydra.core.Name("output"), hydra.encode.core.type(x.output))))))

def test_case(v1: hydra.testing.TestCase) -> hydra.core.Type:
    match v1:
        case hydra.testing.TestCaseAlphaConversion(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("alphaConversion"), alpha_conversion_test_case(v)))))
        
        case hydra.testing.TestCaseCaseConversion(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("caseConversion"), case_conversion_test_case(v2)))))
        
        case hydra.testing.TestCaseDeannotateTerm(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("deannotateTerm"), deannotate_term_test_case(v3)))))
        
        case hydra.testing.TestCaseDeannotateType(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("deannotateType"), deannotate_type_test_case(v4)))))
        
        case hydra.testing.TestCaseDelegatedEvaluation(value=v5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("delegatedEvaluation"), delegated_evaluation_test_case(v5)))))
        
        case hydra.testing.TestCaseEtaExpansion(value=v6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("etaExpansion"), eta_expansion_test_case(v6)))))
        
        case hydra.testing.TestCaseFlattenLetTerms(value=v7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("flattenLetTerms"), flatten_let_terms_test_case(v7)))))
        
        case hydra.testing.TestCaseFreeVariables(value=v8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("freeVariables"), free_variables_test_case(v8)))))
        
        case hydra.testing.TestCaseEvaluation(value=v9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("evaluation"), evaluation_test_case(v9)))))
        
        case hydra.testing.TestCaseInference(value=v10):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("inference"), inference_test_case(v10)))))
        
        case hydra.testing.TestCaseInferenceFailure(value=v11):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("inferenceFailure"), inference_failure_test_case(v11)))))
        
        case hydra.testing.TestCaseJsonCoder(value=v12):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("jsonCoder"), json_coder_test_case(v12)))))
        
        case hydra.testing.TestCaseJsonParser(value=v13):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("jsonParser"), json_parser_test_case(v13)))))
        
        case hydra.testing.TestCaseJsonWriter(value=v14):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("jsonWriter"), json_writer_test_case(v14)))))
        
        case hydra.testing.TestCaseLiftLambdaAboveLet(value=v15):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("liftLambdaAboveLet"), lift_lambda_above_let_test_case(v15)))))
        
        case hydra.testing.TestCaseSerialization(value=v16):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("serialization"), serialization_test_case(v16)))))
        
        case hydra.testing.TestCaseSimplifyTerm(value=v17):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("simplifyTerm"), simplify_term_test_case(v17)))))
        
        case hydra.testing.TestCaseTopologicalSort(value=v18):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("topologicalSort"), topological_sort_test_case(v18)))))
        
        case hydra.testing.TestCaseTopologicalSortBindings(value=v19):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("topologicalSortBindings"), topological_sort_bindings_test_case(v19)))))
        
        case hydra.testing.TestCaseTopologicalSortSCC(value=v20):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("topologicalSortSCC"), topological_sort_s_c_c_test_case(v20)))))
        
        case hydra.testing.TestCaseTypeChecking(value=v21):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("typeChecking"), type_checking_test_case(v21)))))
        
        case hydra.testing.TestCaseTypeCheckingFailure(value=v22):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("typeCheckingFailure"), type_checking_failure_test_case(v22)))))
        
        case hydra.testing.TestCaseTypeReduction(value=v23):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("typeReduction"), type_reduction_test_case(v23)))))
        
        case hydra.testing.TestCaseNormalizeTypeVariables(value=v24):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("normalizeTypeVariables"), normalize_type_variables_test_case(v24)))))
        
        case hydra.testing.TestCaseFoldOverTerm(value=v25):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("foldOverTerm"), fold_over_term_test_case(v25)))))
        
        case hydra.testing.TestCaseRewriteTerm(value=v26):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("rewriteTerm"), rewrite_term_test_case(v26)))))
        
        case hydra.testing.TestCaseRewriteType(value=v27):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.testing.TestCase"), hydra.core.Field(hydra.core.Name("rewriteType"), rewrite_type_test_case(v27)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def test_case_with_metadata(x: hydra.testing.TestCaseWithMetadata) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TestCaseWithMetadata"), (hydra.core.Field(hydra.core.Name("name"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.name)), hydra.core.Field(hydra.core.Name("case"), test_case(x.case)), hydra.core.Field(hydra.core.Name("description"), (lambda opt: cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2))))), opt))))(x.description)), hydra.core.Field(hydra.core.Name("tags"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(tag, xs))))(x.tags))))))

def test_group(x: hydra.testing.TestGroup) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.testing.TestGroup"), (hydra.core.Field(hydra.core.Name("name"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.name)), hydra.core.Field(hydra.core.Name("description"), (lambda opt: cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2))))), opt))))(x.description)), hydra.core.Field(hydra.core.Name("subgroups"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(test_group, xs))))(x.subgroups)), hydra.core.Field(hydra.core.Name("cases"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(test_case_with_metadata, xs))))(x.cases))))))
