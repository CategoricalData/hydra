# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.testing."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.decode.ast
import hydra.decode.coders
import hydra.decode.core
import hydra.decode.json.model
import hydra.decode.parsing
import hydra.decode.util
import hydra.extract.helpers
import hydra.graph
import hydra.json.model
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.testing
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def alpha_conversion_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase]:
    def _hoist_hydra_decode_testing_alpha_conversion_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.AlphaConversionTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("term", hydra.decode.core.term, field_map(), cx), (lambda field_term: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("oldVariable", hydra.decode.core.name, field_map(), cx), (lambda field_old_variable: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("newVariable", hydra.decode.core.name, field_map(), cx), (lambda field_new_variable: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("result", hydra.decode.core.term, field_map(), cx), (lambda field_result: Right(hydra.testing.AlphaConversionTestCase(field_term, field_old_variable, field_new_variable, field_result))))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.AlphaConversionTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_alpha_conversion_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def case_conversion_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.CaseConversionTestCase]:
    def _hoist_hydra_decode_testing_case_conversion_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.CaseConversionTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_2(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                def _hoist_body_3(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_4(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_3(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("fromConvention", hydra.decode.util.case_convention, field_map(), cx), (lambda field_from_convention: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("toConvention", hydra.decode.util.case_convention, field_map(), cx), (lambda field_to_convention: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("fromString", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_from_string: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("toString", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_4(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_to_string: Right(hydra.testing.CaseConversionTestCase(field_from_convention, field_to_convention, field_from_string, field_to_string))))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.CaseConversionTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_case_conversion_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def deannotate_term_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase]:
    def _hoist_hydra_decode_testing_deannotate_term_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.DeannotateTermTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.term, field_map(), cx), (lambda field_output: Right(hydra.testing.DeannotateTermTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.DeannotateTermTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_deannotate_term_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def deannotate_type_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase]:
    def _hoist_hydra_decode_testing_deannotate_type_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.DeannotateTypeTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.type, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.type, field_map(), cx), (lambda field_output: Right(hydra.testing.DeannotateTypeTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.DeannotateTypeTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_deannotate_type_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def delegated_evaluation_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase]:
    def _hoist_hydra_decode_testing_delegated_evaluation_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.DelegatedEvaluationTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.term, field_map(), cx), (lambda field_output: Right(hydra.testing.DelegatedEvaluationTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.DelegatedEvaluationTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_delegated_evaluation_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def eta_expansion_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase]:
    def _hoist_hydra_decode_testing_eta_expansion_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.EtaExpansionTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.term, field_map(), cx), (lambda field_output: Right(hydra.testing.EtaExpansionTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.EtaExpansionTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_eta_expansion_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def evaluation_style(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.EvaluationStyle]:
    def _hoist_hydra_decode_testing_evaluation_style_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.EvaluationStyle]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                @lru_cache(1)
                def tname() -> hydra.core.Type:
                    return inj.type_name
                @lru_cache(1)
                def field() -> hydra.core.Type:
                    return inj.field
                @lru_cache(1)
                def fname() -> hydra.core.Type:
                    return field().name
                @lru_cache(1)
                def fterm() -> hydra.core.Type:
                    return field().term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.testing.EvaluationStyle]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("eager"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.testing.EvaluationStyle.EAGER), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("lazy"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.testing.EvaluationStyle.LAZY), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.testing.EvaluationStyle"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_evaluation_style_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def evaluation_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.EvaluationTestCase]:
    def _hoist_hydra_decode_testing_evaluation_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.EvaluationTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("evaluationStyle", evaluation_style, field_map(), cx), (lambda field_evaluation_style: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.term, field_map(), cx), (lambda field_output: Right(hydra.testing.EvaluationTestCase(field_evaluation_style, field_input, field_output))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.EvaluationTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_evaluation_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def flatten_let_terms_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase]:
    def _hoist_hydra_decode_testing_flatten_let_terms_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.FlattenLetTermsTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.term, field_map(), cx), (lambda field_output: Right(hydra.testing.FlattenLetTermsTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.FlattenLetTermsTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_flatten_let_terms_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def fold_operation(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.FoldOperation]:
    def _hoist_hydra_decode_testing_fold_operation_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.FoldOperation]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                @lru_cache(1)
                def tname() -> hydra.core.Type:
                    return inj.type_name
                @lru_cache(1)
                def field() -> hydra.core.Type:
                    return inj.field
                @lru_cache(1)
                def fname() -> hydra.core.Type:
                    return field().name
                @lru_cache(1)
                def fterm() -> hydra.core.Type:
                    return field().term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.testing.FoldOperation]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("sumInt32Literals"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.testing.FoldOperation.SUM_INT32_LITERALS), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("collectListLengths"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.testing.FoldOperation.COLLECT_LIST_LENGTHS), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("collectLabels"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.testing.FoldOperation.COLLECT_LABELS), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.testing.FoldOperation"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_fold_operation_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def fold_over_term_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase]:
    def _hoist_hydra_decode_testing_fold_over_term_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.FoldOverTermTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("traversalOrder", hydra.decode.coders.traversal_order, field_map(), cx), (lambda field_traversal_order: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("operation", fold_operation, field_map(), cx), (lambda field_operation: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.term, field_map(), cx), (lambda field_output: Right(hydra.testing.FoldOverTermTestCase(field_input, field_traversal_order, field_operation, field_output))))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.FoldOverTermTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_fold_over_term_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def free_variables_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase]:
    def _hoist_hydra_decode_testing_free_variables_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.FreeVariablesTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", (lambda v12, v2: hydra.extract.helpers.decode_set(hydra.decode.core.name, v12, v2)), field_map(), cx), (lambda field_output: Right(hydra.testing.FreeVariablesTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.FreeVariablesTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_free_variables_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def hoist_case_statements_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase]:
    def _hoist_hydra_decode_testing_hoist_case_statements_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.HoistCaseStatementsTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.term, field_map(), cx), (lambda field_output: Right(hydra.testing.HoistCaseStatementsTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.HoistCaseStatementsTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_hoist_case_statements_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def hoist_let_bindings_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase]:
    def _hoist_hydra_decode_testing_hoist_let_bindings_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.HoistLetBindingsTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.let, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.let, field_map(), cx), (lambda field_output: Right(hydra.testing.HoistLetBindingsTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.HoistLetBindingsTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_hoist_let_bindings_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def hoist_polymorphic_let_bindings_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase]:
    def _hoist_hydra_decode_testing_hoist_polymorphic_let_bindings_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.HoistPolymorphicLetBindingsTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.let, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.let, field_map(), cx), (lambda field_output: Right(hydra.testing.HoistPolymorphicLetBindingsTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.HoistPolymorphicLetBindingsTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_hoist_polymorphic_let_bindings_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def hoist_predicate(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.HoistPredicate]:
    def _hoist_hydra_decode_testing_hoist_predicate_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.HoistPredicate]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                @lru_cache(1)
                def tname() -> hydra.core.Type:
                    return inj.type_name
                @lru_cache(1)
                def field() -> hydra.core.Type:
                    return inj.field
                @lru_cache(1)
                def fname() -> hydra.core.Type:
                    return field().name
                @lru_cache(1)
                def fterm() -> hydra.core.Type:
                    return field().term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.testing.HoistPredicate]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("caseStatements"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.testing.HoistPredicate.CASE_STATEMENTS), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("applications"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.testing.HoistPredicate.APPLICATIONS), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("lists"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.testing.HoistPredicate.LISTS), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("nothing"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.testing.HoistPredicate.NOTHING), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.testing.HoistPredicate"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_hoist_predicate_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def hoist_subterms_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase]:
    def _hoist_hydra_decode_testing_hoist_subterms_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.HoistSubtermsTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("predicate", hoist_predicate, field_map(), cx), (lambda field_predicate: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.term, field_map(), cx), (lambda field_output: Right(hydra.testing.HoistSubtermsTestCase(field_predicate, field_input, field_output))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.HoistSubtermsTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_hoist_subterms_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def inference_failure_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase]:
    def _hoist_hydra_decode_testing_inference_failure_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.InferenceFailureTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: Right(hydra.testing.InferenceFailureTestCase(field_input))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.InferenceFailureTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_inference_failure_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def inference_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.InferenceTestCase]:
    def _hoist_hydra_decode_testing_inference_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.InferenceTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.type_scheme, field_map(), cx), (lambda field_output: Right(hydra.testing.InferenceTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.InferenceTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_inference_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def json_coder_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.JsonCoderTestCase]:
    def _hoist_hydra_decode_testing_json_coder_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.JsonCoderTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", hydra.decode.core.type, field_map(), cx), (lambda field_type: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("term", hydra.decode.core.term, field_map(), cx), (lambda field_term: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("json", hydra.decode.json.model.value, field_map(), cx), (lambda field_json: Right(hydra.testing.JsonCoderTestCase(field_type, field_term, field_json))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.JsonCoderTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_json_coder_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def json_decode_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase]:
    def _hoist_hydra_decode_testing_json_decode_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.JsonDecodeTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_2(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", hydra.decode.core.type, field_map(), cx), (lambda field_type: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("json", hydra.decode.json.model.value, field_map(), cx), (lambda field_json: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("expected", (lambda v12, v2: hydra.extract.helpers.decode_either((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), hydra.decode.core.term, v12, v2)), field_map(), cx), (lambda field_expected: Right(hydra.testing.JsonDecodeTestCase(field_type, field_json, field_expected))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.JsonDecodeTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_json_decode_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def json_encode_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase]:
    def _hoist_hydra_decode_testing_json_encode_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.JsonEncodeTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_2(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("term", hydra.decode.core.term, field_map(), cx), (lambda field_term: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("expected", (lambda v12, v2: hydra.extract.helpers.decode_either((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), hydra.decode.json.model.value, v12, v2)), field_map(), cx), (lambda field_expected: Right(hydra.testing.JsonEncodeTestCase(field_term, field_expected))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.JsonEncodeTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_json_encode_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def parser_test_case(a: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.ParserTestCase[T0]]:
    def _hoist_hydra_decode_testing_parser_test_case_1(a: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.ParserTestCase[T1]]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_2(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", (lambda v12, v2: hydra.decode.parsing.parse_result(a, v12, v2)), field_map(), cx), (lambda field_output: Right(hydra.testing.ParserTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.ParserTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_parser_test_case_1(a, cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def json_parser_test_case(v1: hydra.graph.Graph, v2: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.ParserTestCase[hydra.json.model.Value]]:
    return parser_test_case(hydra.decode.json.model.value, v1, v2)

def json_roundtrip_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase]:
    def _hoist_hydra_decode_testing_json_roundtrip_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.JsonRoundtripTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", hydra.decode.core.type, field_map(), cx), (lambda field_type: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("term", hydra.decode.core.term, field_map(), cx), (lambda field_term: Right(hydra.testing.JsonRoundtripTestCase(field_type, field_term))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.JsonRoundtripTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_json_roundtrip_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def writer_test_case(a: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.WriterTestCase[T0]]:
    def _hoist_hydra_decode_testing_writer_test_case_1(a: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.WriterTestCase[T1]]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_2(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", a, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_output: Right(hydra.testing.WriterTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.WriterTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_writer_test_case_1(a, cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def json_writer_test_case(v1: hydra.graph.Graph, v2: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.WriterTestCase[hydra.json.model.Value]]:
    return writer_test_case(hydra.decode.json.model.value, v1, v2)

def lift_lambda_above_let_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase]:
    def _hoist_hydra_decode_testing_lift_lambda_above_let_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.LiftLambdaAboveLetTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.term, field_map(), cx), (lambda field_output: Right(hydra.testing.LiftLambdaAboveLetTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.LiftLambdaAboveLetTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_lift_lambda_above_let_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def normalize_type_variables_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase]:
    def _hoist_hydra_decode_testing_normalize_type_variables_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.NormalizeTypeVariablesTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.term, field_map(), cx), (lambda field_output: Right(hydra.testing.NormalizeTypeVariablesTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.NormalizeTypeVariablesTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_normalize_type_variables_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def term_rewriter(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TermRewriter]:
    def _hoist_hydra_decode_testing_term_rewriter_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TermRewriter]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                @lru_cache(1)
                def tname() -> hydra.core.Type:
                    return inj.type_name
                @lru_cache(1)
                def field() -> hydra.core.Type:
                    return inj.field
                @lru_cache(1)
                def fname() -> hydra.core.Type:
                    return field().name
                @lru_cache(1)
                def fterm() -> hydra.core.Type:
                    return field().term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.testing.TermRewriter]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("replaceFooWithBar"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.testing.TermRewriter.REPLACE_FOO_WITH_BAR), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("replaceInt32WithInt64"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.testing.TermRewriter.REPLACE_INT32_WITH_INT64), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.testing.TermRewriter"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_term_rewriter_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def rewrite_term_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.RewriteTermTestCase]:
    def _hoist_hydra_decode_testing_rewrite_term_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.RewriteTermTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("rewriter", term_rewriter, field_map(), cx), (lambda field_rewriter: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.term, field_map(), cx), (lambda field_output: Right(hydra.testing.RewriteTermTestCase(field_input, field_rewriter, field_output))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.RewriteTermTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_rewrite_term_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_rewriter(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TypeRewriter]:
    def _hoist_hydra_decode_testing_type_rewriter_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TypeRewriter]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                @lru_cache(1)
                def tname() -> hydra.core.Type:
                    return inj.type_name
                @lru_cache(1)
                def field() -> hydra.core.Type:
                    return inj.field
                @lru_cache(1)
                def fname() -> hydra.core.Type:
                    return field().name
                @lru_cache(1)
                def fterm() -> hydra.core.Type:
                    return field().term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.testing.TypeRewriter]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("replaceStringWithInt32"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.testing.TypeRewriter.REPLACE_STRING_WITH_INT32), hydra.extract.helpers.decode_unit(cx, input)))),))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.testing.TypeRewriter"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_type_rewriter_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def rewrite_type_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase]:
    def _hoist_hydra_decode_testing_rewrite_type_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.RewriteTypeTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.type, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("rewriter", type_rewriter, field_map(), cx), (lambda field_rewriter: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.type, field_map(), cx), (lambda field_output: Right(hydra.testing.RewriteTypeTestCase(field_input, field_rewriter, field_output))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.RewriteTypeTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_rewrite_type_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def serialization_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.SerializationTestCase]:
    def _hoist_hydra_decode_testing_serialization_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.SerializationTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_2(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.ast.expr, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_output: Right(hydra.testing.SerializationTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.SerializationTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_serialization_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def simplify_term_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase]:
    def _hoist_hydra_decode_testing_simplify_term_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.SimplifyTermTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.term, field_map(), cx), (lambda field_output: Right(hydra.testing.SimplifyTermTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.SimplifyTermTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_simplify_term_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def tag(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.Tag]:
    def _hoist_hydra_decode_testing_tag_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)
            
            case _:
                return Left(hydra.util.DecodingError("expected string literal"))
    def _hoist_hydra_decode_testing_tag_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_testing_tag_1(v)
            
            case _:
                return Left(hydra.util.DecodingError("expected literal"))
    def _hoist_hydra_decode_testing_tag_3(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.Tag]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.testing.Tag(b)), hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_hydra_decode_testing_tag_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx, wrapped_term.body)))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.testing.Tag"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_tag_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def topological_sort_bindings_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase]:
    def _hoist_hydra_decode_testing_topological_sort_bindings_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TopologicalSortBindingsTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("bindings", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda v13, v22: hydra.extract.helpers.decode_pair(hydra.decode.core.name, hydra.decode.core.term, v13, v22)), v12, v2)), field_map(), cx), (lambda field_bindings: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("expected", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda v13, v22: hydra.extract.helpers.decode_list((lambda v14, v23: hydra.extract.helpers.decode_pair(hydra.decode.core.name, hydra.decode.core.term, v14, v23)), v13, v22)), v12, v2)), field_map(), cx), (lambda field_expected: Right(hydra.testing.TopologicalSortBindingsTestCase(field_bindings, field_expected))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.TopologicalSortBindingsTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_topological_sort_bindings_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def topological_sort_s_c_c_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase]:
    def _hoist_hydra_decode_testing_topological_sort_s_c_c_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TopologicalSortSCCTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v12: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.IntegerValueInt32(value=i):
                            return Right(i)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 value"))
                def _hoist_body_2(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.LiteralInteger(value=v13):
                            return _hoist_body_1(v13)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 literal"))
                def _hoist_body_3(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_2(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                def _hoist_body_4(v12: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.IntegerValueInt32(value=i):
                            return Right(i)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 value"))
                def _hoist_body_5(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.LiteralInteger(value=v13):
                            return _hoist_body_4(v13)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 literal"))
                def _hoist_body_6(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_5(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                def _hoist_body_7(v12: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.IntegerValueInt32(value=i):
                            return Right(i)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 value"))
                def _hoist_body_8(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.LiteralInteger(value=v13):
                            return _hoist_body_7(v13)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 literal"))
                def _hoist_body_9(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_8(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("adjacencyList", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda v13, v22: hydra.extract.helpers.decode_pair((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_3(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), (lambda v14, v23: hydra.extract.helpers.decode_list((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_6(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v14, v23)), v13, v22)), v12, v2)), field_map(), cx), (lambda field_adjacency_list: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("expected", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda v13, v22: hydra.extract.helpers.decode_list((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_9(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v13, v22)), v12, v2)), field_map(), cx), (lambda field_expected: Right(hydra.testing.TopologicalSortSCCTestCase(field_adjacency_list, field_expected))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.TopologicalSortSCCTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_topological_sort_s_c_c_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def topological_sort_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase]:
    def _hoist_hydra_decode_testing_topological_sort_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TopologicalSortTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v12: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.IntegerValueInt32(value=i):
                            return Right(i)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 value"))
                def _hoist_body_2(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.LiteralInteger(value=v13):
                            return _hoist_body_1(v13)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 literal"))
                def _hoist_body_3(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_2(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                def _hoist_body_4(v12: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.IntegerValueInt32(value=i):
                            return Right(i)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 value"))
                def _hoist_body_5(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.LiteralInteger(value=v13):
                            return _hoist_body_4(v13)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 literal"))
                def _hoist_body_6(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_5(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                def _hoist_body_7(v12: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.IntegerValueInt32(value=i):
                            return Right(i)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 value"))
                def _hoist_body_8(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.LiteralInteger(value=v13):
                            return _hoist_body_7(v13)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 literal"))
                def _hoist_body_9(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_8(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                def _hoist_body_10(v12: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.IntegerValueInt32(value=i):
                            return Right(i)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 value"))
                def _hoist_body_11(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.LiteralInteger(value=v13):
                            return _hoist_body_10(v13)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 literal"))
                def _hoist_body_12(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_11(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("adjacencyList", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda v13, v22: hydra.extract.helpers.decode_pair((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_3(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), (lambda v14, v23: hydra.extract.helpers.decode_list((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_6(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v14, v23)), v13, v22)), v12, v2)), field_map(), cx), (lambda field_adjacency_list: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("expected", (lambda v12, v2: hydra.extract.helpers.decode_either((lambda v13, v22: hydra.extract.helpers.decode_list((lambda v14, v23: hydra.extract.helpers.decode_list((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_9(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v14, v23)), v13, v22)), (lambda v13, v22: hydra.extract.helpers.decode_list((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_12(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v13, v22)), v12, v2)), field_map(), cx), (lambda field_expected: Right(hydra.testing.TopologicalSortTestCase(field_adjacency_list, field_expected))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.TopologicalSortTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_topological_sort_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_checking_failure_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase]:
    def _hoist_hydra_decode_testing_type_checking_failure_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TypeCheckingFailureTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: Right(hydra.testing.TypeCheckingFailureTestCase(field_input))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.TypeCheckingFailureTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_type_checking_failure_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_checking_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase]:
    def _hoist_hydra_decode_testing_type_checking_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TypeCheckingTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.term, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("outputTerm", hydra.decode.core.term, field_map(), cx), (lambda field_output_term: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("outputType", hydra.decode.core.type, field_map(), cx), (lambda field_output_type: Right(hydra.testing.TypeCheckingTestCase(field_input, field_output_term, field_output_type))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.TypeCheckingTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_type_checking_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_reduction_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TypeReductionTestCase]:
    def _hoist_hydra_decode_testing_type_reduction_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TypeReductionTestCase]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("input", hydra.decode.core.type, field_map(), cx), (lambda field_input: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("output", hydra.decode.core.type, field_map(), cx), (lambda field_output: Right(hydra.testing.TypeReductionTestCase(field_input, field_output))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.TypeReductionTestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_type_reduction_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def test_case(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TestCase]:
    def _hoist_hydra_decode_testing_test_case_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TestCase]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                @lru_cache(1)
                def tname() -> hydra.core.Type:
                    return inj.type_name
                @lru_cache(1)
                def field() -> hydra.core.Type:
                    return inj.field
                @lru_cache(1)
                def fname() -> hydra.core.Type:
                    return field().name
                @lru_cache(1)
                def fterm() -> hydra.core.Type:
                    return field().term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.testing.TestCase]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("alphaConversion"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseAlphaConversion(t))), alpha_conversion_test_case(cx, input)))), (hydra.core.Name("caseConversion"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseCaseConversion(t))), case_conversion_test_case(cx, input)))), (hydra.core.Name("deannotateTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseDeannotateTerm(t))), deannotate_term_test_case(cx, input)))), (hydra.core.Name("deannotateType"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseDeannotateType(t))), deannotate_type_test_case(cx, input)))), (hydra.core.Name("delegatedEvaluation"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseDelegatedEvaluation(t))), delegated_evaluation_test_case(cx, input)))), (hydra.core.Name("etaExpansion"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseEtaExpansion(t))), eta_expansion_test_case(cx, input)))), (hydra.core.Name("flattenLetTerms"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseFlattenLetTerms(t))), flatten_let_terms_test_case(cx, input)))), (hydra.core.Name("freeVariables"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseFreeVariables(t))), free_variables_test_case(cx, input)))), (hydra.core.Name("evaluation"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseEvaluation(t))), evaluation_test_case(cx, input)))), (hydra.core.Name("inference"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseInference(t))), inference_test_case(cx, input)))), (hydra.core.Name("inferenceFailure"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseInferenceFailure(t))), inference_failure_test_case(cx, input)))), (hydra.core.Name("jsonCoder"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseJsonCoder(t))), json_coder_test_case(cx, input)))), (hydra.core.Name("jsonDecode"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseJsonDecode(t))), json_decode_test_case(cx, input)))), (hydra.core.Name("jsonEncode"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseJsonEncode(t))), json_encode_test_case(cx, input)))), (hydra.core.Name("jsonParser"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseJsonParser(t))), json_parser_test_case(cx, input)))), (hydra.core.Name("jsonRoundtrip"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseJsonRoundtrip(t))), json_roundtrip_test_case(cx, input)))), (hydra.core.Name("jsonWriter"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseJsonWriter(t))), json_writer_test_case(cx, input)))), (hydra.core.Name("liftLambdaAboveLet"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseLiftLambdaAboveLet(t))), lift_lambda_above_let_test_case(cx, input)))), (hydra.core.Name("serialization"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseSerialization(t))), serialization_test_case(cx, input)))), (hydra.core.Name("simplifyTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseSimplifyTerm(t))), simplify_term_test_case(cx, input)))), (hydra.core.Name("topologicalSort"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseTopologicalSort(t))), topological_sort_test_case(cx, input)))), (hydra.core.Name("topologicalSortBindings"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseTopologicalSortBindings(t))), topological_sort_bindings_test_case(cx, input)))), (hydra.core.Name("topologicalSortSCC"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseTopologicalSortSCC(t))), topological_sort_s_c_c_test_case(cx, input)))), (hydra.core.Name("typeChecking"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseTypeChecking(t))), type_checking_test_case(cx, input)))), (hydra.core.Name("typeCheckingFailure"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseTypeCheckingFailure(t))), type_checking_failure_test_case(cx, input)))), (hydra.core.Name("typeReduction"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseTypeReduction(t))), type_reduction_test_case(cx, input)))), (hydra.core.Name("normalizeTypeVariables"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseNormalizeTypeVariables(t))), normalize_type_variables_test_case(cx, input)))), (hydra.core.Name("foldOverTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseFoldOverTerm(t))), fold_over_term_test_case(cx, input)))), (hydra.core.Name("rewriteTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseRewriteTerm(t))), rewrite_term_test_case(cx, input)))), (hydra.core.Name("rewriteType"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseRewriteType(t))), rewrite_type_test_case(cx, input)))), (hydra.core.Name("hoistSubterms"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseHoistSubterms(t))), hoist_subterms_test_case(cx, input)))), (hydra.core.Name("hoistCaseStatements"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseHoistCaseStatements(t))), hoist_case_statements_test_case(cx, input)))), (hydra.core.Name("hoistLetBindings"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseHoistLetBindings(t))), hoist_let_bindings_test_case(cx, input)))), (hydra.core.Name("hoistPolymorphicLetBindings"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseHoistPolymorphicLetBindings(t))), hoist_polymorphic_let_bindings_test_case(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.testing.TestCase"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_test_case_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def test_case_with_metadata(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata]:
    def _hoist_hydra_decode_testing_test_case_with_metadata_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TestCaseWithMetadata]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_2(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                def _hoist_body_3(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_4(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_3(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("case", test_case, field_map(), cx), (lambda field_case: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("description", (lambda v12, v2: hydra.extract.helpers.decode_maybe((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_4(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v12, v2)), field_map(), cx), (lambda field_description: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("tags", (lambda v12, v2: hydra.extract.helpers.decode_list(tag, v12, v2)), field_map(), cx), (lambda field_tags: Right(hydra.testing.TestCaseWithMetadata(field_name, field_case, field_description, field_tags))))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.TestCaseWithMetadata"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_test_case_with_metadata_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def test_group(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TestGroup]:
    def _hoist_hydra_decode_testing_test_group_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.testing.TestGroup]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_2(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                def _hoist_body_3(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_4(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_3(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("description", (lambda v12, v2: hydra.extract.helpers.decode_maybe((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_4(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v12, v2)), field_map(), cx), (lambda field_description: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("subgroups", (lambda v12, v2: hydra.extract.helpers.decode_list(test_group, v12, v2)), field_map(), cx), (lambda field_subgroups: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("cases", (lambda v12, v2: hydra.extract.helpers.decode_list(test_case_with_metadata, v12, v2)), field_map(), cx), (lambda field_cases: Right(hydra.testing.TestGroup(field_name, field_description, field_subgroups, field_cases))))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.testing.TestGroup"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_testing_test_group_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
