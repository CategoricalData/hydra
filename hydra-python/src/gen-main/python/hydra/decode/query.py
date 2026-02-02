# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.query."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Right, frozenlist
from typing import cast
import hydra.core
import hydra.decode.core
import hydra.extract.helpers
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.query
import hydra.util

def comparison_constraint(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.ComparisonConstraint]:
    def _hoist_hydra_decode_query_comparison_constraint_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.ComparisonConstraint]:
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
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.query.ComparisonConstraint]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("equal"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.query.ComparisonConstraint.EQUAL), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("notEqual"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.query.ComparisonConstraint.NOT_EQUAL), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("lessThan"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.query.ComparisonConstraint.LESS_THAN), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("greaterThan"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.query.ComparisonConstraint.GREATER_THAN), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("lessThanOrEqual"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.query.ComparisonConstraint.LESS_THAN_OR_EQUAL), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("greaterThanOrEqual"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.query.ComparisonConstraint.GREATER_THAN_OR_EQUAL), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.query.ComparisonConstraint"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_query_comparison_constraint_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def edge(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.Edge]:
    def _hoist_hydra_decode_query_edge_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.Edge]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", hydra.decode.core.name, field_map(), cx), (lambda field_type: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("out", (lambda v12, v2: hydra.extract.helpers.decode_maybe(hydra.decode.core.name, v12, v2)), field_map(), cx), (lambda field_out: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("in", (lambda v12, v2: hydra.extract.helpers.decode_maybe(hydra.decode.core.name, v12, v2)), field_map(), cx), (lambda field_in: Right(hydra.query.Edge(field_type, field_out, field_in))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.query.Edge"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_query_edge_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def variable(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.Variable]:
    def _hoist_hydra_decode_query_variable_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)
            
            case _:
                return Left(hydra.util.DecodingError("expected string literal"))
    def _hoist_hydra_decode_query_variable_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_query_variable_1(v)
            
            case _:
                return Left(hydra.util.DecodingError("expected literal"))
    def _hoist_hydra_decode_query_variable_3(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.Variable]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.query.Variable(b)), hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_hydra_decode_query_variable_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx, wrapped_term.body)))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.query.Variable"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_query_variable_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def node(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.Node_]:
    def _hoist_hydra_decode_query_node_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.Node_]:
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
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.query.Node_]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("term"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.Node_, hydra.query.NodeTerm(t))), hydra.decode.core.term(cx, input)))), (hydra.core.Name("variable"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.Node_, hydra.query.NodeVariable(t))), variable(cx, input)))), (hydra.core.Name("wildcard"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.Node_, hydra.query.NodeWildcard())), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.query.Node"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_query_node_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def range_(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.Range]:
    def _hoist_hydra_decode_query_range_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.Range]:
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
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("min", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_3(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_min: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("max", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_6(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_max: Right(hydra.query.Range(field_min, field_max))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.query.Range"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_query_range_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def regex_quantifier(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.RegexQuantifier]:
    def _hoist_hydra_decode_query_regex_quantifier_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.RegexQuantifier]:
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
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.query.RegexQuantifier]]]:
                    def _hoist_variant_map_1(v12: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v12:
                            case hydra.core.IntegerValueInt32(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 value"))
                    def _hoist_variant_map_2(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v12:
                            case hydra.core.LiteralInteger(value=v13):
                                return _hoist_variant_map_1(v13)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 literal"))
                    def _hoist_variant_map_3(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v12:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_2(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_4(v12: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v12:
                            case hydra.core.IntegerValueInt32(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 value"))
                    def _hoist_variant_map_5(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v12:
                            case hydra.core.LiteralInteger(value=v13):
                                return _hoist_variant_map_4(v13)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 literal"))
                    def _hoist_variant_map_6(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v12:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_5(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    return hydra.lib.maps.from_list(((hydra.core.Name("one"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.RegexQuantifier, hydra.query.RegexQuantifierOne())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("zeroOrOne"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.RegexQuantifier, hydra.query.RegexQuantifierZeroOrOne())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("zeroOrMore"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.RegexQuantifier, hydra.query.RegexQuantifierZeroOrMore())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("oneOrMore"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.RegexQuantifier, hydra.query.RegexQuantifierOneOrMore())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("exactly"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.RegexQuantifier, hydra.query.RegexQuantifierExactly(t))), hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_3(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx, input))))), (hydra.core.Name("atLeast"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.RegexQuantifier, hydra.query.RegexQuantifierAtLeast(t))), hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_6(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx, input))))), (hydra.core.Name("range"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.RegexQuantifier, hydra.query.RegexQuantifierRange(t))), range_(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.query.RegexQuantifier"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_query_regex_quantifier_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def step(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.Step]:
    def _hoist_hydra_decode_query_step_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.Step]:
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
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.query.Step]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("edge"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.Step, hydra.query.StepEdge(t))), edge(cx, input)))), (hydra.core.Name("project"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.Step, hydra.query.StepProject(t))), hydra.decode.core.projection(cx, input)))), (hydra.core.Name("compare"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.Step, hydra.query.StepCompare(t))), comparison_constraint(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.query.Step"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_query_step_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def path(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.Path]:
    def _hoist_hydra_decode_query_path_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.Path]:
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
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.query.Path]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("step"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.Path, hydra.query.PathStep(t))), step(cx, input)))), (hydra.core.Name("regex"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.Path, hydra.query.PathRegex(t))), regex_sequence(cx, input)))), (hydra.core.Name("inverse"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.Path, hydra.query.PathInverse(t))), path(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.query.Path"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_query_path_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def regex_sequence(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.RegexSequence]:
    def _hoist_hydra_decode_query_regex_sequence_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.RegexSequence]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("path", path, field_map(), cx), (lambda field_path: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("quantifier", regex_quantifier, field_map(), cx), (lambda field_quantifier: Right(hydra.query.RegexSequence(field_path, field_quantifier))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.query.RegexSequence"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_query_regex_sequence_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def triple_pattern(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.TriplePattern]:
    def _hoist_hydra_decode_query_triple_pattern_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.TriplePattern]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("subject", node, field_map(), cx), (lambda field_subject: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("predicate", path, field_map(), cx), (lambda field_predicate: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("object", node, field_map(), cx), (lambda field_object: Right(hydra.query.TriplePattern(field_subject, field_predicate, field_object))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.query.TriplePattern"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_query_triple_pattern_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def graph_pattern(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.GraphPattern]:
    def _hoist_hydra_decode_query_graph_pattern_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.GraphPattern]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("graph", hydra.decode.core.name, field_map(), cx), (lambda field_graph: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("patterns", (lambda v12, v2: hydra.extract.helpers.decode_list(pattern, v12, v2)), field_map(), cx), (lambda field_patterns: Right(hydra.query.GraphPattern(field_graph, field_patterns))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.query.GraphPattern"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_query_graph_pattern_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def pattern(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.Pattern]:
    def _hoist_hydra_decode_query_pattern_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.Pattern]:
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
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.query.Pattern]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("triple"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.Pattern, hydra.query.PatternTriple(t))), triple_pattern(cx, input)))), (hydra.core.Name("negation"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.Pattern, hydra.query.PatternNegation(t))), pattern(cx, input)))), (hydra.core.Name("conjunction"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.Pattern, hydra.query.PatternConjunction(t))), hydra.extract.helpers.decode_list(pattern, cx, input)))), (hydra.core.Name("disjunction"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.Pattern, hydra.query.PatternDisjunction(t))), hydra.extract.helpers.decode_list(pattern, cx, input)))), (hydra.core.Name("graph"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.query.Pattern, hydra.query.PatternGraph(t))), graph_pattern(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.query.Pattern"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_query_pattern_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def query(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.Query]:
    def _hoist_hydra_decode_query_query_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.query.Query]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("variables", (lambda v12, v2: hydra.extract.helpers.decode_list(variable, v12, v2)), field_map(), cx), (lambda field_variables: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("patterns", (lambda v12, v2: hydra.extract.helpers.decode_list(pattern, v12, v2)), field_map(), cx), (lambda field_patterns: Right(hydra.query.Query(field_variables, field_patterns))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.query.Query"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_query_query_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
