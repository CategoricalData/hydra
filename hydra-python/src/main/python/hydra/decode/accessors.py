# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.accessors."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import cast
import hydra.accessors
import hydra.core
import hydra.decode.core
import hydra.extract.helpers
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.util

def accessor_node(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.accessors.AccessorNode]:
    def _hoist_hydra_decode_accessors_accessor_node_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.accessors.AccessorNode]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                def _hoist_body_3(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_4(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_3(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", hydra.decode.core.name, field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("label", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_label: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("id", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_4(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_id: Right(hydra.accessors.AccessorNode(field_name, field_label, field_id))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.accessors.AccessorNode"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_accessors_accessor_node_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def term_accessor(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.accessors.TermAccessor]:
    def _hoist_hydra_decode_accessors_term_accessor_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.accessors.TermAccessor]:
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
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.accessors.TermAccessor]]]:
                    def _hoist_variant_map_1(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.IntegerValueInt32(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 value"))
                    def _hoist_variant_map_2(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.LiteralInteger(value=v1):
                                return _hoist_variant_map_1(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 literal"))
                    def _hoist_variant_map_3(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_2(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_4(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.IntegerValueInt32(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 value"))
                    def _hoist_variant_map_5(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.LiteralInteger(value=v1):
                                return _hoist_variant_map_4(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 literal"))
                    def _hoist_variant_map_6(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_5(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_7(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.IntegerValueInt32(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 value"))
                    def _hoist_variant_map_8(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.LiteralInteger(value=v1):
                                return _hoist_variant_map_7(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 literal"))
                    def _hoist_variant_map_9(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_8(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_10(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.IntegerValueInt32(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 value"))
                    def _hoist_variant_map_11(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.LiteralInteger(value=v1):
                                return _hoist_variant_map_10(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 literal"))
                    def _hoist_variant_map_12(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_11(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_13(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.IntegerValueInt32(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 value"))
                    def _hoist_variant_map_14(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.LiteralInteger(value=v1):
                                return _hoist_variant_map_13(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 literal"))
                    def _hoist_variant_map_15(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_14(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    return hydra.lib.maps.from_list(((hydra.core.Name("annotatedBody"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorAnnotatedBody())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("applicationFunction"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorApplicationFunction())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("applicationArgument"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorApplicationArgument())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("lambdaBody"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLambdaBody())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("unionCasesDefault"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorUnionCasesDefault())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("unionCasesBranch"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorUnionCasesBranch(t))), hydra.decode.core.name(cx, input)))), (hydra.core.Name("letBody"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLetBody())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("letBinding"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLetBinding(t))), hydra.decode.core.name(cx, input)))), (hydra.core.Name("listElement"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorListElement(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_3(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("mapKey"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMapKey(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_6(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("mapValue"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMapValue(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_9(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("maybeTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMaybeTerm())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("productTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorProductTerm(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_12(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("recordField"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorRecordField(t))), hydra.decode.core.name(cx, input)))), (hydra.core.Name("setElement"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorSetElement(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_15(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("sumTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorSumTerm())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("typeLambdaBody"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorTypeLambdaBody())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("typeApplicationTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorTypeApplicationTerm())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("injectionTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorInjectionTerm())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("wrappedTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorWrappedTerm())), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.accessors.TermAccessor"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_accessors_term_accessor_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def accessor_path(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.accessors.AccessorPath]:
    def _hoist_hydra_decode_accessors_accessor_path_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.accessors.AccessorPath]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.accessors.AccessorPath(b)), hydra.extract.helpers.decode_list(term_accessor, cx, wrapped_term.body))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.accessors.AccessorPath"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_accessors_accessor_path_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def accessor_edge(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.accessors.AccessorEdge]:
    def _hoist_hydra_decode_accessors_accessor_edge_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.accessors.AccessorEdge]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("source", accessor_node, field_map(), cx), (lambda field_source: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("path", accessor_path, field_map(), cx), (lambda field_path: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("target", accessor_node, field_map(), cx), (lambda field_target: Right(hydra.accessors.AccessorEdge(field_source, field_path, field_target))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.accessors.AccessorEdge"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_accessors_accessor_edge_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def accessor_graph(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.accessors.AccessorGraph]:
    def _hoist_hydra_decode_accessors_accessor_graph_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.accessors.AccessorGraph]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("nodes", (lambda v1, v2: hydra.extract.helpers.decode_list(accessor_node, v1, v2)), field_map(), cx), (lambda field_nodes: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("edges", (lambda v1, v2: hydra.extract.helpers.decode_list(accessor_edge, v1, v2)), field_map(), cx), (lambda field_edges: Right(hydra.accessors.AccessorGraph(field_nodes, field_edges))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.accessors.AccessorGraph"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_accessors_accessor_graph_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
