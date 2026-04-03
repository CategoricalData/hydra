# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.paths."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import cast
import hydra.core
import hydra.decode.core
import hydra.errors
import hydra.extract.core
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.paths

def subterm_node(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_paths_subterm_node_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                def _hoist_field_map_body_1(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_field_map_body_2(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_1(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_field_map_body_3(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_field_map_body_4(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_3(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.core.require_field("label", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_field_map_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_label: hydra.lib.eithers.bind(hydra.extract.core.require_field("id", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_field_map_body_4(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_id: Right(hydra.paths.SubtermNode(field_name, field_label, field_id))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_paths_subterm_node_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def subterm_step(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_paths_subterm_step_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map():
                    def _hoist_variant_map_1(v12):
                        match v12:
                            case hydra.core.IntegerValueInt32(value=i):
                                return Right(i)

                            case _:
                                return Left(hydra.errors.DecodingError("expected int32 value"))
                    def _hoist_variant_map_2(v12):
                        match v12:
                            case hydra.core.LiteralInteger(value=_match_value):
                                return _hoist_variant_map_1(_match_value)

                            case _:
                                return Left(hydra.errors.DecodingError("expected int32 literal"))
                    def _hoist_variant_map_3(v12):
                        match v12:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_2(v)

                            case _:
                                return Left(hydra.errors.DecodingError("expected literal"))
                    def _hoist_variant_map_4(v12):
                        match v12:
                            case hydra.core.IntegerValueInt32(value=i):
                                return Right(i)

                            case _:
                                return Left(hydra.errors.DecodingError("expected int32 value"))
                    def _hoist_variant_map_5(v12):
                        match v12:
                            case hydra.core.LiteralInteger(value=_match_value):
                                return _hoist_variant_map_4(_match_value)

                            case _:
                                return Left(hydra.errors.DecodingError("expected int32 literal"))
                    def _hoist_variant_map_6(v12):
                        match v12:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_5(v)

                            case _:
                                return Left(hydra.errors.DecodingError("expected literal"))
                    def _hoist_variant_map_7(v12):
                        match v12:
                            case hydra.core.IntegerValueInt32(value=i):
                                return Right(i)

                            case _:
                                return Left(hydra.errors.DecodingError("expected int32 value"))
                    def _hoist_variant_map_8(v12):
                        match v12:
                            case hydra.core.LiteralInteger(value=_match_value):
                                return _hoist_variant_map_7(_match_value)

                            case _:
                                return Left(hydra.errors.DecodingError("expected int32 literal"))
                    def _hoist_variant_map_9(v12):
                        match v12:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_8(v)

                            case _:
                                return Left(hydra.errors.DecodingError("expected literal"))
                    def _hoist_variant_map_10(v12):
                        match v12:
                            case hydra.core.IntegerValueInt32(value=i):
                                return Right(i)

                            case _:
                                return Left(hydra.errors.DecodingError("expected int32 value"))
                    def _hoist_variant_map_11(v12):
                        match v12:
                            case hydra.core.LiteralInteger(value=_match_value):
                                return _hoist_variant_map_10(_match_value)

                            case _:
                                return Left(hydra.errors.DecodingError("expected int32 literal"))
                    def _hoist_variant_map_12(v12):
                        match v12:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_11(v)

                            case _:
                                return Left(hydra.errors.DecodingError("expected literal"))
                    def _hoist_variant_map_13(v12):
                        match v12:
                            case hydra.core.IntegerValueInt32(value=i):
                                return Right(i)

                            case _:
                                return Left(hydra.errors.DecodingError("expected int32 value"))
                    def _hoist_variant_map_14(v12):
                        match v12:
                            case hydra.core.LiteralInteger(value=_match_value):
                                return _hoist_variant_map_13(_match_value)

                            case _:
                                return Left(hydra.errors.DecodingError("expected int32 literal"))
                    def _hoist_variant_map_15(v12):
                        match v12:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_14(v)

                            case _:
                                return Left(hydra.errors.DecodingError("expected literal"))
                    return hydra.lib.maps.from_list(((hydra.core.Name("annotatedBody"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepAnnotatedBody())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("applicationFunction"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepApplicationFunction())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("applicationArgument"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepApplicationArgument())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("lambdaBody"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepLambdaBody())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("unionCasesDefault"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepUnionCasesDefault())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("unionCasesBranch"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepUnionCasesBranch(t))), hydra.decode.core.name(cx, input)))), (hydra.core.Name("letBody"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepLetBody())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("letBinding"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepLetBinding(t))), hydra.decode.core.name(cx, input)))), (hydra.core.Name("listElement"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepListElement(t))), hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_variant_map_3(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx, input))))), (hydra.core.Name("mapKey"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepMapKey(t))), hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_variant_map_6(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx, input))))), (hydra.core.Name("mapValue"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepMapValue(t))), hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_variant_map_9(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx, input))))), (hydra.core.Name("maybeTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepMaybeTerm())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("productTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepProductTerm(t))), hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_variant_map_12(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx, input))))), (hydra.core.Name("recordField"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepRecordField(t))), hydra.decode.core.name(cx, input)))), (hydra.core.Name("setElement"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepSetElement(t))), hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_variant_map_15(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx, input))))), (hydra.core.Name("sumTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepSumTerm())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("typeLambdaBody"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepTypeLambdaBody())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("typeApplicationTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepTypeApplicationTerm())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("injectionTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepInjectionTerm())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("wrappedTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepWrappedTerm())), hydra.extract.core.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_paths_subterm_step_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def subterm_path(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_paths_subterm_path_1(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.paths.SubtermPath(b)), hydra.extract.core.decode_list((lambda x1, x2: subterm_step(x1, x2)), cx, wrapped_term.body))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_paths_subterm_path_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def subterm_edge(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_paths_subterm_edge_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("source", (lambda x1, x2: subterm_node(x1, x2)), field_map(), cx), (lambda field_source: hydra.lib.eithers.bind(hydra.extract.core.require_field("path", (lambda x1, x2: subterm_path(x1, x2)), field_map(), cx), (lambda field_path: hydra.lib.eithers.bind(hydra.extract.core.require_field("target", (lambda x1, x2: subterm_node(x1, x2)), field_map(), cx), (lambda field_target: Right(hydra.paths.SubtermEdge(field_source, field_path, field_target))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_paths_subterm_edge_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def subterm_graph(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_paths_subterm_graph_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("nodes", (lambda v12, v2: hydra.extract.core.decode_list((lambda x1, x2: subterm_node(x1, x2)), v12, v2)), field_map(), cx), (lambda field_nodes: hydra.lib.eithers.bind(hydra.extract.core.require_field("edges", (lambda v12, v2: hydra.extract.core.decode_list((lambda x1, x2: subterm_edge(x1, x2)), v12, v2)), field_map(), cx), (lambda field_edges: Right(hydra.paths.SubtermGraph(field_nodes, field_edges))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_paths_subterm_graph_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def subtype_node(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_paths_subtype_node_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                def _hoist_field_map_body_1(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_field_map_body_2(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_1(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_field_map_body_3(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_field_map_body_4(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_3(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.core.require_field("label", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_field_map_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_label: hydra.lib.eithers.bind(hydra.extract.core.require_field("id", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_field_map_body_4(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_id: Right(hydra.paths.SubtypeNode(field_name, field_label, field_id))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_paths_subtype_node_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def subtype_step(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_paths_subtype_step_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.paths.SubtypeStep]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("annotatedBody"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepAnnotatedBody())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("applicationFunction"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepApplicationFunction())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("applicationArgument"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepApplicationArgument())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("eitherLeft"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepEitherLeft())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("eitherRight"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepEitherRight())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("forallBody"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepForallBody())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("functionDomain"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepFunctionDomain())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("functionCodomain"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepFunctionCodomain())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("listElement"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepListElement())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("mapKeys"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepMapKeys())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("mapValues"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepMapValues())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("maybeElement"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepMaybeElement())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("pairFirst"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepPairFirst())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("pairSecond"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepPairSecond())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("recordField"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepRecordField(t))), hydra.decode.core.name(cx, input)))), (hydra.core.Name("setElement"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepSetElement())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("unionField"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepUnionField(t))), hydra.decode.core.name(cx, input)))), (hydra.core.Name("wrappedType"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.paths.SubtypeStep, hydra.paths.SubtypeStepWrappedType())), hydra.extract.core.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_paths_subtype_step_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def subtype_path(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_paths_subtype_path_1(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.paths.SubtypePath(b)), hydra.extract.core.decode_list((lambda x1, x2: subtype_step(x1, x2)), cx, wrapped_term.body))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_paths_subtype_path_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def subtype_edge(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_paths_subtype_edge_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("source", (lambda x1, x2: subtype_node(x1, x2)), field_map(), cx), (lambda field_source: hydra.lib.eithers.bind(hydra.extract.core.require_field("path", (lambda x1, x2: subtype_path(x1, x2)), field_map(), cx), (lambda field_path: hydra.lib.eithers.bind(hydra.extract.core.require_field("target", (lambda x1, x2: subtype_node(x1, x2)), field_map(), cx), (lambda field_target: Right(hydra.paths.SubtypeEdge(field_source, field_path, field_target))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_paths_subtype_edge_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def subtype_graph(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_paths_subtype_graph_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("nodes", (lambda v12, v2: hydra.extract.core.decode_list((lambda x1, x2: subtype_node(x1, x2)), v12, v2)), field_map(), cx), (lambda field_nodes: hydra.lib.eithers.bind(hydra.extract.core.require_field("edges", (lambda v12, v2: hydra.extract.core.decode_list((lambda x1, x2: subtype_edge(x1, x2)), v12, v2)), field_map(), cx), (lambda field_edges: Right(hydra.paths.SubtypeGraph(field_nodes, field_edges))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_paths_subtype_graph_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
