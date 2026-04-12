# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.testing."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Left, Maybe, Right, frozenlist
from typing import cast
import hydra.core
import hydra.errors
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.testing

def tag(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_testing_tag_1(v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)

            case _:
                return Left(hydra.errors.DecodingError("expected string literal"))
    def _hoist_hydra_decode_testing_tag_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_testing_tag_1(v)

            case _:
                return Left(hydra.errors.DecodingError("expected literal"))
    def _hoist_hydra_decode_testing_tag_3(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.testing.Tag(b)), hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_hydra_decode_testing_tag_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx, wrapped_term.body)))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_testing_tag_3(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def universal_test_case(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_testing_universal_test_case_1(cx, v1):
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
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("actual", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), field_map(), cx), (lambda field_actual: hydra.lib.eithers.bind(hydra.extract.core.require_field("expected", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_4(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), field_map(), cx), (lambda field_expected: Right(hydra.testing.UniversalTestCase(field_actual, field_expected))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_testing_universal_test_case_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def test_case(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_testing_test_case_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.testing.TestCase]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("universal"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.testing.TestCase, hydra.testing.TestCaseUniversal(t))), universal_test_case(cx, input)))),))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_testing_test_case_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def test_case_with_metadata(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_testing_test_case_with_metadata_1(cx, v1):
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
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.core.require_field("case", (lambda x1, x2: test_case(x1, x2)), field_map(), cx), (lambda field_case: hydra.lib.eithers.bind(hydra.extract.core.require_field("description", (lambda v12, v2: hydra.extract.core.decode_maybe((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_4(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), v12, v2)), field_map(), cx), (lambda field_description: hydra.lib.eithers.bind(hydra.extract.core.require_field("tags", (lambda v12, v2: hydra.extract.core.decode_list((lambda x1, x2: tag(x1, x2)), v12, v2)), field_map(), cx), (lambda field_tags: Right(hydra.testing.TestCaseWithMetadata(field_name, field_case, field_description, field_tags))))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_testing_test_case_with_metadata_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def test_group(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_testing_test_group_1(cx, v1):
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
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.core.require_field("description", (lambda v12, v2: hydra.extract.core.decode_maybe((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_4(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), v12, v2)), field_map(), cx), (lambda field_description: hydra.lib.eithers.bind(hydra.extract.core.require_field("subgroups", (lambda v12, v2: hydra.extract.core.decode_list((lambda x1, x2: test_group(x1, x2)), v12, v2)), field_map(), cx), (lambda field_subgroups: hydra.lib.eithers.bind(hydra.extract.core.require_field("cases", (lambda v12, v2: hydra.extract.core.decode_list((lambda x1, x2: test_case_with_metadata(x1, x2)), v12, v2)), field_map(), cx), (lambda field_cases: Right(hydra.testing.TestGroup(field_name, field_description, field_subgroups, field_cases))))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_testing_test_group_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))
