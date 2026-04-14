# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.packaging."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.decode.core
import hydra.errors
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.packaging

T0 = TypeVar("T0")

def term_definition(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_packaging_term_definition_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.core.require_field("term", (lambda x1, x2: hydra.decode.core.term(x1, x2)), field_map(), cx), (lambda field_term: hydra.lib.eithers.bind(hydra.extract.core.require_field("type", (lambda v12, v2: hydra.extract.core.decode_maybe((lambda x1, x2: hydra.decode.core.type_scheme(x1, x2)), v12, v2)), field_map(), cx), (lambda field_type: Right(hydra.packaging.TermDefinition(field_name, field_term, field_type))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_packaging_term_definition_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def type_definition(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_packaging_type_definition_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.core.require_field("type", (lambda x1, x2: hydra.decode.core.type_scheme(x1, x2)), field_map(), cx), (lambda field_type: Right(hydra.packaging.TypeDefinition(field_name, field_type))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_packaging_type_definition_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def definition(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_packaging_definition_1(cx, v1):
        match v1:
            case hydra.core.TermInject(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.packaging.Definition]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("term"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.packaging.Definition, hydra.packaging.DefinitionTerm(t))), term_definition(cx, input)))), (hydra.core.Name("type"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.packaging.Definition, hydra.packaging.DefinitionType(t))), type_definition(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_packaging_definition_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def file_extension(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_packaging_file_extension_1(v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)

            case _:
                return Left(hydra.errors.DecodingError("expected string literal"))
    def _hoist_hydra_decode_packaging_file_extension_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_packaging_file_extension_1(v)

            case _:
                return Left(hydra.errors.DecodingError("expected literal"))
    def _hoist_hydra_decode_packaging_file_extension_3(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.packaging.FileExtension(b)), hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_hydra_decode_packaging_file_extension_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx, wrapped_term.body)))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_packaging_file_extension_3(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def namespace(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_packaging_namespace_1(v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)

            case _:
                return Left(hydra.errors.DecodingError("expected string literal"))
    def _hoist_hydra_decode_packaging_namespace_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_packaging_namespace_1(v)

            case _:
                return Left(hydra.errors.DecodingError("expected literal"))
    def _hoist_hydra_decode_packaging_namespace_3(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.packaging.Namespace(b)), hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_hydra_decode_packaging_namespace_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx, wrapped_term.body)))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_packaging_namespace_3(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def module(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_packaging_module_1(cx, v1):
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
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("namespace", (lambda x1, x2: namespace(x1, x2)), field_map(), cx), (lambda field_namespace: hydra.lib.eithers.bind(hydra.extract.core.require_field("definitions", (lambda v12, v2: hydra.extract.core.decode_list((lambda x1, x2: definition(x1, x2)), v12, v2)), field_map(), cx), (lambda field_definitions: hydra.lib.eithers.bind(hydra.extract.core.require_field("termDependencies", (lambda v12, v2: hydra.extract.core.decode_list((lambda x1, x2: namespace(x1, x2)), v12, v2)), field_map(), cx), (lambda field_term_dependencies: hydra.lib.eithers.bind(hydra.extract.core.require_field("typeDependencies", (lambda v12, v2: hydra.extract.core.decode_list((lambda x1, x2: namespace(x1, x2)), v12, v2)), field_map(), cx), (lambda field_type_dependencies: hydra.lib.eithers.bind(hydra.extract.core.require_field("description", (lambda v12, v2: hydra.extract.core.decode_maybe((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), v12, v2)), field_map(), cx), (lambda field_description: Right(hydra.packaging.Module(field_namespace, field_definitions, field_term_dependencies, field_type_dependencies, field_description))))))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_packaging_module_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def namespaces(n: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_packaging_namespaces_1(cx, n, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("focus", (lambda v12, v2: hydra.extract.core.decode_pair((lambda x1, x2: namespace(x1, x2)), n, v12, v2)), field_map(), cx), (lambda field_focus: hydra.lib.eithers.bind(hydra.extract.core.require_field("mapping", (lambda v12, v2: hydra.extract.core.decode_map((lambda x1, x2: namespace(x1, x2)), n, v12, v2)), field_map(), cx), (lambda field_mapping: Right(hydra.packaging.Namespaces(field_focus, field_mapping))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_packaging_namespaces_1(cx, n, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def package_name(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_packaging_package_name_1(v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)

            case _:
                return Left(hydra.errors.DecodingError("expected string literal"))
    def _hoist_hydra_decode_packaging_package_name_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_packaging_package_name_1(v)

            case _:
                return Left(hydra.errors.DecodingError("expected literal"))
    def _hoist_hydra_decode_packaging_package_name_3(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.packaging.PackageName(b)), hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_hydra_decode_packaging_package_name_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx, wrapped_term.body)))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_packaging_package_name_3(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def package(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_packaging_package_1(cx, v1):
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
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: package_name(x1, x2)), field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.core.require_field("modules", (lambda v12, v2: hydra.extract.core.decode_list((lambda x1, x2: module(x1, x2)), v12, v2)), field_map(), cx), (lambda field_modules: hydra.lib.eithers.bind(hydra.extract.core.require_field("dependencies", (lambda v12, v2: hydra.extract.core.decode_list((lambda x1, x2: package_name(x1, x2)), v12, v2)), field_map(), cx), (lambda field_dependencies: hydra.lib.eithers.bind(hydra.extract.core.require_field("description", (lambda v12, v2: hydra.extract.core.decode_maybe((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), v12, v2)), field_map(), cx), (lambda field_description: Right(hydra.packaging.Package(field_name, field_modules, field_dependencies, field_description))))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_packaging_package_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def qualified_name(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_packaging_qualified_name_1(cx, v1):
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
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("namespace", (lambda v12, v2: hydra.extract.core.decode_maybe((lambda x1, x2: namespace(x1, x2)), v12, v2)), field_map(), cx), (lambda field_namespace: hydra.lib.eithers.bind(hydra.extract.core.require_field("local", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), field_map(), cx), (lambda field_local: Right(hydra.packaging.QualifiedName(field_namespace, field_local))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_packaging_qualified_name_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))
