# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.module."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.decode.core
import hydra.extract.helpers
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.module
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def term_definition(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.module.TermDefinition]:
    def _hoist_hydra_decode_module_term_definition_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.module.TermDefinition]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", hydra.decode.core.name, field_map, cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("term", hydra.decode.core.term, field_map, cx), (lambda field_term: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", hydra.decode.core.type_scheme, field_map, cx), (lambda field_type: cast(Either[hydra.util.DecodingError, hydra.module.TermDefinition], Right(hydra.module.TermDefinition(field_name, field_term, field_type)))))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.module.TermDefinition], Left(hydra.util.DecodingError("expected record of type hydra.module.TermDefinition")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.module.TermDefinition], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_module_term_definition_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_definition(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.module.TypeDefinition]:
    def _hoist_hydra_decode_module_type_definition_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.module.TypeDefinition]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", hydra.decode.core.name, field_map, cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", hydra.decode.core.type, field_map, cx), (lambda field_type: cast(Either[hydra.util.DecodingError, hydra.module.TypeDefinition], Right(hydra.module.TypeDefinition(field_name, field_type)))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.module.TypeDefinition], Left(hydra.util.DecodingError("expected record of type hydra.module.TypeDefinition")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.module.TypeDefinition], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_module_type_definition_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def definition(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.module.Definition]:
    def _hoist_hydra_decode_module_definition_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.module.Definition]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                tname = inj.type_name
                field = inj.field
                fname = field.name
                fterm = field.term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.module.Definition]]]:
                    return cast(FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.module.Definition]]], hydra.lib.maps.from_list((cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.module.Definition]]], (hydra.core.Name("term"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.module.Definition, hydra.module.DefinitionTerm(t))), term_definition(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.module.Definition]]], (hydra.core.Name("type"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.module.Definition, hydra.module.DefinitionType(t))), type_definition(cx, input))))))))
                return hydra.lib.maybes.maybe(cast(Either[hydra.util.DecodingError, hydra.module.Definition], Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union type ", tname.value))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.module.Definition], Left(hydra.util.DecodingError("expected union of type hydra.module.Definition")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.module.Definition], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_module_definition_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def file_extension(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.module.FileExtension]:
    def _hoist_hydra_decode_module_file_extension_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.LiteralString(value=s):
                return cast(Either[hydra.util.DecodingError, str], Right(s))
            
            case _:
                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
    def _hoist_hydra_decode_module_file_extension_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_module_file_extension_1(v)
            
            case _:
                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
    def _hoist_hydra_decode_module_file_extension_3(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.module.FileExtension]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.module.FileExtension(b)), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_hydra_decode_module_file_extension_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, wrapped_term.body))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.module.FileExtension], Left(hydra.util.DecodingError("expected wrapped type hydra.module.FileExtension")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.module.FileExtension], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_module_file_extension_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def namespace(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.module.Namespace]:
    def _hoist_hydra_decode_module_namespace_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.LiteralString(value=s):
                return cast(Either[hydra.util.DecodingError, str], Right(s))
            
            case _:
                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
    def _hoist_hydra_decode_module_namespace_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_module_namespace_1(v)
            
            case _:
                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
    def _hoist_hydra_decode_module_namespace_3(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.module.Namespace]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.module.Namespace(b)), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_hydra_decode_module_namespace_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, wrapped_term.body))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.module.Namespace], Left(hydra.util.DecodingError("expected wrapped type hydra.module.Namespace")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.module.Namespace], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_module_namespace_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def module(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.module.Module]:
    def _hoist_hydra_decode_module_module_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.module.Module]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.LiteralString(value=s):
                            return cast(Either[hydra.util.DecodingError, str], Right(s))
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
                def _hoist_body_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("namespace", namespace, field_map, cx), (lambda field_namespace: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("elements", (lambda v1, v2: hydra.extract.helpers.decode_list(hydra.decode.core.binding, v1, v2)), field_map, cx), (lambda field_elements: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("termDependencies", (lambda v1, v2: hydra.extract.helpers.decode_list(namespace, v1, v2)), field_map, cx), (lambda field_term_dependencies: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("typeDependencies", (lambda v1, v2: hydra.extract.helpers.decode_list(namespace, v1, v2)), field_map, cx), (lambda field_type_dependencies: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("description", (lambda v1, v2: hydra.extract.helpers.decode_maybe((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v1, v2)), field_map, cx), (lambda field_description: cast(Either[hydra.util.DecodingError, hydra.module.Module], Right(hydra.module.Module(field_namespace, field_elements, field_term_dependencies, field_type_dependencies, field_description)))))))))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.module.Module], Left(hydra.util.DecodingError("expected record of type hydra.module.Module")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.module.Module], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_module_module_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def namespaces(n: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.module.Namespaces[T0]]:
    def _hoist_hydra_decode_module_namespaces_1(cx: hydra.graph.Graph, n: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.module.Namespaces[T1]]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("focus", (lambda v1, v2: hydra.extract.helpers.decode_pair(namespace, n, v1, v2)), field_map, cx), (lambda field_focus: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("mapping", (lambda v1, v2: hydra.extract.helpers.decode_map(namespace, n, v1, v2)), field_map, cx), (lambda field_mapping: cast(Either[hydra.util.DecodingError, hydra.module.Namespaces[T1]], Right(cast(hydra.module.Namespaces[T1], hydra.module.Namespaces(field_focus, field_mapping))))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.module.Namespaces[T1]], Left(hydra.util.DecodingError("expected record of type hydra.module.Namespaces")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.module.Namespaces[T0]], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_module_namespaces_1(cx, n, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def qualified_name(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.module.QualifiedName]:
    def _hoist_hydra_decode_module_qualified_name_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.module.QualifiedName]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.LiteralString(value=s):
                            return cast(Either[hydra.util.DecodingError, str], Right(s))
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
                def _hoist_body_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("namespace", (lambda v1, v2: hydra.extract.helpers.decode_maybe(namespace, v1, v2)), field_map, cx), (lambda field_namespace: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("local", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map, cx), (lambda field_local: cast(Either[hydra.util.DecodingError, hydra.module.QualifiedName], Right(hydra.module.QualifiedName(field_namespace, field_local)))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.module.QualifiedName], Left(hydra.util.DecodingError("expected record of type hydra.module.QualifiedName")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.module.QualifiedName], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_module_qualified_name_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
