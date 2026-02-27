# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.module."""

from __future__ import annotations
from collections.abc import Callable
from typing import TypeVar, cast
import hydra.core
import hydra.encode.core
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.module

T0 = TypeVar("T0")

def term_definition(x: hydra.module.TermDefinition) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.module.TermDefinition"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name)), hydra.core.Field(hydra.core.Name("term"), hydra.encode.core.term(x.term)), hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.type_scheme(x.type))))))

def type_definition(x: hydra.module.TypeDefinition) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.module.TypeDefinition"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name)), hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.type(x.type))))))

def definition(v1: hydra.module.Definition) -> hydra.core.Term:
    match v1:
        case hydra.module.DefinitionTerm(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.module.Definition"), hydra.core.Field(hydra.core.Name("term"), term_definition(y)))))
        
        case hydra.module.DefinitionType(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.module.Definition"), hydra.core.Field(hydra.core.Name("type"), type_definition(y2)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def file_extension(x: hydra.module.FileExtension) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.module.FileExtension"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def namespace(x: hydra.module.Namespace) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.module.Namespace"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def module(x: hydra.module.Module) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.module.Module"), (hydra.core.Field(hydra.core.Name("namespace"), namespace(x.namespace)), hydra.core.Field(hydra.core.Name("elements"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: hydra.encode.core.binding(x1)), x.elements)))), hydra.core.Field(hydra.core.Name("termDependencies"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: namespace(x1)), x.term_dependencies)))), hydra.core.Field(hydra.core.Name("typeDependencies"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: namespace(x1)), x.type_dependencies)))), hydra.core.Field(hydra.core.Name("description"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2))))), x.description))))))))

def namespaces(n: Callable[[T0], hydra.core.Term], x: hydra.module.Namespaces[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.module.Namespaces"), (hydra.core.Field(hydra.core.Name("focus"), cast(hydra.core.Term, hydra.core.TermPair(hydra.lib.pairs.bimap((lambda x1: namespace(x1)), n, x.focus)))), hydra.core.Field(hydra.core.Name("mapping"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: namespace(x1)), n, x.mapping))))))))

def qualified_name(x: hydra.module.QualifiedName) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.module.QualifiedName"), (hydra.core.Field(hydra.core.Name("namespace"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x1: namespace(x1)), x.namespace)))), hydra.core.Field(hydra.core.Name("local"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.local)))))))))
