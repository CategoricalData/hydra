# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.module."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import FrozenDict, Maybe, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.encode.core
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.module

T0 = TypeVar("T0")

def term_definition(x: hydra.module.TermDefinition) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.module.TermDefinition"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name)), hydra.core.Field(hydra.core.Name("term"), hydra.encode.core.term(x.term)), hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.type_scheme(x.type))))))

def type_definition(x: hydra.module.TypeDefinition) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.module.TypeDefinition"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name)), hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.type(x.type))))))

def definition(v1: hydra.module.Definition) -> hydra.core.Type:
    match v1:
        case hydra.module.DefinitionTerm(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.module.Definition"), hydra.core.Field(hydra.core.Name("term"), term_definition(v)))))
        
        case hydra.module.DefinitionType(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.module.Definition"), hydra.core.Field(hydra.core.Name("type"), type_definition(v2)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def file_extension(x: hydra.module.FileExtension) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.module.FileExtension"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.value))))

def namespace(x: hydra.module.Namespace) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.module.Namespace"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.value))))

def module(x: hydra.module.Module) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.module.Module"), (hydra.core.Field(hydra.core.Name("namespace"), namespace(x.namespace)), hydra.core.Field(hydra.core.Name("elements"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(hydra.encode.core.binding, xs))))(x.elements)), hydra.core.Field(hydra.core.Name("termDependencies"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(namespace, xs))))(x.term_dependencies)), hydra.core.Field(hydra.core.Name("typeDependencies"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(namespace, xs))))(x.type_dependencies)), hydra.core.Field(hydra.core.Name("description"), (lambda opt: cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2))))), opt))))(x.description))))))

def namespaces(n: Callable[[T0], hydra.core.Term], x: hydra.module.Namespaces[T0]) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.module.Namespaces"), (hydra.core.Field(hydra.core.Name("focus"), (lambda p: cast(hydra.core.Term, hydra.core.TermPair(hydra.lib.pairs.bimap(namespace, n, p))))(x.focus)), hydra.core.Field(hydra.core.Name("mapping"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(namespace, n, m))))(x.mapping))))))

def qualified_name(x: hydra.module.QualifiedName) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.module.QualifiedName"), (hydra.core.Field(hydra.core.Name("namespace"), (lambda opt: cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(namespace, opt))))(x.namespace)), hydra.core.Field(hydra.core.Name("local"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.local))))))
