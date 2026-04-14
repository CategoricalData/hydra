# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.packaging."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from typing import TypeVar, cast
import hydra.core
import hydra.encode.core
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.packaging

T0 = TypeVar("T0")

def term_definition(x: hydra.packaging.TermDefinition) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.packaging.TermDefinition"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name)), hydra.core.Field(hydra.core.Name("term"), hydra.encode.core.term(x.term)), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x1: hydra.encode.core.type_scheme(x1)), x.type))))))))

def type_definition(x: hydra.packaging.TypeDefinition) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.packaging.TypeDefinition"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name)), hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.type_scheme(x.type))))))

def definition(v1: hydra.packaging.Definition) -> hydra.core.Term:
    match v1:
        case hydra.packaging.DefinitionTerm(value=y):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.packaging.Definition"), hydra.core.Field(hydra.core.Name("term"), term_definition(y)))))

        case hydra.packaging.DefinitionType(value=y2):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.packaging.Definition"), hydra.core.Field(hydra.core.Name("type"), type_definition(y2)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def file_extension(x: hydra.packaging.FileExtension) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.packaging.FileExtension"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def namespace(x: hydra.packaging.Namespace) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.packaging.Namespace"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def module(x: hydra.packaging.Module) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.packaging.Module"), (hydra.core.Field(hydra.core.Name("namespace"), namespace(x.namespace)), hydra.core.Field(hydra.core.Name("definitions"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: definition(x1)), x.definitions)))), hydra.core.Field(hydra.core.Name("termDependencies"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: namespace(x1)), x.term_dependencies)))), hydra.core.Field(hydra.core.Name("typeDependencies"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: namespace(x1)), x.type_dependencies)))), hydra.core.Field(hydra.core.Name("description"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2))))), x.description))))))))

def namespaces(n: Callable[[T0], hydra.core.Term], x: hydra.packaging.Namespaces[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.packaging.Namespaces"), (hydra.core.Field(hydra.core.Name("focus"), cast(hydra.core.Term, hydra.core.TermPair(hydra.lib.pairs.bimap((lambda x1: namespace(x1)), n, x.focus)))), hydra.core.Field(hydra.core.Name("mapping"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: namespace(x1)), n, x.mapping))))))))

def package_name(x: hydra.packaging.PackageName) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.packaging.PackageName"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def package(x: hydra.packaging.Package) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.packaging.Package"), (hydra.core.Field(hydra.core.Name("name"), package_name(x.name)), hydra.core.Field(hydra.core.Name("modules"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: module(x1)), x.modules)))), hydra.core.Field(hydra.core.Name("dependencies"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: package_name(x1)), x.dependencies)))), hydra.core.Field(hydra.core.Name("description"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2))))), x.description))))))))

def qualified_name(x: hydra.packaging.QualifiedName) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.packaging.QualifiedName"), (hydra.core.Field(hydra.core.Name("namespace"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x1: namespace(x1)), x.namespace)))), hydra.core.Field(hydra.core.Name("local"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.local)))))))))
