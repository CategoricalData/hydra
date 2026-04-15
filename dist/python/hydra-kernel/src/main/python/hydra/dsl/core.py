# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.core."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Maybe, frozenlist
from typing import cast
import hydra.core
import hydra.phantoms

def annotated_term(body: hydra.phantoms.TTerm[hydra.core.Term], annotation: hydra.phantoms.TTerm[FrozenDict[hydra.core.Name, hydra.core.Term]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.AnnotatedTerm"), (hydra.core.Field(hydra.core.Name("body"), body.value), hydra.core.Field(hydra.core.Name("annotation"), annotation.value))))))

def annotated_term_annotation(x: hydra.phantoms.TTerm[hydra.core.AnnotatedTerm]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.AnnotatedTerm"), hydra.core.Name("annotation")))), x.value))))

def annotated_term_body(x: hydra.phantoms.TTerm[hydra.core.AnnotatedTerm]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.AnnotatedTerm"), hydra.core.Name("body")))), x.value))))

def annotated_term_with_annotation(original: hydra.phantoms.TTerm[hydra.core.AnnotatedTerm], new_val: hydra.phantoms.TTerm[FrozenDict[hydra.core.Name, hydra.core.Term]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.AnnotatedTerm"), (hydra.core.Field(hydra.core.Name("body"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.AnnotatedTerm"), hydra.core.Name("body")))), original.value)))), hydra.core.Field(hydra.core.Name("annotation"), new_val.value))))))

def annotated_term_with_body(original: hydra.phantoms.TTerm[hydra.core.AnnotatedTerm], new_val: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.AnnotatedTerm"), (hydra.core.Field(hydra.core.Name("body"), new_val.value), hydra.core.Field(hydra.core.Name("annotation"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.AnnotatedTerm"), hydra.core.Name("annotation")))), original.value)))))))))

def annotated_type(body: hydra.phantoms.TTerm[hydra.core.Type], annotation: hydra.phantoms.TTerm[FrozenDict[hydra.core.Name, hydra.core.Term]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.AnnotatedType"), (hydra.core.Field(hydra.core.Name("body"), body.value), hydra.core.Field(hydra.core.Name("annotation"), annotation.value))))))

def annotated_type_annotation(x: hydra.phantoms.TTerm[hydra.core.AnnotatedType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.AnnotatedType"), hydra.core.Name("annotation")))), x.value))))

def annotated_type_body(x: hydra.phantoms.TTerm[hydra.core.AnnotatedType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.AnnotatedType"), hydra.core.Name("body")))), x.value))))

def annotated_type_with_annotation(original: hydra.phantoms.TTerm[hydra.core.AnnotatedType], new_val: hydra.phantoms.TTerm[FrozenDict[hydra.core.Name, hydra.core.Term]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.AnnotatedType"), (hydra.core.Field(hydra.core.Name("body"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.AnnotatedType"), hydra.core.Name("body")))), original.value)))), hydra.core.Field(hydra.core.Name("annotation"), new_val.value))))))

def annotated_type_with_body(original: hydra.phantoms.TTerm[hydra.core.AnnotatedType], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.AnnotatedType"), (hydra.core.Field(hydra.core.Name("body"), new_val.value), hydra.core.Field(hydra.core.Name("annotation"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.AnnotatedType"), hydra.core.Name("annotation")))), original.value)))))))))

def application(function: hydra.phantoms.TTerm[hydra.core.Term], argument: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Application"), (hydra.core.Field(hydra.core.Name("function"), function.value), hydra.core.Field(hydra.core.Name("argument"), argument.value))))))

def application_argument(x: hydra.phantoms.TTerm[hydra.core.Application]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Application"), hydra.core.Name("argument")))), x.value))))

def application_function(x: hydra.phantoms.TTerm[hydra.core.Application]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Application"), hydra.core.Name("function")))), x.value))))

def application_type(function: hydra.phantoms.TTerm[hydra.core.Type], argument: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ApplicationType"), (hydra.core.Field(hydra.core.Name("function"), function.value), hydra.core.Field(hydra.core.Name("argument"), argument.value))))))

def application_type_argument(x: hydra.phantoms.TTerm[hydra.core.ApplicationType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.ApplicationType"), hydra.core.Name("argument")))), x.value))))

def application_type_function(x: hydra.phantoms.TTerm[hydra.core.ApplicationType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.ApplicationType"), hydra.core.Name("function")))), x.value))))

def application_type_with_argument(original: hydra.phantoms.TTerm[hydra.core.ApplicationType], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ApplicationType"), (hydra.core.Field(hydra.core.Name("function"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.ApplicationType"), hydra.core.Name("function")))), original.value)))), hydra.core.Field(hydra.core.Name("argument"), new_val.value))))))

def application_type_with_function(original: hydra.phantoms.TTerm[hydra.core.ApplicationType], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ApplicationType"), (hydra.core.Field(hydra.core.Name("function"), new_val.value), hydra.core.Field(hydra.core.Name("argument"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.ApplicationType"), hydra.core.Name("argument")))), original.value)))))))))

def application_with_argument(original: hydra.phantoms.TTerm[hydra.core.Application], new_val: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Application"), (hydra.core.Field(hydra.core.Name("function"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Application"), hydra.core.Name("function")))), original.value)))), hydra.core.Field(hydra.core.Name("argument"), new_val.value))))))

def application_with_function(original: hydra.phantoms.TTerm[hydra.core.Application], new_val: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Application"), (hydra.core.Field(hydra.core.Name("function"), new_val.value), hydra.core.Field(hydra.core.Name("argument"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Application"), hydra.core.Name("argument")))), original.value)))))))))

def binding(name: hydra.phantoms.TTerm[hydra.core.Name], term: hydra.phantoms.TTerm[hydra.core.Term], type: hydra.phantoms.TTerm[Maybe[hydra.core.TypeScheme]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Binding"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("term"), term.value), hydra.core.Field(hydra.core.Name("type"), type.value))))))

def binding_name(x: hydra.phantoms.TTerm[hydra.core.Binding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Binding"), hydra.core.Name("name")))), x.value))))

def binding_term(x: hydra.phantoms.TTerm[hydra.core.Binding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Binding"), hydra.core.Name("term")))), x.value))))

def binding_type(x: hydra.phantoms.TTerm[hydra.core.Binding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Binding"), hydra.core.Name("type")))), x.value))))

def binding_with_name(original: hydra.phantoms.TTerm[hydra.core.Binding], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Binding"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("term"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Binding"), hydra.core.Name("term")))), original.value)))), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Binding"), hydra.core.Name("type")))), original.value)))))))))

def binding_with_term(original: hydra.phantoms.TTerm[hydra.core.Binding], new_val: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Binding"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Binding"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("term"), new_val.value), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Binding"), hydra.core.Name("type")))), original.value)))))))))

def binding_with_type(original: hydra.phantoms.TTerm[hydra.core.Binding], new_val: hydra.phantoms.TTerm[Maybe[hydra.core.TypeScheme]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Binding"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Binding"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("term"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Binding"), hydra.core.Name("term")))), original.value)))), hydra.core.Field(hydra.core.Name("type"), new_val.value))))))

def case_statement(type_name: hydra.phantoms.TTerm[hydra.core.Name], default: hydra.phantoms.TTerm[Maybe[hydra.core.Term]], cases: hydra.phantoms.TTerm[frozenlist[hydra.core.Field]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.CaseStatement"), (hydra.core.Field(hydra.core.Name("typeName"), type_name.value), hydra.core.Field(hydra.core.Name("default"), default.value), hydra.core.Field(hydra.core.Name("cases"), cases.value))))))

def case_statement_cases(x: hydra.phantoms.TTerm[hydra.core.CaseStatement]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.CaseStatement"), hydra.core.Name("cases")))), x.value))))

def case_statement_default(x: hydra.phantoms.TTerm[hydra.core.CaseStatement]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.CaseStatement"), hydra.core.Name("default")))), x.value))))

def case_statement_type_name(x: hydra.phantoms.TTerm[hydra.core.CaseStatement]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.CaseStatement"), hydra.core.Name("typeName")))), x.value))))

def case_statement_with_cases(original: hydra.phantoms.TTerm[hydra.core.CaseStatement], new_val: hydra.phantoms.TTerm[frozenlist[hydra.core.Field]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.CaseStatement"), (hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.CaseStatement"), hydra.core.Name("typeName")))), original.value)))), hydra.core.Field(hydra.core.Name("default"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.CaseStatement"), hydra.core.Name("default")))), original.value)))), hydra.core.Field(hydra.core.Name("cases"), new_val.value))))))

def case_statement_with_default(original: hydra.phantoms.TTerm[hydra.core.CaseStatement], new_val: hydra.phantoms.TTerm[Maybe[hydra.core.Term]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.CaseStatement"), (hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.CaseStatement"), hydra.core.Name("typeName")))), original.value)))), hydra.core.Field(hydra.core.Name("default"), new_val.value), hydra.core.Field(hydra.core.Name("cases"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.CaseStatement"), hydra.core.Name("cases")))), original.value)))))))))

def case_statement_with_type_name(original: hydra.phantoms.TTerm[hydra.core.CaseStatement], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.CaseStatement"), (hydra.core.Field(hydra.core.Name("typeName"), new_val.value), hydra.core.Field(hydra.core.Name("default"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.CaseStatement"), hydra.core.Name("default")))), original.value)))), hydra.core.Field(hydra.core.Name("cases"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.CaseStatement"), hydra.core.Name("cases")))), original.value)))))))))

def either_type(left: hydra.phantoms.TTerm[hydra.core.Type], right: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.EitherType"), (hydra.core.Field(hydra.core.Name("left"), left.value), hydra.core.Field(hydra.core.Name("right"), right.value))))))

def either_type_left(x: hydra.phantoms.TTerm[hydra.core.EitherType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.EitherType"), hydra.core.Name("left")))), x.value))))

def either_type_right(x: hydra.phantoms.TTerm[hydra.core.EitherType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.EitherType"), hydra.core.Name("right")))), x.value))))

def either_type_with_left(original: hydra.phantoms.TTerm[hydra.core.EitherType], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.EitherType"), (hydra.core.Field(hydra.core.Name("left"), new_val.value), hydra.core.Field(hydra.core.Name("right"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.EitherType"), hydra.core.Name("right")))), original.value)))))))))

def either_type_with_right(original: hydra.phantoms.TTerm[hydra.core.EitherType], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.EitherType"), (hydra.core.Field(hydra.core.Name("left"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.EitherType"), hydra.core.Name("left")))), original.value)))), hydra.core.Field(hydra.core.Name("right"), new_val.value))))))

def field(name: hydra.phantoms.TTerm[hydra.core.Name], term: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Field"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("term"), term.value))))))

def field_name(x: hydra.phantoms.TTerm[hydra.core.Field]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Field"), hydra.core.Name("name")))), x.value))))

def field_term(x: hydra.phantoms.TTerm[hydra.core.Field]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Field"), hydra.core.Name("term")))), x.value))))

def field_type(name: hydra.phantoms.TTerm[hydra.core.Name], type: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FieldType"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("type"), type.value))))))

def field_type_name(x: hydra.phantoms.TTerm[hydra.core.FieldType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.FieldType"), hydra.core.Name("name")))), x.value))))

def field_type_type(x: hydra.phantoms.TTerm[hydra.core.FieldType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.FieldType"), hydra.core.Name("type")))), x.value))))

def field_type_with_name(original: hydra.phantoms.TTerm[hydra.core.FieldType], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FieldType"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.FieldType"), hydra.core.Name("type")))), original.value)))))))))

def field_type_with_type(original: hydra.phantoms.TTerm[hydra.core.FieldType], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FieldType"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.FieldType"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("type"), new_val.value))))))

def field_with_name(original: hydra.phantoms.TTerm[hydra.core.Field], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Field"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("term"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Field"), hydra.core.Name("term")))), original.value)))))))))

def field_with_term(original: hydra.phantoms.TTerm[hydra.core.Field], new_val: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Field"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Field"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("term"), new_val.value))))))

float_type_bigfloat = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("bigfloat"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

float_type_float32 = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("float32"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

float_type_float64 = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("float64"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def float_value_bigfloat(x: hydra.phantoms.TTerm[Decimal]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("bigfloat"), x.value)))))

def float_value_float32(x: hydra.phantoms.TTerm[float]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("float32"), x.value)))))

def float_value_float64(x: hydra.phantoms.TTerm[float]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("float64"), x.value)))))

def forall_type(parameter: hydra.phantoms.TTerm[hydra.core.Name], body: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ForallType"), (hydra.core.Field(hydra.core.Name("parameter"), parameter.value), hydra.core.Field(hydra.core.Name("body"), body.value))))))

def forall_type_body(x: hydra.phantoms.TTerm[hydra.core.ForallType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.ForallType"), hydra.core.Name("body")))), x.value))))

def forall_type_parameter(x: hydra.phantoms.TTerm[hydra.core.ForallType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.ForallType"), hydra.core.Name("parameter")))), x.value))))

def forall_type_with_body(original: hydra.phantoms.TTerm[hydra.core.ForallType], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ForallType"), (hydra.core.Field(hydra.core.Name("parameter"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.ForallType"), hydra.core.Name("parameter")))), original.value)))), hydra.core.Field(hydra.core.Name("body"), new_val.value))))))

def forall_type_with_parameter(original: hydra.phantoms.TTerm[hydra.core.ForallType], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ForallType"), (hydra.core.Field(hydra.core.Name("parameter"), new_val.value), hydra.core.Field(hydra.core.Name("body"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.ForallType"), hydra.core.Name("body")))), original.value)))))))))

def function_type(domain: hydra.phantoms.TTerm[hydra.core.Type], codomain: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FunctionType"), (hydra.core.Field(hydra.core.Name("domain"), domain.value), hydra.core.Field(hydra.core.Name("codomain"), codomain.value))))))

def function_type_codomain(x: hydra.phantoms.TTerm[hydra.core.FunctionType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.FunctionType"), hydra.core.Name("codomain")))), x.value))))

def function_type_domain(x: hydra.phantoms.TTerm[hydra.core.FunctionType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.FunctionType"), hydra.core.Name("domain")))), x.value))))

def function_type_with_codomain(original: hydra.phantoms.TTerm[hydra.core.FunctionType], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FunctionType"), (hydra.core.Field(hydra.core.Name("domain"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.FunctionType"), hydra.core.Name("domain")))), original.value)))), hydra.core.Field(hydra.core.Name("codomain"), new_val.value))))))

def function_type_with_domain(original: hydra.phantoms.TTerm[hydra.core.FunctionType], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FunctionType"), (hydra.core.Field(hydra.core.Name("domain"), new_val.value), hydra.core.Field(hydra.core.Name("codomain"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.FunctionType"), hydra.core.Name("codomain")))), original.value)))))))))

def injection(type_name: hydra.phantoms.TTerm[hydra.core.Name], field: hydra.phantoms.TTerm[hydra.core.Field]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Injection"), (hydra.core.Field(hydra.core.Name("typeName"), type_name.value), hydra.core.Field(hydra.core.Name("field"), field.value))))))

def injection_field(x: hydra.phantoms.TTerm[hydra.core.Injection]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Injection"), hydra.core.Name("field")))), x.value))))

def injection_type_name(x: hydra.phantoms.TTerm[hydra.core.Injection]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Injection"), hydra.core.Name("typeName")))), x.value))))

def injection_with_field(original: hydra.phantoms.TTerm[hydra.core.Injection], new_val: hydra.phantoms.TTerm[hydra.core.Field]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Injection"), (hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Injection"), hydra.core.Name("typeName")))), original.value)))), hydra.core.Field(hydra.core.Name("field"), new_val.value))))))

def injection_with_type_name(original: hydra.phantoms.TTerm[hydra.core.Injection], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Injection"), (hydra.core.Field(hydra.core.Name("typeName"), new_val.value), hydra.core.Field(hydra.core.Name("field"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Injection"), hydra.core.Name("field")))), original.value)))))))))

integer_type_bigint = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("bigint"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

integer_type_int16 = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int16"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

integer_type_int32 = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int32"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

integer_type_int64 = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int64"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

integer_type_int8 = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int8"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

integer_type_uint16 = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint16"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

integer_type_uint32 = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint32"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

integer_type_uint64 = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint64"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

integer_type_uint8 = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint8"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def integer_value_bigint(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("bigint"), x.value)))))

def integer_value_int16(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int16"), x.value)))))

def integer_value_int32(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int32"), x.value)))))

def integer_value_int64(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int64"), x.value)))))

def integer_value_int8(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int8"), x.value)))))

def integer_value_uint16(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint16"), x.value)))))

def integer_value_uint32(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint32"), x.value)))))

def integer_value_uint64(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint64"), x.value)))))

def integer_value_uint8(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint8"), x.value)))))

def lambda_(parameter: hydra.phantoms.TTerm[hydra.core.Name], domain: hydra.phantoms.TTerm[Maybe[hydra.core.Type]], body: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Lambda"), (hydra.core.Field(hydra.core.Name("parameter"), parameter.value), hydra.core.Field(hydra.core.Name("domain"), domain.value), hydra.core.Field(hydra.core.Name("body"), body.value))))))

def lambda_body(x: hydra.phantoms.TTerm[hydra.core.Lambda]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Lambda"), hydra.core.Name("body")))), x.value))))

def lambda_domain(x: hydra.phantoms.TTerm[hydra.core.Lambda]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Lambda"), hydra.core.Name("domain")))), x.value))))

def lambda_parameter(x: hydra.phantoms.TTerm[hydra.core.Lambda]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Lambda"), hydra.core.Name("parameter")))), x.value))))

def lambda_with_body(original: hydra.phantoms.TTerm[hydra.core.Lambda], new_val: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Lambda"), (hydra.core.Field(hydra.core.Name("parameter"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Lambda"), hydra.core.Name("parameter")))), original.value)))), hydra.core.Field(hydra.core.Name("domain"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Lambda"), hydra.core.Name("domain")))), original.value)))), hydra.core.Field(hydra.core.Name("body"), new_val.value))))))

def lambda_with_domain(original: hydra.phantoms.TTerm[hydra.core.Lambda], new_val: hydra.phantoms.TTerm[Maybe[hydra.core.Type]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Lambda"), (hydra.core.Field(hydra.core.Name("parameter"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Lambda"), hydra.core.Name("parameter")))), original.value)))), hydra.core.Field(hydra.core.Name("domain"), new_val.value), hydra.core.Field(hydra.core.Name("body"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Lambda"), hydra.core.Name("body")))), original.value)))))))))

def lambda_with_parameter(original: hydra.phantoms.TTerm[hydra.core.Lambda], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Lambda"), (hydra.core.Field(hydra.core.Name("parameter"), new_val.value), hydra.core.Field(hydra.core.Name("domain"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Lambda"), hydra.core.Name("domain")))), original.value)))), hydra.core.Field(hydra.core.Name("body"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Lambda"), hydra.core.Name("body")))), original.value)))))))))

def let(bindings: hydra.phantoms.TTerm[frozenlist[hydra.core.Binding]], body: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Let"), (hydra.core.Field(hydra.core.Name("bindings"), bindings.value), hydra.core.Field(hydra.core.Name("body"), body.value))))))

def let_bindings(x: hydra.phantoms.TTerm[hydra.core.Let]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Let"), hydra.core.Name("bindings")))), x.value))))

def let_body(x: hydra.phantoms.TTerm[hydra.core.Let]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Let"), hydra.core.Name("body")))), x.value))))

def let_with_bindings(original: hydra.phantoms.TTerm[hydra.core.Let], new_val: hydra.phantoms.TTerm[frozenlist[hydra.core.Binding]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Let"), (hydra.core.Field(hydra.core.Name("bindings"), new_val.value), hydra.core.Field(hydra.core.Name("body"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Let"), hydra.core.Name("body")))), original.value)))))))))

def let_with_body(original: hydra.phantoms.TTerm[hydra.core.Let], new_val: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Let"), (hydra.core.Field(hydra.core.Name("bindings"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Let"), hydra.core.Name("bindings")))), original.value)))), hydra.core.Field(hydra.core.Name("body"), new_val.value))))))

def literal_binary(x: hydra.phantoms.TTerm[bytes]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("binary"), x.value)))))

def literal_boolean(x: hydra.phantoms.TTerm[bool]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("boolean"), x.value)))))

def literal_decimal(x: hydra.phantoms.TTerm[Decimal]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("decimal"), x.value)))))

def literal_float(x: hydra.phantoms.TTerm[hydra.core.FloatValue]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("float"), x.value)))))

def literal_integer(x: hydra.phantoms.TTerm[hydra.core.IntegerValue]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("integer"), x.value)))))

def literal_string(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("string"), x.value)))))

literal_type_binary = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("binary"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

literal_type_boolean = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("boolean"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

literal_type_decimal = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("decimal"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def literal_type_float(x: hydra.phantoms.TTerm[hydra.core.FloatType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("float"), x.value)))))

def literal_type_integer(x: hydra.phantoms.TTerm[hydra.core.IntegerType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("integer"), x.value)))))

literal_type_string = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("string"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def map_type(keys: hydra.phantoms.TTerm[hydra.core.Type], values: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.MapType"), (hydra.core.Field(hydra.core.Name("keys"), keys.value), hydra.core.Field(hydra.core.Name("values"), values.value))))))

def map_type_keys(x: hydra.phantoms.TTerm[hydra.core.MapType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.MapType"), hydra.core.Name("keys")))), x.value))))

def map_type_values(x: hydra.phantoms.TTerm[hydra.core.MapType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.MapType"), hydra.core.Name("values")))), x.value))))

def map_type_with_keys(original: hydra.phantoms.TTerm[hydra.core.MapType], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.MapType"), (hydra.core.Field(hydra.core.Name("keys"), new_val.value), hydra.core.Field(hydra.core.Name("values"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.MapType"), hydra.core.Name("values")))), original.value)))))))))

def map_type_with_values(original: hydra.phantoms.TTerm[hydra.core.MapType], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.MapType"), (hydra.core.Field(hydra.core.Name("keys"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.MapType"), hydra.core.Name("keys")))), original.value)))), hydra.core.Field(hydra.core.Name("values"), new_val.value))))))

def name(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), x.value))))

def pair_type(first: hydra.phantoms.TTerm[hydra.core.Type], second: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.PairType"), (hydra.core.Field(hydra.core.Name("first"), first.value), hydra.core.Field(hydra.core.Name("second"), second.value))))))

def pair_type_first(x: hydra.phantoms.TTerm[hydra.core.PairType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.PairType"), hydra.core.Name("first")))), x.value))))

def pair_type_second(x: hydra.phantoms.TTerm[hydra.core.PairType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.PairType"), hydra.core.Name("second")))), x.value))))

def pair_type_with_first(original: hydra.phantoms.TTerm[hydra.core.PairType], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.PairType"), (hydra.core.Field(hydra.core.Name("first"), new_val.value), hydra.core.Field(hydra.core.Name("second"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.PairType"), hydra.core.Name("second")))), original.value)))))))))

def pair_type_with_second(original: hydra.phantoms.TTerm[hydra.core.PairType], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.PairType"), (hydra.core.Field(hydra.core.Name("first"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.PairType"), hydra.core.Name("first")))), original.value)))), hydra.core.Field(hydra.core.Name("second"), new_val.value))))))

def projection(type_name: hydra.phantoms.TTerm[hydra.core.Name], field: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Projection"), (hydra.core.Field(hydra.core.Name("typeName"), type_name.value), hydra.core.Field(hydra.core.Name("field"), field.value))))))

def projection_field(x: hydra.phantoms.TTerm[hydra.core.Projection]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Projection"), hydra.core.Name("field")))), x.value))))

def projection_type_name(x: hydra.phantoms.TTerm[hydra.core.Projection]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Projection"), hydra.core.Name("typeName")))), x.value))))

def projection_with_field(original: hydra.phantoms.TTerm[hydra.core.Projection], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Projection"), (hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Projection"), hydra.core.Name("typeName")))), original.value)))), hydra.core.Field(hydra.core.Name("field"), new_val.value))))))

def projection_with_type_name(original: hydra.phantoms.TTerm[hydra.core.Projection], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Projection"), (hydra.core.Field(hydra.core.Name("typeName"), new_val.value), hydra.core.Field(hydra.core.Name("field"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Projection"), hydra.core.Name("field")))), original.value)))))))))

def record(type_name: hydra.phantoms.TTerm[hydra.core.Name], fields: hydra.phantoms.TTerm[frozenlist[hydra.core.Field]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Record"), (hydra.core.Field(hydra.core.Name("typeName"), type_name.value), hydra.core.Field(hydra.core.Name("fields"), fields.value))))))

def record_fields(x: hydra.phantoms.TTerm[hydra.core.Record]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Record"), hydra.core.Name("fields")))), x.value))))

def record_type_name(x: hydra.phantoms.TTerm[hydra.core.Record]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Record"), hydra.core.Name("typeName")))), x.value))))

def record_with_fields(original: hydra.phantoms.TTerm[hydra.core.Record], new_val: hydra.phantoms.TTerm[frozenlist[hydra.core.Field]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Record"), (hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Record"), hydra.core.Name("typeName")))), original.value)))), hydra.core.Field(hydra.core.Name("fields"), new_val.value))))))

def record_with_type_name(original: hydra.phantoms.TTerm[hydra.core.Record], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Record"), (hydra.core.Field(hydra.core.Name("typeName"), new_val.value), hydra.core.Field(hydra.core.Name("fields"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.Record"), hydra.core.Name("fields")))), original.value)))))))))

def term_annotated(x: hydra.phantoms.TTerm[hydra.core.AnnotatedTerm]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("annotated"), x.value)))))

def term_application(x: hydra.phantoms.TTerm[hydra.core.Application]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("application"), x.value)))))

def term_cases(x: hydra.phantoms.TTerm[hydra.core.CaseStatement]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("cases"), x.value)))))

def term_either(x: hydra.phantoms.TTerm[Either[hydra.core.Term, hydra.core.Term]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("either"), x.value)))))

def term_inject(x: hydra.phantoms.TTerm[hydra.core.Injection]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("inject"), x.value)))))

def term_lambda(x: hydra.phantoms.TTerm[hydra.core.Lambda]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("lambda"), x.value)))))

def term_let(x: hydra.phantoms.TTerm[hydra.core.Let]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("let"), x.value)))))

def term_list(x: hydra.phantoms.TTerm[frozenlist[hydra.core.Term]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("list"), x.value)))))

def term_literal(x: hydra.phantoms.TTerm[hydra.core.Literal]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("literal"), x.value)))))

def term_map(x: hydra.phantoms.TTerm[FrozenDict[hydra.core.Term, hydra.core.Term]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("map"), x.value)))))

def term_maybe(x: hydra.phantoms.TTerm[Maybe[hydra.core.Term]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("maybe"), x.value)))))

def term_pair(x: hydra.phantoms.TTerm[tuple[hydra.core.Term, hydra.core.Term]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("pair"), x.value)))))

def term_project(x: hydra.phantoms.TTerm[hydra.core.Projection]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("project"), x.value)))))

def term_record(x: hydra.phantoms.TTerm[hydra.core.Record]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("record"), x.value)))))

def term_set(x: hydra.phantoms.TTerm[frozenset[hydra.core.Term]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("set"), x.value)))))

def term_type_application(x: hydra.phantoms.TTerm[hydra.core.TypeApplicationTerm]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typeApplication"), x.value)))))

def term_type_lambda(x: hydra.phantoms.TTerm[hydra.core.TypeLambda]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typeLambda"), x.value)))))

term_unit = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("unit"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def term_unwrap(x: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("unwrap"), x.value)))))

def term_variable(x: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("variable"), x.value)))))

def term_wrap(x: hydra.phantoms.TTerm[hydra.core.WrappedTerm]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("wrap"), x.value)))))

def type_annotated(x: hydra.phantoms.TTerm[hydra.core.AnnotatedType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("annotated"), x.value)))))

def type_application(x: hydra.phantoms.TTerm[hydra.core.ApplicationType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("application"), x.value)))))

def type_application_term(body: hydra.phantoms.TTerm[hydra.core.Term], type: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeApplicationTerm"), (hydra.core.Field(hydra.core.Name("body"), body.value), hydra.core.Field(hydra.core.Name("type"), type.value))))))

def type_application_term_body(x: hydra.phantoms.TTerm[hydra.core.TypeApplicationTerm]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeApplicationTerm"), hydra.core.Name("body")))), x.value))))

def type_application_term_type(x: hydra.phantoms.TTerm[hydra.core.TypeApplicationTerm]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeApplicationTerm"), hydra.core.Name("type")))), x.value))))

def type_application_term_with_body(original: hydra.phantoms.TTerm[hydra.core.TypeApplicationTerm], new_val: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeApplicationTerm"), (hydra.core.Field(hydra.core.Name("body"), new_val.value), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeApplicationTerm"), hydra.core.Name("type")))), original.value)))))))))

def type_application_term_with_type(original: hydra.phantoms.TTerm[hydra.core.TypeApplicationTerm], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeApplicationTerm"), (hydra.core.Field(hydra.core.Name("body"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeApplicationTerm"), hydra.core.Name("body")))), original.value)))), hydra.core.Field(hydra.core.Name("type"), new_val.value))))))

def type_either(x: hydra.phantoms.TTerm[hydra.core.EitherType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("either"), x.value)))))

def type_forall(x: hydra.phantoms.TTerm[hydra.core.ForallType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("forall"), x.value)))))

def type_function(x: hydra.phantoms.TTerm[hydra.core.FunctionType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("function"), x.value)))))

def type_lambda(parameter: hydra.phantoms.TTerm[hydra.core.Name], body: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeLambda"), (hydra.core.Field(hydra.core.Name("parameter"), parameter.value), hydra.core.Field(hydra.core.Name("body"), body.value))))))

def type_lambda_body(x: hydra.phantoms.TTerm[hydra.core.TypeLambda]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeLambda"), hydra.core.Name("body")))), x.value))))

def type_lambda_parameter(x: hydra.phantoms.TTerm[hydra.core.TypeLambda]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeLambda"), hydra.core.Name("parameter")))), x.value))))

def type_lambda_with_body(original: hydra.phantoms.TTerm[hydra.core.TypeLambda], new_val: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeLambda"), (hydra.core.Field(hydra.core.Name("parameter"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeLambda"), hydra.core.Name("parameter")))), original.value)))), hydra.core.Field(hydra.core.Name("body"), new_val.value))))))

def type_lambda_with_parameter(original: hydra.phantoms.TTerm[hydra.core.TypeLambda], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeLambda"), (hydra.core.Field(hydra.core.Name("parameter"), new_val.value), hydra.core.Field(hydra.core.Name("body"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeLambda"), hydra.core.Name("body")))), original.value)))))))))

def type_list(x: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("list"), x.value)))))

def type_literal(x: hydra.phantoms.TTerm[hydra.core.LiteralType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("literal"), x.value)))))

def type_map(x: hydra.phantoms.TTerm[hydra.core.MapType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("map"), x.value)))))

def type_maybe(x: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("maybe"), x.value)))))

def type_pair(x: hydra.phantoms.TTerm[hydra.core.PairType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("pair"), x.value)))))

def type_record(x: hydra.phantoms.TTerm[frozenlist[hydra.core.FieldType]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("record"), x.value)))))

def type_scheme(variables: hydra.phantoms.TTerm[frozenlist[hydra.core.Name]], type: hydra.phantoms.TTerm[hydra.core.Type], constraints: hydra.phantoms.TTerm[Maybe[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeScheme"), (hydra.core.Field(hydra.core.Name("variables"), variables.value), hydra.core.Field(hydra.core.Name("type"), type.value), hydra.core.Field(hydra.core.Name("constraints"), constraints.value))))))

def type_scheme_constraints(x: hydra.phantoms.TTerm[hydra.core.TypeScheme]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeScheme"), hydra.core.Name("constraints")))), x.value))))

def type_scheme_type(x: hydra.phantoms.TTerm[hydra.core.TypeScheme]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeScheme"), hydra.core.Name("type")))), x.value))))

def type_scheme_variables(x: hydra.phantoms.TTerm[hydra.core.TypeScheme]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeScheme"), hydra.core.Name("variables")))), x.value))))

def type_scheme_with_constraints(original: hydra.phantoms.TTerm[hydra.core.TypeScheme], new_val: hydra.phantoms.TTerm[Maybe[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeScheme"), (hydra.core.Field(hydra.core.Name("variables"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeScheme"), hydra.core.Name("variables")))), original.value)))), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeScheme"), hydra.core.Name("type")))), original.value)))), hydra.core.Field(hydra.core.Name("constraints"), new_val.value))))))

def type_scheme_with_type(original: hydra.phantoms.TTerm[hydra.core.TypeScheme], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeScheme"), (hydra.core.Field(hydra.core.Name("variables"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeScheme"), hydra.core.Name("variables")))), original.value)))), hydra.core.Field(hydra.core.Name("type"), new_val.value), hydra.core.Field(hydra.core.Name("constraints"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeScheme"), hydra.core.Name("constraints")))), original.value)))))))))

def type_scheme_with_variables(original: hydra.phantoms.TTerm[hydra.core.TypeScheme], new_val: hydra.phantoms.TTerm[frozenlist[hydra.core.Name]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeScheme"), (hydra.core.Field(hydra.core.Name("variables"), new_val.value), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeScheme"), hydra.core.Name("type")))), original.value)))), hydra.core.Field(hydra.core.Name("constraints"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeScheme"), hydra.core.Name("constraints")))), original.value)))))))))

def type_set(x: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("set"), x.value)))))

def type_union(x: hydra.phantoms.TTerm[frozenlist[hydra.core.FieldType]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("union"), x.value)))))

type_unit = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("unit"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def type_variable(x: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("variable"), x.value)))))

def type_variable_metadata(classes: hydra.phantoms.TTerm[frozenset[hydra.core.Name]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeVariableMetadata"), (hydra.core.Field(hydra.core.Name("classes"), classes.value),)))))

def type_variable_metadata_classes(x: hydra.phantoms.TTerm[hydra.core.TypeVariableMetadata]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.TypeVariableMetadata"), hydra.core.Name("classes")))), x.value))))

def type_variable_metadata_with_classes(original: hydra.phantoms.TTerm[hydra.core.TypeVariableMetadata], new_val: hydra.phantoms.TTerm[frozenset[hydra.core.Name]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeVariableMetadata"), (hydra.core.Field(hydra.core.Name("classes"), new_val.value),)))))

type_void = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("void"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def type_wrap(x: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("wrap"), x.value)))))

def un_name(x: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.core.Name"))), x.value))))

def wrapped_term(type_name: hydra.phantoms.TTerm[hydra.core.Name], body: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedTerm"), (hydra.core.Field(hydra.core.Name("typeName"), type_name.value), hydra.core.Field(hydra.core.Name("body"), body.value))))))

def wrapped_term_body(x: hydra.phantoms.TTerm[hydra.core.WrappedTerm]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.WrappedTerm"), hydra.core.Name("body")))), x.value))))

def wrapped_term_type_name(x: hydra.phantoms.TTerm[hydra.core.WrappedTerm]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.WrappedTerm"), hydra.core.Name("typeName")))), x.value))))

def wrapped_term_with_body(original: hydra.phantoms.TTerm[hydra.core.WrappedTerm], new_val: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedTerm"), (hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.WrappedTerm"), hydra.core.Name("typeName")))), original.value)))), hydra.core.Field(hydra.core.Name("body"), new_val.value))))))

def wrapped_term_with_type_name(original: hydra.phantoms.TTerm[hydra.core.WrappedTerm], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedTerm"), (hydra.core.Field(hydra.core.Name("typeName"), new_val.value), hydra.core.Field(hydra.core.Name("body"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.core.WrappedTerm"), hydra.core.Name("body")))), original.value)))))))))
