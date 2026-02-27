# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.typing."""

from __future__ import annotations
from collections.abc import Callable
from typing import TypeVar, cast
import hydra.core
import hydra.encode.core
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.sets
import hydra.typing

T0 = TypeVar("T0")

def function_structure(env: Callable[[T0], hydra.core.Term], x: hydra.typing.FunctionStructure[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.typing.FunctionStructure"), (hydra.core.Field(hydra.core.Name("typeParams"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: hydra.encode.core.name(x1)), x.type_params)))), hydra.core.Field(hydra.core.Name("params"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: hydra.encode.core.name(x1)), x.params)))), hydra.core.Field(hydra.core.Name("bindings"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: hydra.encode.core.binding(x1)), x.bindings)))), hydra.core.Field(hydra.core.Name("body"), hydra.encode.core.term(x.body)), hydra.core.Field(hydra.core.Name("domains"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: hydra.encode.core.type(x1)), x.domains)))), hydra.core.Field(hydra.core.Name("codomain"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x1: hydra.encode.core.type(x1)), x.codomain)))), hydra.core.Field(hydra.core.Name("environment"), env(x.environment))))))

def inference_context(x: hydra.typing.InferenceContext) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.typing.InferenceContext"), (hydra.core.Field(hydra.core.Name("schemaTypes"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: hydra.encode.core.name(x1)), (lambda x1: hydra.encode.core.type_scheme(x1)), x.schema_types)))), hydra.core.Field(hydra.core.Name("primitiveTypes"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: hydra.encode.core.name(x1)), (lambda x1: hydra.encode.core.type_scheme(x1)), x.primitive_types)))), hydra.core.Field(hydra.core.Name("dataTypes"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: hydra.encode.core.name(x1)), (lambda x1: hydra.encode.core.type_scheme(x1)), x.data_types)))), hydra.core.Field(hydra.core.Name("classConstraints"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: hydra.encode.core.name(x1)), (lambda x1: hydra.encode.core.type_variable_metadata(x1)), x.class_constraints)))), hydra.core.Field(hydra.core.Name("debug"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(x.debug)))))))))

def type_subst(x: hydra.typing.TypeSubst) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.typing.TypeSubst"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: hydra.encode.core.name(x1)), (lambda x1: hydra.encode.core.type(x1)), x.value))))))

def inference_result(x: hydra.typing.InferenceResult) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.typing.InferenceResult"), (hydra.core.Field(hydra.core.Name("term"), hydra.encode.core.term(x.term)), hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.type(x.type)), hydra.core.Field(hydra.core.Name("subst"), type_subst(x.subst)), hydra.core.Field(hydra.core.Name("classConstraints"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: hydra.encode.core.name(x1)), (lambda x1: hydra.encode.core.type_variable_metadata(x1)), x.class_constraints))))))))

def term_subst(x: hydra.typing.TermSubst) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.typing.TermSubst"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: hydra.encode.core.name(x1)), (lambda x1: hydra.encode.core.term(x1)), x.value))))))

def type_constraint(x: hydra.typing.TypeConstraint) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.typing.TypeConstraint"), (hydra.core.Field(hydra.core.Name("left"), hydra.encode.core.type(x.left)), hydra.core.Field(hydra.core.Name("right"), hydra.encode.core.type(x.right)), hydra.core.Field(hydra.core.Name("comment"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.comment)))))))))

def type_context(x: hydra.typing.TypeContext) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.typing.TypeContext"), (hydra.core.Field(hydra.core.Name("types"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: hydra.encode.core.name(x1)), (lambda x1: hydra.encode.core.type(x1)), x.types)))), hydra.core.Field(hydra.core.Name("metadata"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: hydra.encode.core.name(x1)), (lambda x1: hydra.encode.core.term(x1)), x.metadata)))), hydra.core.Field(hydra.core.Name("typeVariables"), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map((lambda x1: hydra.encode.core.name(x1)), x.type_variables)))), hydra.core.Field(hydra.core.Name("lambdaVariables"), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map((lambda x1: hydra.encode.core.name(x1)), x.lambda_variables)))), hydra.core.Field(hydra.core.Name("letVariables"), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map((lambda x1: hydra.encode.core.name(x1)), x.let_variables)))), hydra.core.Field(hydra.core.Name("inferenceContext"), inference_context(x.inference_context))))))
