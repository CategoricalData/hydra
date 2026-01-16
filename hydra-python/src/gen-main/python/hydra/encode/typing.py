# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.typing."""

from __future__ import annotations
from hydra.dsl.python import FrozenDict
from typing import cast
import hydra.core
import hydra.encode.core
import hydra.lib.maps
import hydra.lib.sets
import hydra.typing

def inference_context(x: hydra.typing.InferenceContext) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.typing.InferenceContext"), (hydra.core.Field(hydra.core.Name("schemaTypes"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.type_scheme, m))))(x.schema_types)), hydra.core.Field(hydra.core.Name("primitiveTypes"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.type_scheme, m))))(x.primitive_types)), hydra.core.Field(hydra.core.Name("dataTypes"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.type_scheme, m))))(x.data_types)), hydra.core.Field(hydra.core.Name("classConstraints"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.type_variable_metadata, m))))(x.class_constraints)), hydra.core.Field(hydra.core.Name("debug"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(x2)))))(x.debug))))))

def type_subst(x: hydra.typing.TypeSubst) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.typing.TypeSubst"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.type, m))))(x.value))))

def inference_result(x: hydra.typing.InferenceResult) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.typing.InferenceResult"), (hydra.core.Field(hydra.core.Name("term"), hydra.encode.core.term(x.term)), hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.type(x.type)), hydra.core.Field(hydra.core.Name("subst"), type_subst(x.subst)), hydra.core.Field(hydra.core.Name("classConstraints"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.type_variable_metadata, m))))(x.class_constraints))))))

def term_subst(x: hydra.typing.TermSubst) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.typing.TermSubst"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.term, m))))(x.value))))

def type_constraint(x: hydra.typing.TypeConstraint) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.typing.TypeConstraint"), (hydra.core.Field(hydra.core.Name("left"), hydra.encode.core.type(x.left)), hydra.core.Field(hydra.core.Name("right"), hydra.encode.core.type(x.right)), hydra.core.Field(hydra.core.Name("comment"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.comment))))))

def type_context(x: hydra.typing.TypeContext) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.typing.TypeContext"), (hydra.core.Field(hydra.core.Name("types"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.type, m))))(x.types)), hydra.core.Field(hydra.core.Name("metadata"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.term, m))))(x.metadata)), hydra.core.Field(hydra.core.Name("typeVariables"), (lambda s: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map(hydra.encode.core.name, s))))(x.type_variables)), hydra.core.Field(hydra.core.Name("lambdaVariables"), (lambda s: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map(hydra.encode.core.name, s))))(x.lambda_variables)), hydra.core.Field(hydra.core.Name("inferenceContext"), inference_context(x.inference_context))))))
