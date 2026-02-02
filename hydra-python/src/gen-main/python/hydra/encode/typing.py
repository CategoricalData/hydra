# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.typing."""

from __future__ import annotations
from typing import cast
import hydra.core
import hydra.encode.core
import hydra.lib.maps
import hydra.lib.sets
import hydra.typing

def inference_context(x: hydra.typing.InferenceContext) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.typing.InferenceContext"), (hydra.core.Field(hydra.core.Name("schemaTypes"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.type_scheme, x.schema_types)))), hydra.core.Field(hydra.core.Name("primitiveTypes"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.type_scheme, x.primitive_types)))), hydra.core.Field(hydra.core.Name("dataTypes"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.type_scheme, x.data_types)))), hydra.core.Field(hydra.core.Name("classConstraints"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.type_variable_metadata, x.class_constraints)))), hydra.core.Field(hydra.core.Name("debug"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(x.debug)))))))))

def type_subst(x: hydra.typing.TypeSubst) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.typing.TypeSubst"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.type, x.value))))))

def inference_result(x: hydra.typing.InferenceResult) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.typing.InferenceResult"), (hydra.core.Field(hydra.core.Name("term"), hydra.encode.core.term(x.term)), hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.type(x.type)), hydra.core.Field(hydra.core.Name("subst"), type_subst(x.subst)), hydra.core.Field(hydra.core.Name("classConstraints"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.type_variable_metadata, x.class_constraints))))))))

def term_subst(x: hydra.typing.TermSubst) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.typing.TermSubst"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.term, x.value))))))

def type_constraint(x: hydra.typing.TypeConstraint) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.typing.TypeConstraint"), (hydra.core.Field(hydra.core.Name("left"), hydra.encode.core.type(x.left)), hydra.core.Field(hydra.core.Name("right"), hydra.encode.core.type(x.right)), hydra.core.Field(hydra.core.Name("comment"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.comment)))))))))

def type_context(x: hydra.typing.TypeContext) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.typing.TypeContext"), (hydra.core.Field(hydra.core.Name("types"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.type, x.types)))), hydra.core.Field(hydra.core.Name("metadata"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.term, x.metadata)))), hydra.core.Field(hydra.core.Name("typeVariables"), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map(hydra.encode.core.name, x.type_variables)))), hydra.core.Field(hydra.core.Name("lambdaVariables"), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map(hydra.encode.core.name, x.lambda_variables)))), hydra.core.Field(hydra.core.Name("letVariables"), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map(hydra.encode.core.name, x.let_variables)))), hydra.core.Field(hydra.core.Name("inferenceContext"), inference_context(x.inference_context))))))
