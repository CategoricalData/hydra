# Note: this is an automatically generated file. Do not edit.

r"""Types supporting type inference and type reconstruction."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import FrozenDict, Maybe, Node, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar
import hydra.core

Env = TypeVar("Env")

@dataclass(frozen=True)
class FunctionStructure(Generic[Env]):
    r"""A structured representation of a function term's components, replacing ad-hoc tuples. This captures all the information extracted from peeling lambdas, type lambdas, lets, and type applications from a term."""
    
    type_params: Annotated[frozenlist[hydra.core.Name], "Type parameters (from type lambdas)"]
    params: Annotated[frozenlist[hydra.core.Name], "Value parameters (from lambdas)"]
    bindings: Annotated[frozenlist[hydra.core.Binding], "Let bindings accumulated from the term"]
    body: Annotated[hydra.core.Term, "The body term after removing all lambdas, lets, etc."]
    domains: Annotated[frozenlist[hydra.core.Type], "Domain types of the value parameters"]
    codomain: Annotated[Maybe[hydra.core.Type], "The return type of the function (if type inference succeeded)"]
    environment: Annotated[Env, "Updated environment after processing all bindings"]

FUNCTION_STRUCTURE__NAME = hydra.core.Name("hydra.typing.FunctionStructure")
FUNCTION_STRUCTURE__TYPE_PARAMS__NAME = hydra.core.Name("typeParams")
FUNCTION_STRUCTURE__PARAMS__NAME = hydra.core.Name("params")
FUNCTION_STRUCTURE__BINDINGS__NAME = hydra.core.Name("bindings")
FUNCTION_STRUCTURE__BODY__NAME = hydra.core.Name("body")
FUNCTION_STRUCTURE__DOMAINS__NAME = hydra.core.Name("domains")
FUNCTION_STRUCTURE__CODOMAIN__NAME = hydra.core.Name("codomain")
FUNCTION_STRUCTURE__ENVIRONMENT__NAME = hydra.core.Name("environment")

@dataclass(frozen=True)
class InferenceContext:
    r"""The context provided to type inference, including various typing environments."""
    
    schema_types: Annotated[FrozenDict[hydra.core.Name, hydra.core.TypeScheme], "A fixed typing environment which is derived from the schema of the graph."]
    primitive_types: Annotated[FrozenDict[hydra.core.Name, hydra.core.TypeScheme], "A fixed typing environment which is derived from the set of primitives in the graph."]
    data_types: Annotated[FrozenDict[hydra.core.Name, hydra.core.TypeScheme], "A mutable typing environment which is specific to the current graph being processed. This environment is (usually) smaller than the schema and primitive typing environments, and is subject to global substitutions."]
    class_constraints: Annotated[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata], "A mutable map from type variable names to their accumulated class constraints. This is populated during type inference when operations requiring Eq or Ord are encountered."]
    debug: Annotated[bool, "Whether to enable debug output during type inference"]

INFERENCE_CONTEXT__NAME = hydra.core.Name("hydra.typing.InferenceContext")
INFERENCE_CONTEXT__SCHEMA_TYPES__NAME = hydra.core.Name("schemaTypes")
INFERENCE_CONTEXT__PRIMITIVE_TYPES__NAME = hydra.core.Name("primitiveTypes")
INFERENCE_CONTEXT__DATA_TYPES__NAME = hydra.core.Name("dataTypes")
INFERENCE_CONTEXT__CLASS_CONSTRAINTS__NAME = hydra.core.Name("classConstraints")
INFERENCE_CONTEXT__DEBUG__NAME = hydra.core.Name("debug")

@dataclass(frozen=True)
class InferenceResult:
    r"""The result of applying inference rules to a term."""
    
    term: Annotated[hydra.core.Term, "The term which was inferred"]
    type: Annotated[hydra.core.Type, "The inferred type of the term"]
    subst: Annotated[TypeSubst, "The type substitution resulting from unification"]
    class_constraints: Annotated[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata], "Class constraints discovered during inference (e.g., Ord constraints from Map.lookup)"]

INFERENCE_RESULT__NAME = hydra.core.Name("hydra.typing.InferenceResult")
INFERENCE_RESULT__TERM__NAME = hydra.core.Name("term")
INFERENCE_RESULT__TYPE__NAME = hydra.core.Name("type")
INFERENCE_RESULT__SUBST__NAME = hydra.core.Name("subst")
INFERENCE_RESULT__CLASS_CONSTRAINTS__NAME = hydra.core.Name("classConstraints")

class TermSubst(Node["FrozenDict[hydra.core.Name, hydra.core.Term]"]):
    r"""A substitution of term variables for terms."""

TERM_SUBST__NAME = hydra.core.Name("hydra.typing.TermSubst")

@dataclass(frozen=True)
class TypeConstraint:
    r"""An assertion that two types can be unified into a single type."""
    
    left: Annotated[hydra.core.Type, "The left-hand side of the constraint"]
    right: Annotated[hydra.core.Type, "The right-hand side of the constraint"]
    comment: Annotated[str, "A description of the type constraint which may be used for tracing or debugging"]

TYPE_CONSTRAINT__NAME = hydra.core.Name("hydra.typing.TypeConstraint")
TYPE_CONSTRAINT__LEFT__NAME = hydra.core.Name("left")
TYPE_CONSTRAINT__RIGHT__NAME = hydra.core.Name("right")
TYPE_CONSTRAINT__COMMENT__NAME = hydra.core.Name("comment")

@dataclass(frozen=True)
class TypeContext:
    r"""A typing environment used for type reconstruction (typeOf) over System F terms."""
    
    types: Annotated[FrozenDict[hydra.core.Name, hydra.core.Type], "A mapping of lambda- and let-bound variables to their types"]
    metadata: Annotated[FrozenDict[hydra.core.Name, hydra.core.Term], "Any additional metadata about lambda- and let-bound variables"]
    type_variables: Annotated[frozenset[hydra.core.Name], "The set of type variables introduced by enclosing type lambdas"]
    lambda_variables: Annotated[frozenset[hydra.core.Name], "The set of term variables introduced by lambdas (even if untyped)"]
    let_variables: Annotated[frozenset[hydra.core.Name], "The set of term variables introduced by let bindings (even if untyped)"]
    inference_context: Annotated[InferenceContext, "The schema types, primitive types, and data types of the graph"]

TYPE_CONTEXT__NAME = hydra.core.Name("hydra.typing.TypeContext")
TYPE_CONTEXT__TYPES__NAME = hydra.core.Name("types")
TYPE_CONTEXT__METADATA__NAME = hydra.core.Name("metadata")
TYPE_CONTEXT__TYPE_VARIABLES__NAME = hydra.core.Name("typeVariables")
TYPE_CONTEXT__LAMBDA_VARIABLES__NAME = hydra.core.Name("lambdaVariables")
TYPE_CONTEXT__LET_VARIABLES__NAME = hydra.core.Name("letVariables")
TYPE_CONTEXT__INFERENCE_CONTEXT__NAME = hydra.core.Name("inferenceContext")

class TypeSubst(Node["FrozenDict[hydra.core.Name, hydra.core.Type]"]):
    r"""A substitution of type variables for types."""

TYPE_SUBST__NAME = hydra.core.Name("hydra.typing.TypeSubst")
