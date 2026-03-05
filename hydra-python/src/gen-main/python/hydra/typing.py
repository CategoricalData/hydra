# Note: this is an automatically generated file. Do not edit.

r"""Types supporting type inference and type reconstruction."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Maybe, Node, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar, cast
import hydra.context
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
    
    TYPE_ = hydra.core.Name("hydra.typing.FunctionStructure")
    TYPE_PARAMS = hydra.core.Name("typeParams")
    PARAMS = hydra.core.Name("params")
    BINDINGS = hydra.core.Name("bindings")
    BODY = hydra.core.Name("body")
    DOMAINS = hydra.core.Name("domains")
    CODOMAIN = hydra.core.Name("codomain")
    ENVIRONMENT = hydra.core.Name("environment")

@dataclass(frozen=True)
class InferenceResult:
    r"""The result of applying inference rules to a term."""
    
    term: Annotated[hydra.core.Term, "The term which was inferred"]
    type: Annotated[hydra.core.Type, "The inferred type of the term"]
    subst: Annotated[TypeSubst, "The type substitution resulting from unification"]
    class_constraints: Annotated[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata], "Class constraints discovered during inference (e.g., Ord constraints from Map.lookup)"]
    context: Annotated[hydra.context.Context, "The updated context after inference (carries fresh variable state)"]
    
    TYPE_ = hydra.core.Name("hydra.typing.InferenceResult")
    TERM = hydra.core.Name("term")
    TYPE = hydra.core.Name("type")
    SUBST = hydra.core.Name("subst")
    CLASS_CONSTRAINTS = hydra.core.Name("classConstraints")
    CONTEXT = hydra.core.Name("context")

class TermSubst(Node["FrozenDict[hydra.core.Name, hydra.core.Term]"]):
    r"""A substitution of term variables for terms."""

TermSubst.TYPE_ = hydra.core.Name("hydra.typing.TermSubst")

@dataclass(frozen=True)
class TypeConstraint:
    r"""An assertion that two types can be unified into a single type."""
    
    left: Annotated[hydra.core.Type, "The left-hand side of the constraint"]
    right: Annotated[hydra.core.Type, "The right-hand side of the constraint"]
    comment: Annotated[str, "A description of the type constraint which may be used for tracing or debugging"]
    
    TYPE_ = hydra.core.Name("hydra.typing.TypeConstraint")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")
    COMMENT = hydra.core.Name("comment")

class TypeSubst(Node["FrozenDict[hydra.core.Name, hydra.core.Type]"]):
    r"""A substitution of type variables for types."""

TypeSubst.TYPE_ = hydra.core.Name("hydra.typing.TypeSubst")
