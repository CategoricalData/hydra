# Note: this is an automatically generated file. Do not edit.

r"""Helper types for Java code generation."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import FrozenDict, Maybe, Node
from typing import Annotated, TypeAlias
import hydra.core
import hydra.ext.java.syntax
import hydra.graph
import hydra.module

class JavaSymbolClassConstant:
    r"""A constant value"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, JavaSymbolClassConstant)
    def __hash__(self):
        return hash("JavaSymbolClassConstant")

class JavaSymbolClassNullaryFunction:
    r"""A nullary function (no arguments)"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, JavaSymbolClassNullaryFunction)
    def __hash__(self):
        return hash("JavaSymbolClassNullaryFunction")

class JavaSymbolClassHoistedLambda(Node[int]):
    r"""A hoisted lambda wrapped in type lambdas. The Int is the number of curried lambda parameters."""

class JavaSymbolClassUnaryFunction:
    r"""A unary function (single argument)"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, JavaSymbolClassUnaryFunction)
    def __hash__(self):
        return hash("JavaSymbolClassUnaryFunction")

class JavaSymbolClassLocalVariable:
    r"""A local variable"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, JavaSymbolClassLocalVariable)
    def __hash__(self):
        return hash("JavaSymbolClassLocalVariable")

class _JavaSymbolClassMeta(type):
    def __getitem__(cls, item):
        return object

# Classification of a Java symbol for code generation.
class JavaSymbolClass(metaclass=_JavaSymbolClassMeta):
    r"""JavaSymbolClassConstant | JavaSymbolClassNullaryFunction | JavaSymbolClassHoistedLambda | JavaSymbolClassUnaryFunction | JavaSymbolClassLocalVariable"""
    
    TYPE_ = hydra.core.Name("hydra.ext.java.helpers.JavaSymbolClass")
    CONSTANT = hydra.core.Name("constant")
    NULLARY_FUNCTION = hydra.core.Name("nullaryFunction")
    HOISTED_LAMBDA = hydra.core.Name("hoistedLambda")
    UNARY_FUNCTION = hydra.core.Name("unaryFunction")
    LOCAL_VARIABLE = hydra.core.Name("localVariable")

@dataclass(frozen=True)
class JavaFeatures:
    r"""Feature flags for the target Java version."""
    
    supports_diamond_operator: Annotated[bool, "Whether the diamond operator (<>) is supported (Java 7+)"]
    
    TYPE_ = hydra.core.Name("hydra.ext.java.helpers.JavaFeatures")
    SUPPORTS_DIAMOND_OPERATOR = hydra.core.Name("supportsDiamondOperator")

@dataclass(frozen=True)
class Aliases:
    r"""Aliases and context for Java code generation."""
    
    current_namespace: Annotated[hydra.module.Namespace, "Current module namespace context"]
    packages: Annotated[FrozenDict[hydra.module.Namespace, hydra.ext.java.syntax.PackageName], "Maps namespaces to Java package names"]
    branch_vars: Annotated[frozenset[hydra.core.Name], "Variables bound in pattern matching branches"]
    recursive_vars: Annotated[frozenset[hydra.core.Name], "Variables that are self-recursive"]
    in_scope_type_params: Annotated[frozenset[hydra.core.Name], "Type parameters that are in scope (from method-level type parameters)"]
    polymorphic_locals: Annotated[frozenset[hydra.core.Name], "Local variables that have polymorphic types (declared with raw types)"]
    in_scope_java_vars: Annotated[frozenset[hydra.core.Name], "All in-scope Java variable names (for avoiding lambda parameter shadowing)"]
    var_renames: Annotated[FrozenDict[hydra.core.Name, hydra.core.Name], "Variable renames for avoiding shadowing (maps Hydra name to Java name)"]
    lambda_vars: Annotated[frozenset[hydra.core.Name], "Lambda-bound variables (including hoisted captures with qualified names)"]
    type_var_subst: Annotated[FrozenDict[hydra.core.Name, hydra.core.Name], "Type variable substitution: maps fresh inference variable names to canonical scheme variable names"]
    trusted_type_vars: Annotated[frozenset[hydra.core.Name], "Type variables that actually appear in the method's formal parameter types"]
    method_codomain: Annotated[Maybe[hydra.core.Type], "The enclosing method's codomain (return type), used for casting pair expressions"]
    thunked_vars: Annotated[frozenset[hydra.core.Name], "Variables that have been thunked (wrapped in Supplier) for lazy evaluation"]
    
    TYPE_ = hydra.core.Name("hydra.ext.java.helpers.Aliases")
    CURRENT_NAMESPACE = hydra.core.Name("currentNamespace")
    PACKAGES = hydra.core.Name("packages")
    BRANCH_VARS = hydra.core.Name("branchVars")
    RECURSIVE_VARS = hydra.core.Name("recursiveVars")
    IN_SCOPE_TYPE_PARAMS = hydra.core.Name("inScopeTypeParams")
    POLYMORPHIC_LOCALS = hydra.core.Name("polymorphicLocals")
    IN_SCOPE_JAVA_VARS = hydra.core.Name("inScopeJavaVars")
    VAR_RENAMES = hydra.core.Name("varRenames")
    LAMBDA_VARS = hydra.core.Name("lambdaVars")
    TYPE_VAR_SUBST = hydra.core.Name("typeVarSubst")
    TRUSTED_TYPE_VARS = hydra.core.Name("trustedTypeVars")
    METHOD_CODOMAIN = hydra.core.Name("methodCodomain")
    THUNKED_VARS = hydra.core.Name("thunkedVars")

@dataclass(frozen=True)
class JavaEnvironment:
    r"""Environment for Java code generation."""
    
    aliases: Annotated[Aliases, "Aliases and context state"]
    graph: Annotated[hydra.graph.Graph, "Graph context for type inference"]
    
    TYPE_ = hydra.core.Name("hydra.ext.java.helpers.JavaEnvironment")
    ALIASES = hydra.core.Name("aliases")
    GRAPH = hydra.core.Name("graph")
