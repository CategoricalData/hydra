# Note: this is an automatically generated file. Do not edit.

r"""A model for term access patterns."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class AccessorEdge:
    r"""An edge in an accessor graph, connecting two nodes via a path."""
    
    source: Annotated[AccessorNode, "The source node of the edge"]
    path: Annotated[AccessorPath, "The accessor path connecting source to target"]
    target: Annotated[AccessorNode, "The target node of the edge"]
    
    TYPE_ = hydra.core.Name("hydra.accessors.AccessorEdge")
    SOURCE = hydra.core.Name("source")
    PATH = hydra.core.Name("path")
    TARGET = hydra.core.Name("target")

@dataclass(frozen=True)
class AccessorGraph:
    r"""A graph of accessor nodes and edges, representing term access patterns."""
    
    nodes: Annotated[frozenlist[AccessorNode], "All nodes in the graph"]
    edges: Annotated[frozenlist[AccessorEdge], "All edges in the graph"]
    
    TYPE_ = hydra.core.Name("hydra.accessors.AccessorGraph")
    NODES = hydra.core.Name("nodes")
    EDGES = hydra.core.Name("edges")

@dataclass(frozen=True)
class AccessorNode:
    r"""A node in an accessor graph, representing a term or subterm."""
    
    name: Annotated[hydra.core.Name, "The qualified name of the term"]
    label: Annotated[str, "A human-readable label for the node"]
    id: Annotated[str, "A unique identifier for the node"]
    
    TYPE_ = hydra.core.Name("hydra.accessors.AccessorNode")
    NAME = hydra.core.Name("name")
    LABEL = hydra.core.Name("label")
    ID = hydra.core.Name("id")

class AccessorPath(Node["frozenlist[TermAccessor]"]):
    r"""A sequence of term accessors forming a path through a term."""

AccessorPath.TYPE_ = hydra.core.Name("hydra.accessors.AccessorPath")

class TermAccessorAnnotatedBody:
    r"""Access the body of an annotated term"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TermAccessorAnnotatedBody)
    def __hash__(self):
        return hash("TermAccessorAnnotatedBody")

class TermAccessorApplicationFunction:
    r"""Access the function of an application term"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TermAccessorApplicationFunction)
    def __hash__(self):
        return hash("TermAccessorApplicationFunction")

class TermAccessorApplicationArgument:
    r"""Access the argument of an application term"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TermAccessorApplicationArgument)
    def __hash__(self):
        return hash("TermAccessorApplicationArgument")

class TermAccessorLambdaBody:
    r"""Access the body of a lambda term"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TermAccessorLambdaBody)
    def __hash__(self):
        return hash("TermAccessorLambdaBody")

class TermAccessorUnionCasesDefault:
    r"""Access the default case of a union elimination"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TermAccessorUnionCasesDefault)
    def __hash__(self):
        return hash("TermAccessorUnionCasesDefault")

class TermAccessorUnionCasesBranch(Node["hydra.core.Name"]):
    r"""Access a specific branch of a union elimination by field name"""

class TermAccessorLetBody:
    r"""Access the body of a let term"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TermAccessorLetBody)
    def __hash__(self):
        return hash("TermAccessorLetBody")

class TermAccessorLetBinding(Node["hydra.core.Name"]):
    r"""Access a specific binding in a let term by variable name"""

class TermAccessorListElement(Node[int]):
    r"""Access an element of a list by index"""

class TermAccessorMapKey(Node[int]):
    r"""Access a key in a map by index"""

class TermAccessorMapValue(Node[int]):
    r"""Access a value in a map by index"""

class TermAccessorMaybeTerm:
    r"""Access the term inside a Just value"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TermAccessorMaybeTerm)
    def __hash__(self):
        return hash("TermAccessorMaybeTerm")

class TermAccessorProductTerm(Node[int]):
    r"""Access an element of a product (tuple) by index"""

class TermAccessorRecordField(Node["hydra.core.Name"]):
    r"""Access a field of a record by field name"""

class TermAccessorSetElement(Node[int]):
    r"""Access an element of a set by index"""

class TermAccessorSumTerm:
    r"""Access the term inside a sum variant"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TermAccessorSumTerm)
    def __hash__(self):
        return hash("TermAccessorSumTerm")

class TermAccessorTypeLambdaBody:
    r"""Access the body of a type lambda term"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TermAccessorTypeLambdaBody)
    def __hash__(self):
        return hash("TermAccessorTypeLambdaBody")

class TermAccessorTypeApplicationTerm:
    r"""Access the term being applied to a type"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TermAccessorTypeApplicationTerm)
    def __hash__(self):
        return hash("TermAccessorTypeApplicationTerm")

class TermAccessorInjectionTerm:
    r"""Access the term inside a union injection"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TermAccessorInjectionTerm)
    def __hash__(self):
        return hash("TermAccessorInjectionTerm")

class TermAccessorWrappedTerm:
    r"""Access the term inside a wrapped term"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TermAccessorWrappedTerm)
    def __hash__(self):
        return hash("TermAccessorWrappedTerm")

class _TermAccessorMeta(type):
    def __getitem__(cls, item):
        return object

# A function which maps from a term to a particular immediate subterm.
class TermAccessor(metaclass=_TermAccessorMeta):
    r"""TermAccessorAnnotatedBody | TermAccessorApplicationFunction | TermAccessorApplicationArgument | TermAccessorLambdaBody | TermAccessorUnionCasesDefault | TermAccessorUnionCasesBranch | TermAccessorLetBody | TermAccessorLetBinding | TermAccessorListElement | TermAccessorMapKey | TermAccessorMapValue | TermAccessorMaybeTerm | TermAccessorProductTerm | TermAccessorRecordField | TermAccessorSetElement | TermAccessorSumTerm | TermAccessorTypeLambdaBody | TermAccessorTypeApplicationTerm | TermAccessorInjectionTerm | TermAccessorWrappedTerm"""
    
    TYPE_ = hydra.core.Name("hydra.accessors.TermAccessor")
    ANNOTATED_BODY = hydra.core.Name("annotatedBody")
    APPLICATION_FUNCTION = hydra.core.Name("applicationFunction")
    APPLICATION_ARGUMENT = hydra.core.Name("applicationArgument")
    LAMBDA_BODY = hydra.core.Name("lambdaBody")
    UNION_CASES_DEFAULT = hydra.core.Name("unionCasesDefault")
    UNION_CASES_BRANCH = hydra.core.Name("unionCasesBranch")
    LET_BODY = hydra.core.Name("letBody")
    LET_BINDING = hydra.core.Name("letBinding")
    LIST_ELEMENT = hydra.core.Name("listElement")
    MAP_KEY = hydra.core.Name("mapKey")
    MAP_VALUE = hydra.core.Name("mapValue")
    MAYBE_TERM = hydra.core.Name("maybeTerm")
    PRODUCT_TERM = hydra.core.Name("productTerm")
    RECORD_FIELD = hydra.core.Name("recordField")
    SET_ELEMENT = hydra.core.Name("setElement")
    SUM_TERM = hydra.core.Name("sumTerm")
    TYPE_LAMBDA_BODY = hydra.core.Name("typeLambdaBody")
    TYPE_APPLICATION_TERM = hydra.core.Name("typeApplicationTerm")
    INJECTION_TERM = hydra.core.Name("injectionTerm")
    WRAPPED_TERM = hydra.core.Name("wrappedTerm")
