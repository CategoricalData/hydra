# Note: this is an automatically generated file. Do not edit.

r"""A model for term access patterns."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import Node, frozenlist
from typing import Annotated, TypeAlias
import hydra.core

@dataclass(frozen=True)
class AccessorEdge:
    r"""An edge in an accessor graph, connecting two nodes via a path."""
    
    source: Annotated[AccessorNode, "The source node of the edge"]
    path: Annotated[AccessorPath, "The accessor path connecting source to target"]
    target: Annotated[AccessorNode, "The target node of the edge"]

ACCESSOR_EDGE__NAME = hydra.core.Name("hydra.accessors.AccessorEdge")
ACCESSOR_EDGE__SOURCE__NAME = hydra.core.Name("source")
ACCESSOR_EDGE__PATH__NAME = hydra.core.Name("path")
ACCESSOR_EDGE__TARGET__NAME = hydra.core.Name("target")

@dataclass(frozen=True)
class AccessorGraph:
    r"""A graph of accessor nodes and edges, representing term access patterns."""
    
    nodes: Annotated[frozenlist[AccessorNode], "All nodes in the graph"]
    edges: Annotated[frozenlist[AccessorEdge], "All edges in the graph"]

ACCESSOR_GRAPH__NAME = hydra.core.Name("hydra.accessors.AccessorGraph")
ACCESSOR_GRAPH__NODES__NAME = hydra.core.Name("nodes")
ACCESSOR_GRAPH__EDGES__NAME = hydra.core.Name("edges")

@dataclass(frozen=True)
class AccessorNode:
    r"""A node in an accessor graph, representing a term or subterm."""
    
    name: Annotated[hydra.core.Name, "The qualified name of the term"]
    label: Annotated[str, "A human-readable label for the node"]
    id: Annotated[str, "A unique identifier for the node"]

ACCESSOR_NODE__NAME = hydra.core.Name("hydra.accessors.AccessorNode")
ACCESSOR_NODE__NAME__NAME = hydra.core.Name("name")
ACCESSOR_NODE__LABEL__NAME = hydra.core.Name("label")
ACCESSOR_NODE__ID__NAME = hydra.core.Name("id")

class AccessorPath(Node["frozenlist[TermAccessor]"]):
    r"""A sequence of term accessors forming a path through a term."""

ACCESSOR_PATH__NAME = hydra.core.Name("hydra.accessors.AccessorPath")

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
    
    pass

TERM_ACCESSOR__NAME = hydra.core.Name("hydra.accessors.TermAccessor")
TERM_ACCESSOR__ANNOTATED_BODY__NAME = hydra.core.Name("annotatedBody")
TERM_ACCESSOR__APPLICATION_FUNCTION__NAME = hydra.core.Name("applicationFunction")
TERM_ACCESSOR__APPLICATION_ARGUMENT__NAME = hydra.core.Name("applicationArgument")
TERM_ACCESSOR__LAMBDA_BODY__NAME = hydra.core.Name("lambdaBody")
TERM_ACCESSOR__UNION_CASES_DEFAULT__NAME = hydra.core.Name("unionCasesDefault")
TERM_ACCESSOR__UNION_CASES_BRANCH__NAME = hydra.core.Name("unionCasesBranch")
TERM_ACCESSOR__LET_BODY__NAME = hydra.core.Name("letBody")
TERM_ACCESSOR__LET_BINDING__NAME = hydra.core.Name("letBinding")
TERM_ACCESSOR__LIST_ELEMENT__NAME = hydra.core.Name("listElement")
TERM_ACCESSOR__MAP_KEY__NAME = hydra.core.Name("mapKey")
TERM_ACCESSOR__MAP_VALUE__NAME = hydra.core.Name("mapValue")
TERM_ACCESSOR__MAYBE_TERM__NAME = hydra.core.Name("maybeTerm")
TERM_ACCESSOR__PRODUCT_TERM__NAME = hydra.core.Name("productTerm")
TERM_ACCESSOR__RECORD_FIELD__NAME = hydra.core.Name("recordField")
TERM_ACCESSOR__SET_ELEMENT__NAME = hydra.core.Name("setElement")
TERM_ACCESSOR__SUM_TERM__NAME = hydra.core.Name("sumTerm")
TERM_ACCESSOR__TYPE_LAMBDA_BODY__NAME = hydra.core.Name("typeLambdaBody")
TERM_ACCESSOR__TYPE_APPLICATION_TERM__NAME = hydra.core.Name("typeApplicationTerm")
TERM_ACCESSOR__INJECTION_TERM__NAME = hydra.core.Name("injectionTerm")
TERM_ACCESSOR__WRAPPED_TERM__NAME = hydra.core.Name("wrappedTerm")
