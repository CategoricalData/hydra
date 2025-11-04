# Note: this is an automatically generated file. Do not edit.

r"""A model for term access patterns."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import Node, frozenlist
import hydra.core

@dataclass
class AccessorEdge:
    source: AccessorNode
    path: AccessorPath
    target: AccessorNode

ACCESSOR_EDGE__NAME = hydra.core.Name("hydra.accessors.AccessorEdge")
ACCESSOR_EDGE__SOURCE__NAME = hydra.core.Name("source")
ACCESSOR_EDGE__PATH__NAME = hydra.core.Name("path")
ACCESSOR_EDGE__TARGET__NAME = hydra.core.Name("target")

@dataclass
class AccessorGraph:
    nodes: frozenlist[AccessorNode]
    edges: frozenlist[AccessorEdge]

ACCESSOR_GRAPH__NAME = hydra.core.Name("hydra.accessors.AccessorGraph")
ACCESSOR_GRAPH__NODES__NAME = hydra.core.Name("nodes")
ACCESSOR_GRAPH__EDGES__NAME = hydra.core.Name("edges")

@dataclass
class AccessorNode:
    name: hydra.core.Name
    label: str
    id: str

ACCESSOR_NODE__NAME = hydra.core.Name("hydra.accessors.AccessorNode")
ACCESSOR_NODE__NAME__NAME = hydra.core.Name("name")
ACCESSOR_NODE__LABEL__NAME = hydra.core.Name("label")
ACCESSOR_NODE__ID__NAME = hydra.core.Name("id")

class AccessorPath(Node["frozenlist[TermAccessor]"]): ...

ACCESSOR_PATH__NAME = hydra.core.Name("hydra.accessors.AccessorPath")

class TermAccessorAnnotatedBody(Node[None]): ...

class TermAccessorApplicationFunction(Node[None]): ...

class TermAccessorApplicationArgument(Node[None]): ...

class TermAccessorLambdaBody(Node[None]): ...

class TermAccessorUnionCasesDefault(Node[None]): ...

class TermAccessorUnionCasesBranch(Node["hydra.core.Name"]): ...

class TermAccessorLetBody(Node[None]): ...

class TermAccessorLetBinding(Node["hydra.core.Name"]): ...

class TermAccessorListElement(Node[int]): ...

class TermAccessorMapKey(Node[int]): ...

class TermAccessorMapValue(Node[int]): ...

class TermAccessorMaybeTerm(Node[None]): ...

class TermAccessorProductTerm(Node[int]): ...

class TermAccessorRecordField(Node["hydra.core.Name"]): ...

class TermAccessorSetElement(Node[int]): ...

class TermAccessorSumTerm(Node[None]): ...

class TermAccessorTypeLambdaBody(Node[None]): ...

class TermAccessorTypeApplicationTerm(Node[None]): ...

class TermAccessorInjectionTerm(Node[None]): ...

class TermAccessorWrappedTerm(Node[None]): ...

# A function which maps from a term to a particular immediate subterm.
type TermAccessor = TermAccessorAnnotatedBody | TermAccessorApplicationFunction | TermAccessorApplicationArgument | TermAccessorLambdaBody | TermAccessorUnionCasesDefault | TermAccessorUnionCasesBranch | TermAccessorLetBody | TermAccessorLetBinding | TermAccessorListElement | TermAccessorMapKey | TermAccessorMapValue | TermAccessorMaybeTerm | TermAccessorProductTerm | TermAccessorRecordField | TermAccessorSetElement | TermAccessorSumTerm | TermAccessorTypeLambdaBody | TermAccessorTypeApplicationTerm | TermAccessorInjectionTerm | TermAccessorWrappedTerm

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
