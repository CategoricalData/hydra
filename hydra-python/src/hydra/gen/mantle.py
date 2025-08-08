"""A set of types which supplement hydra.gen.core with variants and accessors. Currently contains miscellaneous additional types including CaseConvention and Either."""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import TypeVar

import hydra.gen.core
from hydra.dsl.python import Node, frozenlist

A = TypeVar("A")
B = TypeVar("B")

@dataclass
class AccessorEdge:
    source: AccessorNode
    path: AccessorPath
    target: AccessorNode

ACCESSOR_EDGE__NAME = hydra.gen.core.Name("hydra.mantle.AccessorEdge")
ACCESSOR_EDGE__SOURCE__NAME = hydra.gen.core.Name("source")
ACCESSOR_EDGE__PATH__NAME = hydra.gen.core.Name("path")
ACCESSOR_EDGE__TARGET__NAME = hydra.gen.core.Name("target")

@dataclass
class AccessorGraph:
    nodes: frozenlist[AccessorNode]
    edges: frozenlist[AccessorEdge]

ACCESSOR_GRAPH__NAME = hydra.gen.core.Name("hydra.mantle.AccessorGraph")
ACCESSOR_GRAPH__NODES__NAME = hydra.gen.core.Name("nodes")
ACCESSOR_GRAPH__EDGES__NAME = hydra.gen.core.Name("edges")

@dataclass
class AccessorNode:
    name: hydra.gen.core.Name
    label: str
    id: str

ACCESSOR_NODE__NAME = hydra.gen.core.Name("hydra.mantle.AccessorNode")
ACCESSOR_NODE__NAME__NAME = hydra.gen.core.Name("name")
ACCESSOR_NODE__LABEL__NAME = hydra.gen.core.Name("label")
ACCESSOR_NODE__ID__NAME = hydra.gen.core.Name("id")

class AccessorPath(Node["frozenlist[TermAccessor]"]): ...

ACCESSOR_PATH__NAME = hydra.gen.core.Name("hydra.mantle.AccessorPath")

class CaseConvention(Enum):
    CAMEL = "camel"
    
    PASCAL = "pascal"
    
    LOWER_SNAKE = "lowerSnake"
    
    UPPER_SNAKE = "upperSnake"

CASE_CONVENTION__NAME = hydra.gen.core.Name("hydra.mantle.CaseConvention")
CASE_CONVENTION__CAMEL__NAME = hydra.gen.core.Name("camel")
CASE_CONVENTION__PASCAL__NAME = hydra.gen.core.Name("pascal")
CASE_CONVENTION__LOWER_SNAKE__NAME = hydra.gen.core.Name("lowerSnake")
CASE_CONVENTION__UPPER_SNAKE__NAME = hydra.gen.core.Name("upperSnake")

class EitherLeft(Node["A"]): ...

class EitherRight(Node["B"]): ...

# A disjoint union between a 'left' type and a 'right' type.
type Either[A, B] = EitherLeft[A] | EitherRight[B]

EITHER__NAME = hydra.gen.core.Name("hydra.mantle.Either")
EITHER__LEFT__NAME = hydra.gen.core.Name("left")
EITHER__RIGHT__NAME = hydra.gen.core.Name("right")

class EliminationVariant(Enum):
    """The identifier of an elimination constructor."""
    
    PRODUCT = "product"
    
    RECORD = "record"
    
    UNION = "union"
    
    WRAP = "wrap"

ELIMINATION_VARIANT__NAME = hydra.gen.core.Name("hydra.mantle.EliminationVariant")
ELIMINATION_VARIANT__PRODUCT__NAME = hydra.gen.core.Name("product")
ELIMINATION_VARIANT__RECORD__NAME = hydra.gen.core.Name("record")
ELIMINATION_VARIANT__UNION__NAME = hydra.gen.core.Name("union")
ELIMINATION_VARIANT__WRAP__NAME = hydra.gen.core.Name("wrap")

class FunctionVariant(Enum):
    """The identifier of a function constructor."""
    
    ELIMINATION = "elimination"
    
    LAMBDA = "lambda"
    
    PRIMITIVE = "primitive"

FUNCTION_VARIANT__NAME = hydra.gen.core.Name("hydra.mantle.FunctionVariant")
FUNCTION_VARIANT__ELIMINATION__NAME = hydra.gen.core.Name("elimination")
FUNCTION_VARIANT__LAMBDA__NAME = hydra.gen.core.Name("lambda")
FUNCTION_VARIANT__PRIMITIVE__NAME = hydra.gen.core.Name("primitive")

class LiteralVariant(Enum):
    """The identifier of a literal constructor."""
    
    BINARY = "binary"
    
    BOOLEAN = "boolean"
    
    FLOAT = "float"
    
    INTEGER = "integer"
    
    STRING = "string"

LITERAL_VARIANT__NAME = hydra.gen.core.Name("hydra.mantle.LiteralVariant")
LITERAL_VARIANT__BINARY__NAME = hydra.gen.core.Name("binary")
LITERAL_VARIANT__BOOLEAN__NAME = hydra.gen.core.Name("boolean")
LITERAL_VARIANT__FLOAT__NAME = hydra.gen.core.Name("float")
LITERAL_VARIANT__INTEGER__NAME = hydra.gen.core.Name("integer")
LITERAL_VARIANT__STRING__NAME = hydra.gen.core.Name("string")

class PrecisionArbitrary(Node[None]): ...

class PrecisionBits(Node[int]): ...

# Numeric precision: arbitrary precision, or precision to a specified number of bits.
type Precision = PrecisionArbitrary | PrecisionBits

PRECISION__NAME = hydra.gen.core.Name("hydra.mantle.Precision")
PRECISION__ARBITRARY__NAME = hydra.gen.core.Name("arbitrary")
PRECISION__BITS__NAME = hydra.gen.core.Name("bits")

class TermAccessorAnnotatedSubject(Node[None]): ...

class TermAccessorApplicationFunction(Node[None]): ...

class TermAccessorApplicationArgument(Node[None]): ...

class TermAccessorLambdaBody(Node[None]): ...

class TermAccessorUnionCasesDefault(Node[None]): ...

class TermAccessorUnionCasesBranch(Node[hydra.gen.core.Name]):
    """A unique identifier in some context; a string-valued key."""

class TermAccessorLetEnvironment(Node[None]): ...

class TermAccessorLetBinding(Node[hydra.gen.core.Name]):
    """A unique identifier in some context; a string-valued key."""

class TermAccessorListElement(Node[int]): ...

class TermAccessorMapKey(Node[int]): ...

class TermAccessorMapValue(Node[int]): ...

class TermAccessorOptionalTerm(Node[None]): ...

class TermAccessorProductTerm(Node[int]): ...

class TermAccessorRecordField(Node[hydra.gen.core.Name]):
    """A unique identifier in some context; a string-valued key."""

class TermAccessorSetElement(Node[int]): ...

class TermAccessorSumTerm(Node[None]): ...

class TermAccessorTypeAbstractionBody(Node[None]): ...

class TermAccessorTypeApplicationTerm(Node[None]): ...

class TermAccessorTypedTerm(Node[None]): ...

class TermAccessorInjectionTerm(Node[None]): ...

class TermAccessorWrappedTerm(Node[None]): ...

# A function which maps from a term to a particular immediate subterm.
type TermAccessor = TermAccessorAnnotatedSubject | TermAccessorApplicationFunction | TermAccessorApplicationArgument | TermAccessorLambdaBody | TermAccessorUnionCasesDefault | TermAccessorUnionCasesBranch | TermAccessorLetEnvironment | TermAccessorLetBinding | TermAccessorListElement | TermAccessorMapKey | TermAccessorMapValue | TermAccessorOptionalTerm | TermAccessorProductTerm | TermAccessorRecordField | TermAccessorSetElement | TermAccessorSumTerm | TermAccessorTypeAbstractionBody | TermAccessorTypeApplicationTerm | TermAccessorTypedTerm | TermAccessorInjectionTerm | TermAccessorWrappedTerm

TERM_ACCESSOR__NAME = hydra.gen.core.Name("hydra.mantle.TermAccessor")
TERM_ACCESSOR__ANNOTATED_SUBJECT__NAME = hydra.gen.core.Name("annotatedSubject")
TERM_ACCESSOR__APPLICATION_FUNCTION__NAME = hydra.gen.core.Name("applicationFunction")
TERM_ACCESSOR__APPLICATION_ARGUMENT__NAME = hydra.gen.core.Name("applicationArgument")
TERM_ACCESSOR__LAMBDA_BODY__NAME = hydra.gen.core.Name("lambdaBody")
TERM_ACCESSOR__UNION_CASES_DEFAULT__NAME = hydra.gen.core.Name("unionCasesDefault")
TERM_ACCESSOR__UNION_CASES_BRANCH__NAME = hydra.gen.core.Name("unionCasesBranch")
TERM_ACCESSOR__LET_ENVIRONMENT__NAME = hydra.gen.core.Name("letEnvironment")
TERM_ACCESSOR__LET_BINDING__NAME = hydra.gen.core.Name("letBinding")
TERM_ACCESSOR__LIST_ELEMENT__NAME = hydra.gen.core.Name("listElement")
TERM_ACCESSOR__MAP_KEY__NAME = hydra.gen.core.Name("mapKey")
TERM_ACCESSOR__MAP_VALUE__NAME = hydra.gen.core.Name("mapValue")
TERM_ACCESSOR__OPTIONAL_TERM__NAME = hydra.gen.core.Name("optionalTerm")
TERM_ACCESSOR__PRODUCT_TERM__NAME = hydra.gen.core.Name("productTerm")
TERM_ACCESSOR__RECORD_FIELD__NAME = hydra.gen.core.Name("recordField")
TERM_ACCESSOR__SET_ELEMENT__NAME = hydra.gen.core.Name("setElement")
TERM_ACCESSOR__SUM_TERM__NAME = hydra.gen.core.Name("sumTerm")
TERM_ACCESSOR__TYPE_ABSTRACTION_BODY__NAME = hydra.gen.core.Name("typeAbstractionBody")
TERM_ACCESSOR__TYPE_APPLICATION_TERM__NAME = hydra.gen.core.Name("typeApplicationTerm")
TERM_ACCESSOR__TYPED_TERM__NAME = hydra.gen.core.Name("typedTerm")
TERM_ACCESSOR__INJECTION_TERM__NAME = hydra.gen.core.Name("injectionTerm")
TERM_ACCESSOR__WRAPPED_TERM__NAME = hydra.gen.core.Name("wrappedTerm")

class TermVariant(Enum):
    """The identifier of a term expression constructor."""
    
    ANNOTATED = "annotated"
    
    APPLICATION = "application"
    
    FUNCTION = "function"
    
    LET = "let"
    
    LIST = "list"
    
    LITERAL = "literal"
    
    MAP = "map"
    
    OPTIONAL = "optional"
    
    PRODUCT = "product"
    
    RECORD = "record"
    
    SET = "set"
    
    SUM = "sum"
    
    TYPE_ABSTRACTION = "typeAbstraction"
    
    TYPE_APPLICATION = "typeApplication"
    
    TYPED = "typed"
    
    UNION = "union"
    
    VARIABLE = "variable"
    
    WRAP = "wrap"

TERM_VARIANT__NAME = hydra.gen.core.Name("hydra.mantle.TermVariant")
TERM_VARIANT__ANNOTATED__NAME = hydra.gen.core.Name("annotated")
TERM_VARIANT__APPLICATION__NAME = hydra.gen.core.Name("application")
TERM_VARIANT__FUNCTION__NAME = hydra.gen.core.Name("function")
TERM_VARIANT__LET__NAME = hydra.gen.core.Name("let")
TERM_VARIANT__LIST__NAME = hydra.gen.core.Name("list")
TERM_VARIANT__LITERAL__NAME = hydra.gen.core.Name("literal")
TERM_VARIANT__MAP__NAME = hydra.gen.core.Name("map")
TERM_VARIANT__OPTIONAL__NAME = hydra.gen.core.Name("optional")
TERM_VARIANT__PRODUCT__NAME = hydra.gen.core.Name("product")
TERM_VARIANT__RECORD__NAME = hydra.gen.core.Name("record")
TERM_VARIANT__SET__NAME = hydra.gen.core.Name("set")
TERM_VARIANT__SUM__NAME = hydra.gen.core.Name("sum")
TERM_VARIANT__TYPE_ABSTRACTION__NAME = hydra.gen.core.Name("typeAbstraction")
TERM_VARIANT__TYPE_APPLICATION__NAME = hydra.gen.core.Name("typeApplication")
TERM_VARIANT__TYPED__NAME = hydra.gen.core.Name("typed")
TERM_VARIANT__UNION__NAME = hydra.gen.core.Name("union")
TERM_VARIANT__VARIABLE__NAME = hydra.gen.core.Name("variable")
TERM_VARIANT__WRAP__NAME = hydra.gen.core.Name("wrap")

class TypeVariant(Enum):
    """The identifier of a type constructor."""
    
    ANNOTATED = "annotated"
    
    APPLICATION = "application"
    
    FORALL = "forall"
    
    FUNCTION = "function"
    
    LIST = "list"
    
    LITERAL = "literal"
    
    MAP = "map"
    
    OPTIONAL = "optional"
    
    PRODUCT = "product"
    
    RECORD = "record"
    
    SET = "set"
    
    SUM = "sum"
    
    UNION = "union"
    
    VARIABLE = "variable"
    
    WRAP = "wrap"

TYPE_VARIANT__NAME = hydra.gen.core.Name("hydra.mantle.TypeVariant")
TYPE_VARIANT__ANNOTATED__NAME = hydra.gen.core.Name("annotated")
TYPE_VARIANT__APPLICATION__NAME = hydra.gen.core.Name("application")
TYPE_VARIANT__FORALL__NAME = hydra.gen.core.Name("forall")
TYPE_VARIANT__FUNCTION__NAME = hydra.gen.core.Name("function")
TYPE_VARIANT__LIST__NAME = hydra.gen.core.Name("list")
TYPE_VARIANT__LITERAL__NAME = hydra.gen.core.Name("literal")
TYPE_VARIANT__MAP__NAME = hydra.gen.core.Name("map")
TYPE_VARIANT__OPTIONAL__NAME = hydra.gen.core.Name("optional")
TYPE_VARIANT__PRODUCT__NAME = hydra.gen.core.Name("product")
TYPE_VARIANT__RECORD__NAME = hydra.gen.core.Name("record")
TYPE_VARIANT__SET__NAME = hydra.gen.core.Name("set")
TYPE_VARIANT__SUM__NAME = hydra.gen.core.Name("sum")
TYPE_VARIANT__UNION__NAME = hydra.gen.core.Name("union")
TYPE_VARIANT__VARIABLE__NAME = hydra.gen.core.Name("variable")
TYPE_VARIANT__WRAP__NAME = hydra.gen.core.Name("wrap")
