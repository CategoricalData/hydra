# Note: this is an automatically generated file. Do not edit.

r"""A model for subterm and subtype access patterns."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class SubtermEdge:
    r"""An edge in a subterm graph, connecting two nodes via a path."""

    source: Annotated[SubtermNode, "The source node of the edge"]
    path: Annotated[SubtermPath, "The subterm path connecting source to target"]
    target: Annotated[SubtermNode, "The target node of the edge"]

    TYPE_ = hydra.core.Name("hydra.paths.SubtermEdge")
    SOURCE = hydra.core.Name("source")
    PATH = hydra.core.Name("path")
    TARGET = hydra.core.Name("target")

@dataclass(frozen=True)
class SubtermGraph:
    r"""A graph of subterm nodes and edges, representing term access patterns."""

    nodes: Annotated[frozenlist[SubtermNode], "All nodes in the graph"]
    edges: Annotated[frozenlist[SubtermEdge], "All edges in the graph"]

    TYPE_ = hydra.core.Name("hydra.paths.SubtermGraph")
    NODES = hydra.core.Name("nodes")
    EDGES = hydra.core.Name("edges")

@dataclass(frozen=True)
class SubtermNode:
    r"""A node in a subterm graph, representing a term or subterm."""

    name: Annotated[hydra.core.Name, "The qualified name of the term"]
    label: Annotated[str, "A human-readable label for the node"]
    id: Annotated[str, "A unique identifier for the node"]

    TYPE_ = hydra.core.Name("hydra.paths.SubtermNode")
    NAME = hydra.core.Name("name")
    LABEL = hydra.core.Name("label")
    ID = hydra.core.Name("id")

class SubtermPath(Node["frozenlist[SubtermStep]"]):
    r"""A sequence of subterm steps forming a path through a term."""

SubtermPath.TYPE_ = hydra.core.Name("hydra.paths.SubtermPath")

class SubtermStepAnnotatedBody:
    r"""Access the body of an annotated term"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtermStepAnnotatedBody)
    def __hash__(self):
        return hash("SubtermStepAnnotatedBody")

class SubtermStepApplicationFunction:
    r"""Access the function of an application term"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtermStepApplicationFunction)
    def __hash__(self):
        return hash("SubtermStepApplicationFunction")

class SubtermStepApplicationArgument:
    r"""Access the argument of an application term"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtermStepApplicationArgument)
    def __hash__(self):
        return hash("SubtermStepApplicationArgument")

class SubtermStepLambdaBody:
    r"""Access the body of a lambda term"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtermStepLambdaBody)
    def __hash__(self):
        return hash("SubtermStepLambdaBody")

class SubtermStepUnionCasesDefault:
    r"""Access the default case of a union elimination"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtermStepUnionCasesDefault)
    def __hash__(self):
        return hash("SubtermStepUnionCasesDefault")

class SubtermStepUnionCasesBranch(Node["hydra.core.Name"]):
    r"""Access a specific branch of a union elimination by field name"""

class SubtermStepLetBody:
    r"""Access the body of a let term"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtermStepLetBody)
    def __hash__(self):
        return hash("SubtermStepLetBody")

class SubtermStepLetBinding(Node["hydra.core.Name"]):
    r"""Access a specific binding in a let term by variable name"""

class SubtermStepListElement(Node[int]):
    r"""Access an element of a list by index"""

class SubtermStepMapKey(Node[int]):
    r"""Access a key in a map by index"""

class SubtermStepMapValue(Node[int]):
    r"""Access a value in a map by index"""

class SubtermStepMaybeTerm:
    r"""Access the term inside a Just value"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtermStepMaybeTerm)
    def __hash__(self):
        return hash("SubtermStepMaybeTerm")

class SubtermStepProductTerm(Node[int]):
    r"""Access an element of a product (tuple) by index"""

class SubtermStepRecordField(Node["hydra.core.Name"]):
    r"""Access a field of a record by field name"""

class SubtermStepSetElement(Node[int]):
    r"""Access an element of a set by index"""

class SubtermStepSumTerm:
    r"""Access the term inside a sum variant"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtermStepSumTerm)
    def __hash__(self):
        return hash("SubtermStepSumTerm")

class SubtermStepTypeLambdaBody:
    r"""Access the body of a type lambda term"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtermStepTypeLambdaBody)
    def __hash__(self):
        return hash("SubtermStepTypeLambdaBody")

class SubtermStepTypeApplicationTerm:
    r"""Access the term being applied to a type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtermStepTypeApplicationTerm)
    def __hash__(self):
        return hash("SubtermStepTypeApplicationTerm")

class SubtermStepInjectionTerm:
    r"""Access the term inside a union injection"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtermStepInjectionTerm)
    def __hash__(self):
        return hash("SubtermStepInjectionTerm")

class SubtermStepWrappedTerm:
    r"""Access the term inside a wrapped term"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtermStepWrappedTerm)
    def __hash__(self):
        return hash("SubtermStepWrappedTerm")

class _SubtermStepMeta(type):
    def __getitem__(cls, item):
        return object

# A function which maps from a term to a particular immediate subterm.
class SubtermStep(metaclass=_SubtermStepMeta):
    r"""SubtermStepAnnotatedBody | SubtermStepApplicationFunction | SubtermStepApplicationArgument | SubtermStepLambdaBody | SubtermStepUnionCasesDefault | SubtermStepUnionCasesBranch | SubtermStepLetBody | SubtermStepLetBinding | SubtermStepListElement | SubtermStepMapKey | SubtermStepMapValue | SubtermStepMaybeTerm | SubtermStepProductTerm | SubtermStepRecordField | SubtermStepSetElement | SubtermStepSumTerm | SubtermStepTypeLambdaBody | SubtermStepTypeApplicationTerm | SubtermStepInjectionTerm | SubtermStepWrappedTerm"""

    TYPE_ = hydra.core.Name("hydra.paths.SubtermStep")
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

@dataclass(frozen=True)
class SubtypeEdge:
    r"""An edge in a subtype graph, connecting two nodes via a path."""

    source: Annotated[SubtypeNode, "The source node of the edge"]
    path: Annotated[SubtypePath, "The subtype path connecting source to target"]
    target: Annotated[SubtypeNode, "The target node of the edge"]

    TYPE_ = hydra.core.Name("hydra.paths.SubtypeEdge")
    SOURCE = hydra.core.Name("source")
    PATH = hydra.core.Name("path")
    TARGET = hydra.core.Name("target")

@dataclass(frozen=True)
class SubtypeGraph:
    r"""A graph of subtype nodes and edges, representing type access patterns."""

    nodes: Annotated[frozenlist[SubtypeNode], "All nodes in the graph"]
    edges: Annotated[frozenlist[SubtypeEdge], "All edges in the graph"]

    TYPE_ = hydra.core.Name("hydra.paths.SubtypeGraph")
    NODES = hydra.core.Name("nodes")
    EDGES = hydra.core.Name("edges")

@dataclass(frozen=True)
class SubtypeNode:
    r"""A node in a subtype graph, representing a type or subtype."""

    name: Annotated[hydra.core.Name, "The qualified name of the type"]
    label: Annotated[str, "A human-readable label for the node"]
    id: Annotated[str, "A unique identifier for the node"]

    TYPE_ = hydra.core.Name("hydra.paths.SubtypeNode")
    NAME = hydra.core.Name("name")
    LABEL = hydra.core.Name("label")
    ID = hydra.core.Name("id")

class SubtypePath(Node["frozenlist[SubtypeStep]"]):
    r"""A sequence of subtype steps forming a path through a type."""

SubtypePath.TYPE_ = hydra.core.Name("hydra.paths.SubtypePath")

class SubtypeStepAnnotatedBody:
    r"""Access the body of an annotated type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtypeStepAnnotatedBody)
    def __hash__(self):
        return hash("SubtypeStepAnnotatedBody")

class SubtypeStepApplicationFunction:
    r"""Access the function of an application type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtypeStepApplicationFunction)
    def __hash__(self):
        return hash("SubtypeStepApplicationFunction")

class SubtypeStepApplicationArgument:
    r"""Access the argument of an application type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtypeStepApplicationArgument)
    def __hash__(self):
        return hash("SubtypeStepApplicationArgument")

class SubtypeStepEitherLeft:
    r"""Access the left type of an either type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtypeStepEitherLeft)
    def __hash__(self):
        return hash("SubtypeStepEitherLeft")

class SubtypeStepEitherRight:
    r"""Access the right type of an either type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtypeStepEitherRight)
    def __hash__(self):
        return hash("SubtypeStepEitherRight")

class SubtypeStepForallBody:
    r"""Access the body of a universally quantified type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtypeStepForallBody)
    def __hash__(self):
        return hash("SubtypeStepForallBody")

class SubtypeStepFunctionDomain:
    r"""Access the domain type of a function type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtypeStepFunctionDomain)
    def __hash__(self):
        return hash("SubtypeStepFunctionDomain")

class SubtypeStepFunctionCodomain:
    r"""Access the codomain type of a function type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtypeStepFunctionCodomain)
    def __hash__(self):
        return hash("SubtypeStepFunctionCodomain")

class SubtypeStepListElement:
    r"""Access the element type of a list type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtypeStepListElement)
    def __hash__(self):
        return hash("SubtypeStepListElement")

class SubtypeStepMapKeys:
    r"""Access the key type of a map type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtypeStepMapKeys)
    def __hash__(self):
        return hash("SubtypeStepMapKeys")

class SubtypeStepMapValues:
    r"""Access the value type of a map type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtypeStepMapValues)
    def __hash__(self):
        return hash("SubtypeStepMapValues")

class SubtypeStepMaybeElement:
    r"""Access the element type of an optional type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtypeStepMaybeElement)
    def __hash__(self):
        return hash("SubtypeStepMaybeElement")

class SubtypeStepPairFirst:
    r"""Access the first type of a pair type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtypeStepPairFirst)
    def __hash__(self):
        return hash("SubtypeStepPairFirst")

class SubtypeStepPairSecond:
    r"""Access the second type of a pair type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtypeStepPairSecond)
    def __hash__(self):
        return hash("SubtypeStepPairSecond")

class SubtypeStepRecordField(Node["hydra.core.Name"]):
    r"""Access a field type of a record type by field name"""

class SubtypeStepSetElement:
    r"""Access the element type of a set type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtypeStepSetElement)
    def __hash__(self):
        return hash("SubtypeStepSetElement")

class SubtypeStepUnionField(Node["hydra.core.Name"]):
    r"""Access a field type of a union type by field name"""

class SubtypeStepWrappedType:
    r"""Access the type inside a wrapped type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubtypeStepWrappedType)
    def __hash__(self):
        return hash("SubtypeStepWrappedType")

class _SubtypeStepMeta(type):
    def __getitem__(cls, item):
        return object

# A function which maps from a type to a particular immediate subtype.
class SubtypeStep(metaclass=_SubtypeStepMeta):
    r"""SubtypeStepAnnotatedBody | SubtypeStepApplicationFunction | SubtypeStepApplicationArgument | SubtypeStepEitherLeft | SubtypeStepEitherRight | SubtypeStepForallBody | SubtypeStepFunctionDomain | SubtypeStepFunctionCodomain | SubtypeStepListElement | SubtypeStepMapKeys | SubtypeStepMapValues | SubtypeStepMaybeElement | SubtypeStepPairFirst | SubtypeStepPairSecond | SubtypeStepRecordField | SubtypeStepSetElement | SubtypeStepUnionField | SubtypeStepWrappedType"""

    TYPE_ = hydra.core.Name("hydra.paths.SubtypeStep")
    ANNOTATED_BODY = hydra.core.Name("annotatedBody")
    APPLICATION_FUNCTION = hydra.core.Name("applicationFunction")
    APPLICATION_ARGUMENT = hydra.core.Name("applicationArgument")
    EITHER_LEFT = hydra.core.Name("eitherLeft")
    EITHER_RIGHT = hydra.core.Name("eitherRight")
    FORALL_BODY = hydra.core.Name("forallBody")
    FUNCTION_DOMAIN = hydra.core.Name("functionDomain")
    FUNCTION_CODOMAIN = hydra.core.Name("functionCodomain")
    LIST_ELEMENT = hydra.core.Name("listElement")
    MAP_KEYS = hydra.core.Name("mapKeys")
    MAP_VALUES = hydra.core.Name("mapValues")
    MAYBE_ELEMENT = hydra.core.Name("maybeElement")
    PAIR_FIRST = hydra.core.Name("pairFirst")
    PAIR_SECOND = hydra.core.Name("pairSecond")
    RECORD_FIELD = hydra.core.Name("recordField")
    SET_ELEMENT = hydra.core.Name("setElement")
    UNION_FIELD = hydra.core.Name("unionField")
    WRAPPED_TYPE = hydra.core.Name("wrappedType")
