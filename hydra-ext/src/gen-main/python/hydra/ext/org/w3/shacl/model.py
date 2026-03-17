# Note: this is an automatically generated file. Do not edit.

r"""A SHACL syntax model. See https://www.w3.org/TR/shacl."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar, cast
import hydra.core
import hydra.ext.org.w3.rdf.syntax

A = TypeVar("A")

@dataclass(frozen=True)
class Closed:
    r"""See https://www.w3.org/TR/shacl/#ClosedPatterConstraintComponent."""
    
    is_closed: bool
    ignored_properties: Maybe[frozenset[hydra.ext.org.w3.rdf.syntax.Property]]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.shacl.model.Closed")
    IS_CLOSED = hydra.core.Name("isClosed")
    IGNORED_PROPERTIES = hydra.core.Name("ignoredProperties")

class CommonConstraintAnd(Node["frozenset[Reference[Shape]]"]):
    r"""See https://www.w3.org/TR/shacl/#AndConstraintComponent"""

class CommonConstraintClosed(Node["Closed"]):
    r"""See https://www.w3.org/TR/shacl/#ClosedConstraintComponent"""

class CommonConstraintClass(Node["frozenset[hydra.ext.org.w3.rdf.syntax.RdfsClass]"]):
    r"""See https://www.w3.org/TR/shacl/#ClassConstraintComponent"""

class CommonConstraintDatatype(Node["hydra.ext.org.w3.rdf.syntax.Iri"]):
    r"""See https://www.w3.org/TR/shacl/#DatatypeConstraintComponent"""

class CommonConstraintDisjoint(Node["frozenset[hydra.ext.org.w3.rdf.syntax.Property]"]):
    r"""See https://www.w3.org/TR/shacl/#DisjointConstraintComponent"""

class CommonConstraintEquals(Node["frozenset[hydra.ext.org.w3.rdf.syntax.Property]"]):
    r"""See https://www.w3.org/TR/shacl/#EqualsConstraintComponent"""

class CommonConstraintHasValue(Node["frozenset[hydra.ext.org.w3.rdf.syntax.Node_]"]):
    r"""Specifies the condition that at least one value node is equal to the given RDF term. See https://www.w3.org/TR/shacl/#HasValueConstraintComponent"""

class CommonConstraintIn(Node["frozenlist[hydra.ext.org.w3.rdf.syntax.Node_]"]):
    r"""Specifies the condition that each value node is a member of a provided SHACL list. See https://www.w3.org/TR/shacl/#InConstraintComponent"""

class CommonConstraintLanguageIn(Node["frozenset[hydra.ext.org.w3.rdf.syntax.LanguageTag]"]):
    r"""See https://www.w3.org/TR/shacl/#LanguageInConstraintComponent"""

class CommonConstraintNodeKind(Node["NodeKind"]):
    r"""See https://www.w3.org/TR/shacl/#NodeKindConstraintComponent"""

class CommonConstraintNode(Node["frozenset[Reference[NodeShape]]"]):
    r"""See https://www.w3.org/TR/shacl/#NodeConstraintComponent"""

class CommonConstraintNot(Node["frozenset[Reference[Shape]]"]):
    r"""See https://www.w3.org/TR/shacl/#NotConstraintComponent"""

class CommonConstraintMaxExclusive(Node["hydra.ext.org.w3.rdf.syntax.Literal"]):
    r"""See https://www.w3.org/TR/shacl/#MaxExclusiveConstraintComponent"""

class CommonConstraintMaxInclusive(Node["hydra.ext.org.w3.rdf.syntax.Literal"]):
    r"""See https://www.w3.org/TR/shacl/#MaxInclusiveConstraintComponent"""

class CommonConstraintMaxLength(Node[int]):
    r"""See https://www.w3.org/TR/shacl/#MaxLengthConstraintComponent"""

class CommonConstraintMinExclusive(Node["hydra.ext.org.w3.rdf.syntax.Literal"]):
    r"""See https://www.w3.org/TR/shacl/#MinExclusiveConstraintComponent"""

class CommonConstraintMinInclusive(Node["hydra.ext.org.w3.rdf.syntax.Literal"]):
    r"""See https://www.w3.org/TR/shacl/#MinInclusiveConstraintComponent"""

class CommonConstraintMinLength(Node[int]):
    r"""See https://www.w3.org/TR/shacl/#MinLengthConstraintComponent"""

class CommonConstraintPattern(Node["Pattern"]):
    r"""See https://www.w3.org/TR/shacl/#PatternConstraintComponent"""

class CommonConstraintProperty(Node["frozenset[Reference[PropertyShape]]"]):
    r"""See https://www.w3.org/TR/shacl/#PropertyConstraintComponent"""

class CommonConstraintOr(Node["frozenset[Reference[Shape]]"]):
    r"""See https://www.w3.org/TR/shacl/#OrConstraintComponent"""

class CommonConstraintXone(Node["frozenset[Reference[Shape]]"]):
    r"""See https://www.w3.org/TR/shacl/#XoneConstraintComponent"""

class _CommonConstraintMeta(type):
    def __getitem__(cls, item):
        return object

# Any of a number of constraint parameters which can be applied either to node or property shapes.
class CommonConstraint(metaclass=_CommonConstraintMeta):
    r"""CommonConstraintAnd | CommonConstraintClosed | CommonConstraintClass | CommonConstraintDatatype | CommonConstraintDisjoint | CommonConstraintEquals | CommonConstraintHasValue | CommonConstraintIn | CommonConstraintLanguageIn | CommonConstraintNodeKind | CommonConstraintNode | CommonConstraintNot | CommonConstraintMaxExclusive | CommonConstraintMaxInclusive | CommonConstraintMaxLength | CommonConstraintMinExclusive | CommonConstraintMinInclusive | CommonConstraintMinLength | CommonConstraintPattern | CommonConstraintProperty | CommonConstraintOr | CommonConstraintXone"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.shacl.model.CommonConstraint")
    AND = hydra.core.Name("and")
    CLOSED = hydra.core.Name("closed")
    CLASS = hydra.core.Name("class")
    DATATYPE = hydra.core.Name("datatype")
    DISJOINT = hydra.core.Name("disjoint")
    EQUALS = hydra.core.Name("equals")
    HAS_VALUE = hydra.core.Name("hasValue")
    IN = hydra.core.Name("in")
    LANGUAGE_IN = hydra.core.Name("languageIn")
    NODE_KIND = hydra.core.Name("nodeKind")
    NODE = hydra.core.Name("node")
    NOT = hydra.core.Name("not")
    MAX_EXCLUSIVE = hydra.core.Name("maxExclusive")
    MAX_INCLUSIVE = hydra.core.Name("maxInclusive")
    MAX_LENGTH = hydra.core.Name("maxLength")
    MIN_EXCLUSIVE = hydra.core.Name("minExclusive")
    MIN_INCLUSIVE = hydra.core.Name("minInclusive")
    MIN_LENGTH = hydra.core.Name("minLength")
    PATTERN = hydra.core.Name("pattern")
    PROPERTY = hydra.core.Name("property")
    OR = hydra.core.Name("or")
    XONE = hydra.core.Name("xone")

@dataclass(frozen=True)
class CommonProperties:
    r"""Common constraint parameters and other properties for SHACL shapes."""
    
    constraints: Annotated[frozenset[CommonConstraint], "Common constraint parameters attached to this shape"]
    deactivated: Annotated[Maybe[bool], "See https://www.w3.org/TR/shacl/#deactivated"]
    message: Annotated[hydra.ext.org.w3.rdf.syntax.LangStrings, "See https://www.w3.org/TR/shacl/#message"]
    severity: Annotated[Severity, "See https://www.w3.org/TR/shacl/#severity"]
    target_class: Annotated[frozenset[hydra.ext.org.w3.rdf.syntax.RdfsClass], "See https://www.w3.org/TR/shacl/#targetClass"]
    target_node: Annotated[frozenset[hydra.ext.org.w3.rdf.syntax.IriOrLiteral], "See https://www.w3.org/TR/shacl/#targetNode"]
    target_objects_of: Annotated[frozenset[hydra.ext.org.w3.rdf.syntax.Property], "See https://www.w3.org/TR/shacl/#targetObjectsOf"]
    target_subjects_of: Annotated[frozenset[hydra.ext.org.w3.rdf.syntax.Property], "See https://www.w3.org/TR/shacl/#targetSubjectsOf"]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.shacl.model.CommonProperties")
    CONSTRAINTS = hydra.core.Name("constraints")
    DEACTIVATED = hydra.core.Name("deactivated")
    MESSAGE = hydra.core.Name("message")
    SEVERITY = hydra.core.Name("severity")
    TARGET_CLASS = hydra.core.Name("targetClass")
    TARGET_NODE = hydra.core.Name("targetNode")
    TARGET_OBJECTS_OF = hydra.core.Name("targetObjectsOf")
    TARGET_SUBJECTS_OF = hydra.core.Name("targetSubjectsOf")

@dataclass(frozen=True)
class Definition(Generic[A]):
    r"""An instance of a type like sh:Shape or sh:NodeShape, together with a unique IRI for that instance."""
    
    iri: hydra.ext.org.w3.rdf.syntax.Iri
    target: A
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.shacl.model.Definition")
    IRI = hydra.core.Name("iri")
    TARGET = hydra.core.Name("target")

class NodeKind(Enum):
    BLANK_NODE = hydra.core.Name("blankNode")
    r"""A blank node"""
    
    IRI = hydra.core.Name("iri")
    r"""An IRI"""
    
    LITERAL = hydra.core.Name("literal")
    r"""A literal"""
    
    BLANK_NODE_OR_IRI = hydra.core.Name("blankNodeOrIri")
    r"""A blank node or an IRI"""
    
    BLANK_NODE_OR_LITERAL = hydra.core.Name("blankNodeOrLiteral")
    r"""A blank node or a literal"""
    
    IRI_OR_LITERAL = hydra.core.Name("iriOrLiteral")
    r"""An IRI or a literal"""

NodeKind.TYPE_ = hydra.core.Name("hydra.ext.org.w3.shacl.model.NodeKind")

@dataclass(frozen=True)
class NodeShape:
    r"""A SHACL node shape. See https://www.w3.org/TR/shacl/#node-shapes."""
    
    common: CommonProperties
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.shacl.model.NodeShape")
    COMMON = hydra.core.Name("common")

@dataclass(frozen=True)
class Pattern:
    r"""A SHACL pattern. See https://www.w3.org/TR/shacl/#PatternConstraintComponent."""
    
    regex: str
    flags: Maybe[str]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.shacl.model.Pattern")
    REGEX = hydra.core.Name("regex")
    FLAGS = hydra.core.Name("flags")

@dataclass(frozen=True)
class PropertyShape:
    r"""A SHACL property shape. See https://www.w3.org/TR/shacl/#property-shapes."""
    
    common: CommonProperties
    constraints: Annotated[frozenset[PropertyShapeConstraint], "Any property shape -specific constraint parameters"]
    default_value: Annotated[Maybe[hydra.ext.org.w3.rdf.syntax.Node_], "See https://www.w3.org/TR/shacl/#defaultValue"]
    description: Annotated[hydra.ext.org.w3.rdf.syntax.LangStrings, "See https://www.w3.org/TR/shacl/#name"]
    name: Annotated[hydra.ext.org.w3.rdf.syntax.LangStrings, "See https://www.w3.org/TR/shacl/#name"]
    order: Annotated[Maybe[int], "See https://www.w3.org/TR/shacl/#order"]
    path: hydra.ext.org.w3.rdf.syntax.Iri
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.shacl.model.PropertyShape")
    COMMON = hydra.core.Name("common")
    CONSTRAINTS = hydra.core.Name("constraints")
    DEFAULT_VALUE = hydra.core.Name("defaultValue")
    DESCRIPTION = hydra.core.Name("description")
    NAME = hydra.core.Name("name")
    ORDER = hydra.core.Name("order")
    PATH = hydra.core.Name("path")

class PropertyShapeConstraintLessThan(Node["frozenset[hydra.ext.org.w3.rdf.syntax.Property]"]):
    r"""See https://www.w3.org/TR/shacl/#LessThanConstraintComponent"""

class PropertyShapeConstraintLessThanOrEquals(Node["frozenset[hydra.ext.org.w3.rdf.syntax.Property]"]):
    r"""See https://www.w3.org/TR/shacl/#LessThanOrEqualsConstraintComponent"""

class PropertyShapeConstraintMaxCount(Node[int]):
    r"""The maximum cardinality. Node shapes cannot have any value for sh:maxCount. See https://www.w3.org/TR/shacl/#MaxCountConstraintComponent"""

class PropertyShapeConstraintMinCount(Node[int]):
    r"""The minimum cardinality. Node shapes cannot have any value for sh:minCount. See https://www.w3.org/TR/shacl/#MinCountConstraintComponent"""

class PropertyShapeConstraintUniqueLang(Node[bool]):
    r"""See https://www.w3.org/TR/shacl/#UniqueLangConstraintComponent"""

class PropertyShapeConstraintQualifiedValueShape(Node["QualifiedValueShape"]):
    r"""See https://www.w3.org/TR/shacl/#QualifiedValueShapeConstraintComponent"""

class _PropertyShapeConstraintMeta(type):
    def __getitem__(cls, item):
        return object

# A number of constraint parameters which are specific to property shapes, and cannot be applied to node shapes.
class PropertyShapeConstraint(metaclass=_PropertyShapeConstraintMeta):
    r"""PropertyShapeConstraintLessThan | PropertyShapeConstraintLessThanOrEquals | PropertyShapeConstraintMaxCount | PropertyShapeConstraintMinCount | PropertyShapeConstraintUniqueLang | PropertyShapeConstraintQualifiedValueShape"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.shacl.model.PropertyShapeConstraint")
    LESS_THAN = hydra.core.Name("lessThan")
    LESS_THAN_OR_EQUALS = hydra.core.Name("lessThanOrEquals")
    MAX_COUNT = hydra.core.Name("maxCount")
    MIN_COUNT = hydra.core.Name("minCount")
    UNIQUE_LANG = hydra.core.Name("uniqueLang")
    QUALIFIED_VALUE_SHAPE = hydra.core.Name("qualifiedValueShape")

@dataclass(frozen=True)
class QualifiedValueShape:
    r"""See https://www.w3.org/TR/shacl/#QualifiedValueShapeConstraintComponent."""
    
    qualified_value_shape: Reference[Shape]
    qualified_max_count: int
    qualified_min_count: int
    qualified_value_shapes_disjoint: Maybe[bool]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.shacl.model.QualifiedValueShape")
    QUALIFIED_VALUE_SHAPE = hydra.core.Name("qualifiedValueShape")
    QUALIFIED_MAX_COUNT = hydra.core.Name("qualifiedMaxCount")
    QUALIFIED_MIN_COUNT = hydra.core.Name("qualifiedMinCount")
    QUALIFIED_VALUE_SHAPES_DISJOINT = hydra.core.Name("qualifiedValueShapesDisjoint")

class ReferenceNamed(Node["hydra.ext.org.w3.rdf.syntax.Iri"]):
    ...

class ReferenceAnonymous(Node["A"]):
    r"""An anonymous instance"""

class ReferenceDefinition(Node["Definition[A]"]):
    r"""An inline definition"""

class _ReferenceMeta(type):
    def __getitem__(cls, item):
        return object

# Either an instance of a type like sh:Shape or sh:NodeShape, or an IRI which refers to an instance of that type.
class Reference(metaclass=_ReferenceMeta):
    r"""ReferenceNamed | ReferenceAnonymous[A] | ReferenceDefinition[A]"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.shacl.model.Reference")
    NAMED = hydra.core.Name("named")
    ANONYMOUS = hydra.core.Name("anonymous")
    DEFINITION = hydra.core.Name("definition")

class Severity(Enum):
    INFO = hydra.core.Name("info")
    r"""A non-critical constraint violation indicating an informative message"""
    
    WARNING = hydra.core.Name("warning")
    r"""A non-critical constraint violation indicating a warning"""
    
    VIOLATION = hydra.core.Name("violation")
    r"""A constraint violation"""

Severity.TYPE_ = hydra.core.Name("hydra.ext.org.w3.shacl.model.Severity")

class ShapeNode(Node["NodeShape"]):
    ...

class ShapeProperty(Node["PropertyShape"]):
    ...

class _ShapeMeta(type):
    def __getitem__(cls, item):
        return object

# A SHACL node or property shape. See https://www.w3.org/TR/shacl/#shapes.
class Shape(metaclass=_ShapeMeta):
    r"""ShapeNode | ShapeProperty"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.shacl.model.Shape")
    NODE = hydra.core.Name("node")
    PROPERTY = hydra.core.Name("property")

class ShapesGraph(Node["frozenset[Definition[Shape]]"]):
    r"""An RDF graph containing zero or more shapes that is passed into a SHACL validation process so that a data graph can be validated against the shapes."""

ShapesGraph.TYPE_ = hydra.core.Name("hydra.ext.org.w3.shacl.model.ShapesGraph")
