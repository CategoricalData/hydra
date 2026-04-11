# Note: this is an automatically generated file. Do not edit.

r"""A model for language-agnostic graph pattern queries."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core

class ComparisonConstraint(Enum):
    r"""One of several comparison operators."""

    EQUAL = hydra.core.Name("equal")

    NOT_EQUAL = hydra.core.Name("notEqual")

    LESS_THAN = hydra.core.Name("lessThan")

    GREATER_THAN = hydra.core.Name("greaterThan")

    LESS_THAN_OR_EQUAL = hydra.core.Name("lessThanOrEqual")

    GREATER_THAN_OR_EQUAL = hydra.core.Name("greaterThanOrEqual")

ComparisonConstraint.TYPE_ = hydra.core.Name("hydra.query.ComparisonConstraint")

@dataclass(frozen=True)
class Edge:
    r"""An abstract edge based on a record type."""

    type: Annotated[hydra.core.Name, "The name of a record type, for which the edge also specifies an out- and an in- projection"]
    out: Annotated[Maybe[hydra.core.Name], "The field representing the out-projection of the edge. Defaults to 'out'."]
    in_: Annotated[Maybe[hydra.core.Name], "The field representing the in-projection of the edge. Defaults to 'in'."]

    TYPE_ = hydra.core.Name("hydra.query.Edge")
    TYPE = hydra.core.Name("type")
    OUT = hydra.core.Name("out")
    IN = hydra.core.Name("in")

@dataclass(frozen=True)
class GraphPattern:
    r"""A query pattern which matches within a designated component subgraph."""

    graph: Annotated[hydra.core.Name, "The name of the component graph"]
    patterns: Annotated[frozenlist[Pattern], "The patterns to match within the subgraph"]

    TYPE_ = hydra.core.Name("hydra.query.GraphPattern")
    GRAPH = hydra.core.Name("graph")
    PATTERNS = hydra.core.Name("patterns")

class NodeTerm(Node["hydra.core.Term"]):
    r"""A graph term; an expression which is valid in the graph being matched"""

class NodeVariable(Node["Variable"]):
    r"""A query variable, not to be confused with a variable term"""

class NodeWildcard:
    r"""An anonymous variable which we do not care to join across patterns"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, NodeWildcard)
    def __hash__(self):
        return hash("NodeWildcard")

class _Node_Meta(type):
    def __getitem__(cls, item):
        return object

# A node in a query expression; it may be a term, a variable, or a wildcard.
class Node_(metaclass=_Node_Meta):
    r"""NodeTerm | NodeVariable | NodeWildcard"""

    TYPE_ = hydra.core.Name("hydra.query.Node")
    TERM = hydra.core.Name("term")
    VARIABLE = hydra.core.Name("variable")
    WILDCARD = hydra.core.Name("wildcard")

class PathStep(Node["Step"]):
    r"""A path given by a single step"""

class PathRegex(Node["RegexSequence"]):
    r"""A path given by a regular expression quantifier applied to another path"""

class PathInverse(Node["Path"]):
    r"""A path given by the inverse of another path"""

class _PathMeta(type):
    def __getitem__(cls, item):
        return object

# A query path.
class Path(metaclass=_PathMeta):
    r"""PathStep | PathRegex | PathInverse"""

    TYPE_ = hydra.core.Name("hydra.query.Path")
    STEP = hydra.core.Name("step")
    REGEX = hydra.core.Name("regex")
    INVERSE = hydra.core.Name("inverse")

@dataclass(frozen=True)
class PathEquation:
    r"""A declared equivalence between two abstract paths in a graph."""

    left: Annotated[Path, "The left-hand side of the equation"]
    right: Annotated[Path, "The right-hand side of the equation"]

    TYPE_ = hydra.core.Name("hydra.query.PathEquation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class PatternTriple(Node["TriplePattern"]):
    r"""A subject/predicate/object pattern"""

class PatternNegation(Node["Pattern"]):
    r"""The negation of another pattern"""

class PatternConjunction(Node["frozenlist[Pattern]"]):
    r"""The conjunction ('and') of several other patterns"""

class PatternDisjunction(Node["frozenlist[Pattern]"]):
    r"""The disjunction (inclusive 'or') of several other patterns"""

class PatternGraph(Node["GraphPattern"]):
    r"""A pattern which matches within a named subgraph"""

class _PatternMeta(type):
    def __getitem__(cls, item):
        return object

# A query pattern.
class Pattern(metaclass=_PatternMeta):
    r"""PatternTriple | PatternNegation | PatternConjunction | PatternDisjunction | PatternGraph"""

    TYPE_ = hydra.core.Name("hydra.query.Pattern")
    TRIPLE = hydra.core.Name("triple")
    NEGATION = hydra.core.Name("negation")
    CONJUNCTION = hydra.core.Name("conjunction")
    DISJUNCTION = hydra.core.Name("disjunction")
    GRAPH = hydra.core.Name("graph")

@dataclass(frozen=True)
class PatternImplication:
    r"""A pattern which, if it matches in a given graph, implies that another pattern must also match. Query variables are shared between the two patterns."""

    antecedent: Annotated[Pattern, "The pattern which, if it matches, triggers the constraint"]
    consequent: Annotated[Pattern, "The pattern which must also match when the antecedent matches"]

    TYPE_ = hydra.core.Name("hydra.query.PatternImplication")
    ANTECEDENT = hydra.core.Name("antecedent")
    CONSEQUENT = hydra.core.Name("consequent")

@dataclass(frozen=True)
class Query:
    r"""A SELECT-style graph pattern matching query."""

    variables: Annotated[frozenlist[Variable], "The variables selected by the query"]
    patterns: Annotated[frozenlist[Pattern], "The patterns to be matched"]

    TYPE_ = hydra.core.Name("hydra.query.Query")
    VARIABLES = hydra.core.Name("variables")
    PATTERNS = hydra.core.Name("patterns")

@dataclass(frozen=True)
class Range:
    r"""A range from min to max, inclusive."""

    min: Annotated[int, "The minimum value (inclusive)"]
    max: Annotated[int, "The maximum value (inclusive)"]

    TYPE_ = hydra.core.Name("hydra.query.Range")
    MIN = hydra.core.Name("min")
    MAX = hydra.core.Name("max")

class RegexQuantifierOne:
    r"""No quantifier; matches a single occurrence"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, RegexQuantifierOne)
    def __hash__(self):
        return hash("RegexQuantifierOne")

class RegexQuantifierZeroOrOne:
    r"""The ? quanifier; matches zero or one occurrence"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, RegexQuantifierZeroOrOne)
    def __hash__(self):
        return hash("RegexQuantifierZeroOrOne")

class RegexQuantifierZeroOrMore:
    r"""The * quantifier; matches any number of occurrences"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, RegexQuantifierZeroOrMore)
    def __hash__(self):
        return hash("RegexQuantifierZeroOrMore")

class RegexQuantifierOneOrMore:
    r"""The + quantifier; matches one or more occurrences"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, RegexQuantifierOneOrMore)
    def __hash__(self):
        return hash("RegexQuantifierOneOrMore")

class RegexQuantifierExactly(Node[int]):
    r"""The {n} quantifier; matches exactly n occurrences"""

class RegexQuantifierAtLeast(Node[int]):
    r"""The {n,} quantifier; matches at least n occurrences"""

class RegexQuantifierRange(Node["Range"]):
    r"""The {n, m} quantifier; matches between n and m (inclusive) occurrences"""

class _RegexQuantifierMeta(type):
    def __getitem__(cls, item):
        return object

# A regular expression quantifier.
class RegexQuantifier(metaclass=_RegexQuantifierMeta):
    r"""RegexQuantifierOne | RegexQuantifierZeroOrOne | RegexQuantifierZeroOrMore | RegexQuantifierOneOrMore | RegexQuantifierExactly | RegexQuantifierAtLeast | RegexQuantifierRange"""

    TYPE_ = hydra.core.Name("hydra.query.RegexQuantifier")
    ONE = hydra.core.Name("one")
    ZERO_OR_ONE = hydra.core.Name("zeroOrOne")
    ZERO_OR_MORE = hydra.core.Name("zeroOrMore")
    ONE_OR_MORE = hydra.core.Name("oneOrMore")
    EXACTLY = hydra.core.Name("exactly")
    AT_LEAST = hydra.core.Name("atLeast")
    RANGE = hydra.core.Name("range")

@dataclass(frozen=True)
class RegexSequence:
    r"""A path with a regex quantifier."""

    path: Annotated[Path, "The path to which the quantifier applies"]
    quantifier: Annotated[RegexQuantifier, "The quantifier"]

    TYPE_ = hydra.core.Name("hydra.query.RegexSequence")
    PATH = hydra.core.Name("path")
    QUANTIFIER = hydra.core.Name("quantifier")

class StepEdge(Node["Edge"]):
    r"""An out-to-in traversal of an abstract edge"""

class StepProject(Node["hydra.core.Projection"]):
    r"""A projection from a record through one of its fields"""

class StepCompare(Node["ComparisonConstraint"]):
    r"""A comparison of two terms"""

class _StepMeta(type):
    def __getitem__(cls, item):
        return object

# An atomic function as part of a query. When applied to a graph, steps are typed by function types.
class Step(metaclass=_StepMeta):
    r"""StepEdge | StepProject | StepCompare"""

    TYPE_ = hydra.core.Name("hydra.query.Step")
    EDGE = hydra.core.Name("edge")
    PROJECT = hydra.core.Name("project")
    COMPARE = hydra.core.Name("compare")

@dataclass(frozen=True)
class TriplePattern:
    r"""A subject/predicate/object pattern."""

    subject: Annotated[Node_, "The subject of the pattern"]
    predicate: Annotated[Path, "The predicate (property) of the pattern"]
    object: Annotated[Node_, "The object of the pattern"]

    TYPE_ = hydra.core.Name("hydra.query.TriplePattern")
    SUBJECT = hydra.core.Name("subject")
    PREDICATE = hydra.core.Name("predicate")
    OBJECT = hydra.core.Name("object")

class Variable(Node[str]):
    r"""A query variable."""

Variable.TYPE_ = hydra.core.Name("hydra.query.Variable")
