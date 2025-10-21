# Note: this is an automatically generated file. Do not edit.

"""A model for language-agnostic graph pattern queries."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated
import hydra.core

class ComparisonConstraint(Enum):
    """One of several comparison operators."""
    
    EQUAL = "equal"
    
    NOT_EQUAL = "notEqual"
    
    LESS_THAN = "lessThan"
    
    GREATER_THAN = "greaterThan"
    
    LESS_THAN_OR_EQUAL = "lessThanOrEqual"
    
    GREATER_THAN_OR_EQUAL = "greaterThanOrEqual"

COMPARISON_CONSTRAINT__NAME = hydra.core.Name("hydra.query.ComparisonConstraint")
COMPARISON_CONSTRAINT__EQUAL__NAME = hydra.core.Name("equal")
COMPARISON_CONSTRAINT__NOT_EQUAL__NAME = hydra.core.Name("notEqual")
COMPARISON_CONSTRAINT__LESS_THAN__NAME = hydra.core.Name("lessThan")
COMPARISON_CONSTRAINT__GREATER_THAN__NAME = hydra.core.Name("greaterThan")
COMPARISON_CONSTRAINT__LESS_THAN_OR_EQUAL__NAME = hydra.core.Name("lessThanOrEqual")
COMPARISON_CONSTRAINT__GREATER_THAN_OR_EQUAL__NAME = hydra.core.Name("greaterThanOrEqual")

@dataclass
class Edge:
    """An abstract edge based on a record type."""
    
    type: Annotated[hydra.core.Name, "The name of a record type, for which the edge also specifies an out- and an in- projection"]
    out: Annotated[Maybe[hydra.core.Name], "The field representing the out-projection of the edge. Defaults to 'out'."]
    in_: Annotated[Maybe[hydra.core.Name], "The field representing the in-projection of the edge. Defaults to 'in'."]

EDGE__NAME = hydra.core.Name("hydra.query.Edge")
EDGE__TYPE__NAME = hydra.core.Name("type")
EDGE__OUT__NAME = hydra.core.Name("out")
EDGE__IN__NAME = hydra.core.Name("in")

@dataclass
class GraphPattern:
    """A query pattern which matches within a designated component subgraph."""
    
    graph: Annotated[hydra.core.Name, "The name of the component graph"]
    patterns: Annotated[frozenlist[Pattern], "The patterns to match within the subgraph"]

GRAPH_PATTERN__NAME = hydra.core.Name("hydra.query.GraphPattern")
GRAPH_PATTERN__GRAPH__NAME = hydra.core.Name("graph")
GRAPH_PATTERN__PATTERNS__NAME = hydra.core.Name("patterns")

class NodeTerm(Node["hydra.core.Term"]):
    """A graph term; an expression which is valid in the graph being matched."""

class NodeVariable(Node["Variable"]):
    """A query variable, not to be confused with a variable term."""

class NodeWildcard(Node[None]):
    """An anonymous variable which we do not care to join across patterns."""

# A node in a query expression; it may be a term, a variable, or a wildcard.
type Node_ = NodeTerm | NodeVariable | NodeWildcard

NODE__NAME = hydra.core.Name("hydra.query.Node")
NODE__TERM__NAME = hydra.core.Name("term")
NODE__VARIABLE__NAME = hydra.core.Name("variable")
NODE__WILDCARD__NAME = hydra.core.Name("wildcard")

class PathStep(Node["Step"]):
    """A path given by a single step."""

class PathRegex(Node["RegexSequence"]):
    """A path given by a regular expression quantifier applied to another path."""

class PathInverse(Node["Path"]):
    """A path given by the inverse of another path."""

# A query path.
type Path = PathStep | PathRegex | PathInverse

PATH__NAME = hydra.core.Name("hydra.query.Path")
PATH__STEP__NAME = hydra.core.Name("step")
PATH__REGEX__NAME = hydra.core.Name("regex")
PATH__INVERSE__NAME = hydra.core.Name("inverse")

class PatternTriple(Node["TriplePattern"]):
    """A subject/predicate/object pattern."""

class PatternNegation(Node["Pattern"]):
    """The negation of another pattern."""

class PatternConjunction(Node["frozenlist[Pattern]"]):
    """The conjunction ('and') of several other patterns."""

class PatternDisjunction(Node["frozenlist[Pattern]"]):
    """The disjunction (inclusive 'or') of several other patterns."""

class PatternGraph(Node["GraphPattern"]):
    """A pattern which matches within a named subgraph."""

# A query pattern.
type Pattern = PatternTriple | PatternNegation | PatternConjunction | PatternDisjunction | PatternGraph

PATTERN__NAME = hydra.core.Name("hydra.query.Pattern")
PATTERN__TRIPLE__NAME = hydra.core.Name("triple")
PATTERN__NEGATION__NAME = hydra.core.Name("negation")
PATTERN__CONJUNCTION__NAME = hydra.core.Name("conjunction")
PATTERN__DISJUNCTION__NAME = hydra.core.Name("disjunction")
PATTERN__GRAPH__NAME = hydra.core.Name("graph")

@dataclass
class Query:
    """A SELECT-style graph pattern matching query."""
    
    variables: Annotated[frozenlist[Variable], "The variables selected by the query"]
    patterns: Annotated[frozenlist[Pattern], "The patterns to be matched"]

QUERY__NAME = hydra.core.Name("hydra.query.Query")
QUERY__VARIABLES__NAME = hydra.core.Name("variables")
QUERY__PATTERNS__NAME = hydra.core.Name("patterns")

@dataclass
class Range:
    """A range from min to max, inclusive."""
    
    min: int
    max: int

RANGE__NAME = hydra.core.Name("hydra.query.Range")
RANGE__MIN__NAME = hydra.core.Name("min")
RANGE__MAX__NAME = hydra.core.Name("max")

class RegexQuantifierOne(Node[None]):
    """No quantifier; matches a single occurrence."""

class RegexQuantifierZeroOrOne(Node[None]):
    """The ? quanifier; matches zero or one occurrence."""

class RegexQuantifierZeroOrMore(Node[None]):
    """The * quantifier; matches any number of occurrences."""

class RegexQuantifierOneOrMore(Node[None]):
    """The + quantifier; matches one or more occurrences."""

class RegexQuantifierExactly(Node[int]):
    """The {n} quantifier; matches exactly n occurrences."""

class RegexQuantifierAtLeast(Node[int]):
    """The {n,} quantifier; matches at least n occurrences."""

class RegexQuantifierRange(Node["Range"]):
    """The {n, m} quantifier; matches between n and m (inclusive) occurrences."""

# A regular expression quantifier.
type RegexQuantifier = RegexQuantifierOne | RegexQuantifierZeroOrOne | RegexQuantifierZeroOrMore | RegexQuantifierOneOrMore | RegexQuantifierExactly | RegexQuantifierAtLeast | RegexQuantifierRange

REGEX_QUANTIFIER__NAME = hydra.core.Name("hydra.query.RegexQuantifier")
REGEX_QUANTIFIER__ONE__NAME = hydra.core.Name("one")
REGEX_QUANTIFIER__ZERO_OR_ONE__NAME = hydra.core.Name("zeroOrOne")
REGEX_QUANTIFIER__ZERO_OR_MORE__NAME = hydra.core.Name("zeroOrMore")
REGEX_QUANTIFIER__ONE_OR_MORE__NAME = hydra.core.Name("oneOrMore")
REGEX_QUANTIFIER__EXACTLY__NAME = hydra.core.Name("exactly")
REGEX_QUANTIFIER__AT_LEAST__NAME = hydra.core.Name("atLeast")
REGEX_QUANTIFIER__RANGE__NAME = hydra.core.Name("range")

@dataclass
class RegexSequence:
    """A path with a regex quantifier."""
    
    path: Path
    quantifier: RegexQuantifier

REGEX_SEQUENCE__NAME = hydra.core.Name("hydra.query.RegexSequence")
REGEX_SEQUENCE__PATH__NAME = hydra.core.Name("path")
REGEX_SEQUENCE__QUANTIFIER__NAME = hydra.core.Name("quantifier")

class StepEdge(Node["Edge"]):
    """An out-to-in traversal of an abstract edge."""

class StepProject(Node["hydra.core.Projection"]):
    """A projection from a record through one of its fields."""

class StepCompare(Node["ComparisonConstraint"]):
    """A comparison of two terms."""

# An atomic function as part of a query. When applied to a graph, steps are typed by function types.
type Step = StepEdge | StepProject | StepCompare

STEP__NAME = hydra.core.Name("hydra.query.Step")
STEP__EDGE__NAME = hydra.core.Name("edge")
STEP__PROJECT__NAME = hydra.core.Name("project")
STEP__COMPARE__NAME = hydra.core.Name("compare")

@dataclass
class TriplePattern:
    """A subject/predicate/object pattern."""
    
    subject: Node_
    predicate: Path
    object: Node_

TRIPLE_PATTERN__NAME = hydra.core.Name("hydra.query.TriplePattern")
TRIPLE_PATTERN__SUBJECT__NAME = hydra.core.Name("subject")
TRIPLE_PATTERN__PREDICATE__NAME = hydra.core.Name("predicate")
TRIPLE_PATTERN__OBJECT__NAME = hydra.core.Name("object")

class Variable(Node[str]):
    """A query variable."""

VARIABLE__NAME = hydra.core.Name("hydra.query.Variable")
