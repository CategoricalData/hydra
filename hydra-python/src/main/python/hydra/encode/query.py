# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.query."""

from __future__ import annotations
from hydra.dsl.python import Maybe, frozenlist
from typing import cast
import hydra.core
import hydra.encode.core
import hydra.lib.lists
import hydra.lib.maybes
import hydra.query

def comparison_constraint(v1: hydra.query.ComparisonConstraint) -> hydra.core.Type:
    match v1:
        case hydra.query.ComparisonConstraint.EQUAL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("equal"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v)))))
        
        case hydra.query.ComparisonConstraint.NOT_EQUAL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("notEqual"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v2)))))
        
        case hydra.query.ComparisonConstraint.LESS_THAN:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("lessThan"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v3)))))
        
        case hydra.query.ComparisonConstraint.GREATER_THAN:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("greaterThan"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v4)))))
        
        case hydra.query.ComparisonConstraint.LESS_THAN_OR_EQUAL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("lessThanOrEqual"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v5)))))
        
        case hydra.query.ComparisonConstraint.GREATER_THAN_OR_EQUAL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("greaterThanOrEqual"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v6)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def edge(x: hydra.query.Edge) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.Edge"), (hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.name(x.type)), hydra.core.Field(hydra.core.Name("out"), (lambda opt: cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(hydra.encode.core.name, opt))))(x.out)), hydra.core.Field(hydra.core.Name("in"), (lambda opt: cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(hydra.encode.core.name, opt))))(x.in_))))))

def variable(x: hydra.query.Variable) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.query.Variable"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.value))))

def node(v1: hydra.query.Node_) -> hydra.core.Type:
    match v1:
        case hydra.query.NodeTerm(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Node"), hydra.core.Field(hydra.core.Name("term"), hydra.encode.core.term(v)))))
        
        case hydra.query.NodeVariable(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Node"), hydra.core.Field(hydra.core.Name("variable"), variable(v2)))))
        
        case hydra.query.NodeWildcard(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Node"), hydra.core.Field(hydra.core.Name("wildcard"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v3)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def range_(x: hydra.query.Range) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.Range"), (hydra.core.Field(hydra.core.Name("min"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x2)))))))(x.min)), hydra.core.Field(hydra.core.Name("max"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x2)))))))(x.max))))))

def regex_quantifier(v1: hydra.query.RegexQuantifier) -> hydra.core.Type:
    match v1:
        case hydra.query.RegexQuantifierOne(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("one"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v)))))
        
        case hydra.query.RegexQuantifierZeroOrOne(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("zeroOrOne"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v2)))))
        
        case hydra.query.RegexQuantifierZeroOrMore(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("zeroOrMore"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v3)))))
        
        case hydra.query.RegexQuantifierOneOrMore(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("oneOrMore"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v4)))))
        
        case hydra.query.RegexQuantifierExactly(value=v5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("exactly"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x)))))))(v5)))))
        
        case hydra.query.RegexQuantifierAtLeast(value=v6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("atLeast"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x)))))))(v6)))))
        
        case hydra.query.RegexQuantifierRange(value=v7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("range"), range_(v7)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def step(v1: hydra.query.Step) -> hydra.core.Type:
    match v1:
        case hydra.query.StepEdge(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Step"), hydra.core.Field(hydra.core.Name("edge"), edge(v)))))
        
        case hydra.query.StepProject(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Step"), hydra.core.Field(hydra.core.Name("project"), hydra.encode.core.projection(v2)))))
        
        case hydra.query.StepCompare(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Step"), hydra.core.Field(hydra.core.Name("compare"), comparison_constraint(v3)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def path(v1: hydra.query.Path) -> hydra.core.Type:
    match v1:
        case hydra.query.PathStep(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Path"), hydra.core.Field(hydra.core.Name("step"), step(v)))))
        
        case hydra.query.PathRegex(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Path"), hydra.core.Field(hydra.core.Name("regex"), regex_sequence(v2)))))
        
        case hydra.query.PathInverse(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Path"), hydra.core.Field(hydra.core.Name("inverse"), path(v3)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def regex_sequence(x: hydra.query.RegexSequence) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.RegexSequence"), (hydra.core.Field(hydra.core.Name("path"), path(x.path)), hydra.core.Field(hydra.core.Name("quantifier"), regex_quantifier(x.quantifier))))))

def triple_pattern(x: hydra.query.TriplePattern) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.TriplePattern"), (hydra.core.Field(hydra.core.Name("subject"), node(x.subject)), hydra.core.Field(hydra.core.Name("predicate"), path(x.predicate)), hydra.core.Field(hydra.core.Name("object"), node(x.object))))))

def graph_pattern(x: hydra.query.GraphPattern) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.GraphPattern"), (hydra.core.Field(hydra.core.Name("graph"), hydra.encode.core.name(x.graph)), hydra.core.Field(hydra.core.Name("patterns"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(pattern, xs))))(x.patterns))))))

def pattern(v1: hydra.query.Pattern) -> hydra.core.Type:
    match v1:
        case hydra.query.PatternTriple(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Pattern"), hydra.core.Field(hydra.core.Name("triple"), triple_pattern(v)))))
        
        case hydra.query.PatternNegation(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Pattern"), hydra.core.Field(hydra.core.Name("negation"), pattern(v2)))))
        
        case hydra.query.PatternConjunction(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Pattern"), hydra.core.Field(hydra.core.Name("conjunction"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(pattern, xs))))(v3)))))
        
        case hydra.query.PatternDisjunction(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Pattern"), hydra.core.Field(hydra.core.Name("disjunction"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(pattern, xs))))(v4)))))
        
        case hydra.query.PatternGraph(value=v5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Pattern"), hydra.core.Field(hydra.core.Name("graph"), graph_pattern(v5)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def query(x: hydra.query.Query) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.Query"), (hydra.core.Field(hydra.core.Name("variables"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(variable, xs))))(x.variables)), hydra.core.Field(hydra.core.Name("patterns"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(pattern, xs))))(x.patterns))))))
