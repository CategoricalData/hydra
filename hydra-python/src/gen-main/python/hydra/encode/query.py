# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.query."""

from __future__ import annotations
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.core
import hydra.encode.core
import hydra.lib.lists
import hydra.lib.maybes
import hydra.query

def comparison_constraint(v1: hydra.query.ComparisonConstraint) -> hydra.core.Type:
    match v1:
        case hydra.query.ComparisonConstraint.EQUAL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("equal"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.query.ComparisonConstraint.NOT_EQUAL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("notEqual"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.query.ComparisonConstraint.LESS_THAN:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("lessThan"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.query.ComparisonConstraint.GREATER_THAN:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("greaterThan"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.query.ComparisonConstraint.LESS_THAN_OR_EQUAL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("lessThanOrEqual"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.query.ComparisonConstraint.GREATER_THAN_OR_EQUAL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("greaterThanOrEqual"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def edge(x: hydra.query.Edge) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.Edge"), (hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.name(x.type)), hydra.core.Field(hydra.core.Name("out"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(hydra.encode.core.name, x.out)))), hydra.core.Field(hydra.core.Name("in"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(hydra.encode.core.name, x.in_))))))))

def variable(x: hydra.query.Variable) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.query.Variable"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def node(v1: hydra.query.Node_) -> hydra.core.Type:
    match v1:
        case hydra.query.NodeTerm(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Node"), hydra.core.Field(hydra.core.Name("term"), hydra.encode.core.term(y)))))
        
        case hydra.query.NodeVariable(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Node"), hydra.core.Field(hydra.core.Name("variable"), variable(y2)))))
        
        case hydra.query.NodeWildcard():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Node"), hydra.core.Field(hydra.core.Name("wildcard"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def range_(x: hydra.query.Range) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.Range"), (hydra.core.Field(hydra.core.Name("min"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x.min))))))), hydra.core.Field(hydra.core.Name("max"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x.max)))))))))))

def regex_quantifier(v1: hydra.query.RegexQuantifier) -> hydra.core.Type:
    match v1:
        case hydra.query.RegexQuantifierOne():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("one"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.query.RegexQuantifierZeroOrOne():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("zeroOrOne"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.query.RegexQuantifierZeroOrMore():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("zeroOrMore"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.query.RegexQuantifierOneOrMore():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("oneOrMore"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.query.RegexQuantifierExactly(value=y5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("exactly"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(y5))))))))))
        
        case hydra.query.RegexQuantifierAtLeast(value=y6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("atLeast"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(y6))))))))))
        
        case hydra.query.RegexQuantifierRange(value=y7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("range"), range_(y7)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def step(v1: hydra.query.Step) -> hydra.core.Type:
    match v1:
        case hydra.query.StepEdge(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Step"), hydra.core.Field(hydra.core.Name("edge"), edge(y)))))
        
        case hydra.query.StepProject(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Step"), hydra.core.Field(hydra.core.Name("project"), hydra.encode.core.projection(y2)))))
        
        case hydra.query.StepCompare(value=y3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Step"), hydra.core.Field(hydra.core.Name("compare"), comparison_constraint(y3)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def path(v1: hydra.query.Path) -> hydra.core.Type:
    match v1:
        case hydra.query.PathStep(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Path"), hydra.core.Field(hydra.core.Name("step"), step(y)))))
        
        case hydra.query.PathRegex(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Path"), hydra.core.Field(hydra.core.Name("regex"), regex_sequence(y2)))))
        
        case hydra.query.PathInverse(value=y3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Path"), hydra.core.Field(hydra.core.Name("inverse"), path(y3)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def regex_sequence(x: hydra.query.RegexSequence) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.RegexSequence"), (hydra.core.Field(hydra.core.Name("path"), path(x.path)), hydra.core.Field(hydra.core.Name("quantifier"), regex_quantifier(x.quantifier))))))

def triple_pattern(x: hydra.query.TriplePattern) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.TriplePattern"), (hydra.core.Field(hydra.core.Name("subject"), node(x.subject)), hydra.core.Field(hydra.core.Name("predicate"), path(x.predicate)), hydra.core.Field(hydra.core.Name("object"), node(x.object))))))

def graph_pattern(x: hydra.query.GraphPattern) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.GraphPattern"), (hydra.core.Field(hydra.core.Name("graph"), hydra.encode.core.name(x.graph)), hydra.core.Field(hydra.core.Name("patterns"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(pattern, x.patterns))))))))

def pattern(v1: hydra.query.Pattern) -> hydra.core.Type:
    match v1:
        case hydra.query.PatternTriple(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Pattern"), hydra.core.Field(hydra.core.Name("triple"), triple_pattern(y)))))
        
        case hydra.query.PatternNegation(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Pattern"), hydra.core.Field(hydra.core.Name("negation"), pattern(y2)))))
        
        case hydra.query.PatternConjunction(value=y3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Pattern"), hydra.core.Field(hydra.core.Name("conjunction"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(pattern, y3)))))))
        
        case hydra.query.PatternDisjunction(value=y4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Pattern"), hydra.core.Field(hydra.core.Name("disjunction"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(pattern, y4)))))))
        
        case hydra.query.PatternGraph(value=y5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.query.Pattern"), hydra.core.Field(hydra.core.Name("graph"), graph_pattern(y5)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def query(x: hydra.query.Query) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.Query"), (hydra.core.Field(hydra.core.Name("variables"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(variable, x.variables)))), hydra.core.Field(hydra.core.Name("patterns"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(pattern, x.patterns))))))))
