# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.query."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Maybe, frozenlist
from typing import cast
import hydra.core
import hydra.phantoms

comparison_constraint_equal = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("equal"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

comparison_constraint_greater_than = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("greaterThan"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

comparison_constraint_greater_than_or_equal = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("greaterThanOrEqual"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

comparison_constraint_less_than = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("lessThan"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

comparison_constraint_less_than_or_equal = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("lessThanOrEqual"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

comparison_constraint_not_equal = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.ComparisonConstraint"), hydra.core.Field(hydra.core.Name("notEqual"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def edge(type: hydra.phantoms.TTerm[hydra.core.Name], out: hydra.phantoms.TTerm[Maybe[hydra.core.Name]], in_: hydra.phantoms.TTerm[Maybe[hydra.core.Name]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.Edge"), (hydra.core.Field(hydra.core.Name("type"), type.value), hydra.core.Field(hydra.core.Name("out"), out.value), hydra.core.Field(hydra.core.Name("in"), in_.value))))))

def edge_in(x: hydra.phantoms.TTerm[hydra.query.Edge]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Edge"), hydra.core.Name("in")))), x.value))))

def edge_out(x: hydra.phantoms.TTerm[hydra.query.Edge]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Edge"), hydra.core.Name("out")))), x.value))))

def edge_type(x: hydra.phantoms.TTerm[hydra.query.Edge]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Edge"), hydra.core.Name("type")))), x.value))))

def edge_with_in(original: hydra.phantoms.TTerm[hydra.query.Edge], new_val: hydra.phantoms.TTerm[Maybe[hydra.core.Name]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.Edge"), (hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Edge"), hydra.core.Name("type")))), original.value)))), hydra.core.Field(hydra.core.Name("out"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Edge"), hydra.core.Name("out")))), original.value)))), hydra.core.Field(hydra.core.Name("in"), new_val.value))))))

def edge_with_out(original: hydra.phantoms.TTerm[hydra.query.Edge], new_val: hydra.phantoms.TTerm[Maybe[hydra.core.Name]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.Edge"), (hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Edge"), hydra.core.Name("type")))), original.value)))), hydra.core.Field(hydra.core.Name("out"), new_val.value), hydra.core.Field(hydra.core.Name("in"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Edge"), hydra.core.Name("in")))), original.value)))))))))

def edge_with_type(original: hydra.phantoms.TTerm[hydra.query.Edge], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.Edge"), (hydra.core.Field(hydra.core.Name("type"), new_val.value), hydra.core.Field(hydra.core.Name("out"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Edge"), hydra.core.Name("out")))), original.value)))), hydra.core.Field(hydra.core.Name("in"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Edge"), hydra.core.Name("in")))), original.value)))))))))

def graph_pattern(graph: hydra.phantoms.TTerm[hydra.core.Name], patterns: hydra.phantoms.TTerm[frozenlist[hydra.query.Pattern]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.GraphPattern"), (hydra.core.Field(hydra.core.Name("graph"), graph.value), hydra.core.Field(hydra.core.Name("patterns"), patterns.value))))))

def graph_pattern_graph(x: hydra.phantoms.TTerm[hydra.query.GraphPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.GraphPattern"), hydra.core.Name("graph")))), x.value))))

def graph_pattern_patterns(x: hydra.phantoms.TTerm[hydra.query.GraphPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.GraphPattern"), hydra.core.Name("patterns")))), x.value))))

def graph_pattern_with_graph(original: hydra.phantoms.TTerm[hydra.query.GraphPattern], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.GraphPattern"), (hydra.core.Field(hydra.core.Name("graph"), new_val.value), hydra.core.Field(hydra.core.Name("patterns"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.GraphPattern"), hydra.core.Name("patterns")))), original.value)))))))))

def graph_pattern_with_patterns(original: hydra.phantoms.TTerm[hydra.query.GraphPattern], new_val: hydra.phantoms.TTerm[frozenlist[hydra.query.Pattern]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.GraphPattern"), (hydra.core.Field(hydra.core.Name("graph"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.GraphPattern"), hydra.core.Name("graph")))), original.value)))), hydra.core.Field(hydra.core.Name("patterns"), new_val.value))))))

def node_term(x: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.Node"), hydra.core.Field(hydra.core.Name("term"), x.value)))))

def node_variable(x: hydra.phantoms.TTerm[hydra.query.Variable]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.Node"), hydra.core.Field(hydra.core.Name("variable"), x.value)))))

node_wildcard = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.Node"), hydra.core.Field(hydra.core.Name("wildcard"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def path_equation(left: hydra.phantoms.TTerm[hydra.query.Path], right: hydra.phantoms.TTerm[hydra.query.Path]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.PathEquation"), (hydra.core.Field(hydra.core.Name("left"), left.value), hydra.core.Field(hydra.core.Name("right"), right.value))))))

def path_equation_left(x: hydra.phantoms.TTerm[hydra.query.PathEquation]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.PathEquation"), hydra.core.Name("left")))), x.value))))

def path_equation_right(x: hydra.phantoms.TTerm[hydra.query.PathEquation]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.PathEquation"), hydra.core.Name("right")))), x.value))))

def path_equation_with_left(original: hydra.phantoms.TTerm[hydra.query.PathEquation], new_val: hydra.phantoms.TTerm[hydra.query.Path]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.PathEquation"), (hydra.core.Field(hydra.core.Name("left"), new_val.value), hydra.core.Field(hydra.core.Name("right"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.PathEquation"), hydra.core.Name("right")))), original.value)))))))))

def path_equation_with_right(original: hydra.phantoms.TTerm[hydra.query.PathEquation], new_val: hydra.phantoms.TTerm[hydra.query.Path]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.PathEquation"), (hydra.core.Field(hydra.core.Name("left"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.PathEquation"), hydra.core.Name("left")))), original.value)))), hydra.core.Field(hydra.core.Name("right"), new_val.value))))))

def path_inverse(x: hydra.phantoms.TTerm[hydra.query.Path]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.Path"), hydra.core.Field(hydra.core.Name("inverse"), x.value)))))

def path_regex(x: hydra.phantoms.TTerm[hydra.query.RegexSequence]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.Path"), hydra.core.Field(hydra.core.Name("regex"), x.value)))))

def path_step(x: hydra.phantoms.TTerm[hydra.query.Step]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.Path"), hydra.core.Field(hydra.core.Name("step"), x.value)))))

def pattern_conjunction(x: hydra.phantoms.TTerm[frozenlist[hydra.query.Pattern]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.Pattern"), hydra.core.Field(hydra.core.Name("conjunction"), x.value)))))

def pattern_disjunction(x: hydra.phantoms.TTerm[frozenlist[hydra.query.Pattern]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.Pattern"), hydra.core.Field(hydra.core.Name("disjunction"), x.value)))))

def pattern_graph(x: hydra.phantoms.TTerm[hydra.query.GraphPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.Pattern"), hydra.core.Field(hydra.core.Name("graph"), x.value)))))

def pattern_implication(antecedent: hydra.phantoms.TTerm[hydra.query.Pattern], consequent: hydra.phantoms.TTerm[hydra.query.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.PatternImplication"), (hydra.core.Field(hydra.core.Name("antecedent"), antecedent.value), hydra.core.Field(hydra.core.Name("consequent"), consequent.value))))))

def pattern_implication_antecedent(x: hydra.phantoms.TTerm[hydra.query.PatternImplication]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.PatternImplication"), hydra.core.Name("antecedent")))), x.value))))

def pattern_implication_consequent(x: hydra.phantoms.TTerm[hydra.query.PatternImplication]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.PatternImplication"), hydra.core.Name("consequent")))), x.value))))

def pattern_implication_with_antecedent(original: hydra.phantoms.TTerm[hydra.query.PatternImplication], new_val: hydra.phantoms.TTerm[hydra.query.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.PatternImplication"), (hydra.core.Field(hydra.core.Name("antecedent"), new_val.value), hydra.core.Field(hydra.core.Name("consequent"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.PatternImplication"), hydra.core.Name("consequent")))), original.value)))))))))

def pattern_implication_with_consequent(original: hydra.phantoms.TTerm[hydra.query.PatternImplication], new_val: hydra.phantoms.TTerm[hydra.query.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.PatternImplication"), (hydra.core.Field(hydra.core.Name("antecedent"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.PatternImplication"), hydra.core.Name("antecedent")))), original.value)))), hydra.core.Field(hydra.core.Name("consequent"), new_val.value))))))

def pattern_negation(x: hydra.phantoms.TTerm[hydra.query.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.Pattern"), hydra.core.Field(hydra.core.Name("negation"), x.value)))))

def pattern_triple(x: hydra.phantoms.TTerm[hydra.query.TriplePattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.Pattern"), hydra.core.Field(hydra.core.Name("triple"), x.value)))))

def query(variables: hydra.phantoms.TTerm[frozenlist[hydra.query.Variable]], patterns: hydra.phantoms.TTerm[frozenlist[hydra.query.Pattern]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.Query"), (hydra.core.Field(hydra.core.Name("variables"), variables.value), hydra.core.Field(hydra.core.Name("patterns"), patterns.value))))))

def query_patterns(x: hydra.phantoms.TTerm[hydra.query.Query]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Query"), hydra.core.Name("patterns")))), x.value))))

def query_variables(x: hydra.phantoms.TTerm[hydra.query.Query]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Query"), hydra.core.Name("variables")))), x.value))))

def query_with_patterns(original: hydra.phantoms.TTerm[hydra.query.Query], new_val: hydra.phantoms.TTerm[frozenlist[hydra.query.Pattern]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.Query"), (hydra.core.Field(hydra.core.Name("variables"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Query"), hydra.core.Name("variables")))), original.value)))), hydra.core.Field(hydra.core.Name("patterns"), new_val.value))))))

def query_with_variables(original: hydra.phantoms.TTerm[hydra.query.Query], new_val: hydra.phantoms.TTerm[frozenlist[hydra.query.Variable]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.Query"), (hydra.core.Field(hydra.core.Name("variables"), new_val.value), hydra.core.Field(hydra.core.Name("patterns"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Query"), hydra.core.Name("patterns")))), original.value)))))))))

def range_(min: hydra.phantoms.TTerm[int], max: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.Range"), (hydra.core.Field(hydra.core.Name("min"), min.value), hydra.core.Field(hydra.core.Name("max"), max.value))))))

def range_max(x: hydra.phantoms.TTerm[hydra.query.Range]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Range"), hydra.core.Name("max")))), x.value))))

def range_min(x: hydra.phantoms.TTerm[hydra.query.Range]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Range"), hydra.core.Name("min")))), x.value))))

def range_with_max(original: hydra.phantoms.TTerm[hydra.query.Range], new_val: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.Range"), (hydra.core.Field(hydra.core.Name("min"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Range"), hydra.core.Name("min")))), original.value)))), hydra.core.Field(hydra.core.Name("max"), new_val.value))))))

def range_with_min(original: hydra.phantoms.TTerm[hydra.query.Range], new_val: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.Range"), (hydra.core.Field(hydra.core.Name("min"), new_val.value), hydra.core.Field(hydra.core.Name("max"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.Range"), hydra.core.Name("max")))), original.value)))))))))

def regex_quantifier_at_least(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("atLeast"), x.value)))))

def regex_quantifier_exactly(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("exactly"), x.value)))))

regex_quantifier_one = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("one"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

regex_quantifier_one_or_more = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("oneOrMore"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def regex_quantifier_range(x: hydra.phantoms.TTerm[hydra.query.Range]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("range"), x.value)))))

regex_quantifier_zero_or_more = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("zeroOrMore"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

regex_quantifier_zero_or_one = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.RegexQuantifier"), hydra.core.Field(hydra.core.Name("zeroOrOne"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def regex_sequence(path: hydra.phantoms.TTerm[hydra.query.Path], quantifier: hydra.phantoms.TTerm[hydra.query.RegexQuantifier]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.RegexSequence"), (hydra.core.Field(hydra.core.Name("path"), path.value), hydra.core.Field(hydra.core.Name("quantifier"), quantifier.value))))))

def regex_sequence_path(x: hydra.phantoms.TTerm[hydra.query.RegexSequence]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.RegexSequence"), hydra.core.Name("path")))), x.value))))

def regex_sequence_quantifier(x: hydra.phantoms.TTerm[hydra.query.RegexSequence]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.RegexSequence"), hydra.core.Name("quantifier")))), x.value))))

def regex_sequence_with_path(original: hydra.phantoms.TTerm[hydra.query.RegexSequence], new_val: hydra.phantoms.TTerm[hydra.query.Path]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.RegexSequence"), (hydra.core.Field(hydra.core.Name("path"), new_val.value), hydra.core.Field(hydra.core.Name("quantifier"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.RegexSequence"), hydra.core.Name("quantifier")))), original.value)))))))))

def regex_sequence_with_quantifier(original: hydra.phantoms.TTerm[hydra.query.RegexSequence], new_val: hydra.phantoms.TTerm[hydra.query.RegexQuantifier]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.RegexSequence"), (hydra.core.Field(hydra.core.Name("path"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.RegexSequence"), hydra.core.Name("path")))), original.value)))), hydra.core.Field(hydra.core.Name("quantifier"), new_val.value))))))

def step_compare(x: hydra.phantoms.TTerm[hydra.query.ComparisonConstraint]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.Step"), hydra.core.Field(hydra.core.Name("compare"), x.value)))))

def step_edge(x: hydra.phantoms.TTerm[hydra.query.Edge]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.Step"), hydra.core.Field(hydra.core.Name("edge"), x.value)))))

def step_project(x: hydra.phantoms.TTerm[hydra.core.Projection]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.query.Step"), hydra.core.Field(hydra.core.Name("project"), x.value)))))

def triple_pattern(subject: hydra.phantoms.TTerm[hydra.query.Node_], predicate: hydra.phantoms.TTerm[hydra.query.Path], object: hydra.phantoms.TTerm[hydra.query.Node_]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.TriplePattern"), (hydra.core.Field(hydra.core.Name("subject"), subject.value), hydra.core.Field(hydra.core.Name("predicate"), predicate.value), hydra.core.Field(hydra.core.Name("object"), object.value))))))

def triple_pattern_object(x: hydra.phantoms.TTerm[hydra.query.TriplePattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.TriplePattern"), hydra.core.Name("object")))), x.value))))

def triple_pattern_predicate(x: hydra.phantoms.TTerm[hydra.query.TriplePattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.TriplePattern"), hydra.core.Name("predicate")))), x.value))))

def triple_pattern_subject(x: hydra.phantoms.TTerm[hydra.query.TriplePattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.TriplePattern"), hydra.core.Name("subject")))), x.value))))

def triple_pattern_with_object(original: hydra.phantoms.TTerm[hydra.query.TriplePattern], new_val: hydra.phantoms.TTerm[hydra.query.Node_]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.TriplePattern"), (hydra.core.Field(hydra.core.Name("subject"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.TriplePattern"), hydra.core.Name("subject")))), original.value)))), hydra.core.Field(hydra.core.Name("predicate"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.TriplePattern"), hydra.core.Name("predicate")))), original.value)))), hydra.core.Field(hydra.core.Name("object"), new_val.value))))))

def triple_pattern_with_predicate(original: hydra.phantoms.TTerm[hydra.query.TriplePattern], new_val: hydra.phantoms.TTerm[hydra.query.Path]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.TriplePattern"), (hydra.core.Field(hydra.core.Name("subject"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.TriplePattern"), hydra.core.Name("subject")))), original.value)))), hydra.core.Field(hydra.core.Name("predicate"), new_val.value), hydra.core.Field(hydra.core.Name("object"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.TriplePattern"), hydra.core.Name("object")))), original.value)))))))))

def triple_pattern_with_subject(original: hydra.phantoms.TTerm[hydra.query.TriplePattern], new_val: hydra.phantoms.TTerm[hydra.query.Node_]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.query.TriplePattern"), (hydra.core.Field(hydra.core.Name("subject"), new_val.value), hydra.core.Field(hydra.core.Name("predicate"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.TriplePattern"), hydra.core.Name("predicate")))), original.value)))), hydra.core.Field(hydra.core.Name("object"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.query.TriplePattern"), hydra.core.Name("object")))), original.value)))))))))

def un_variable(x: hydra.phantoms.TTerm[hydra.query.Variable]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.query.Variable"))), x.value))))

def variable(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.query.Variable"), x.value))))
