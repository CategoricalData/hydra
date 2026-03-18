# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.grammar."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.core
import hydra.phantoms

def constant(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.grammar.Constant"), x.value))))

def grammar(x: hydra.phantoms.TTerm[frozenlist[hydra.grammar.Production]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.grammar.Grammar"), x.value))))

def label(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.grammar.Label"), x.value))))

def labeled_pattern(label: hydra.phantoms.TTerm[hydra.grammar.Label], pattern: hydra.phantoms.TTerm[hydra.grammar.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.grammar.LabeledPattern"), (hydra.core.Field(hydra.core.Name("label"), label.value), hydra.core.Field(hydra.core.Name("pattern"), pattern.value))))))

def labeled_pattern_label(x: hydra.phantoms.TTerm[hydra.grammar.LabeledPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.grammar.LabeledPattern"), hydra.core.Name("label")))))))), x.value))))

def labeled_pattern_pattern(x: hydra.phantoms.TTerm[hydra.grammar.LabeledPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.grammar.LabeledPattern"), hydra.core.Name("pattern")))))))), x.value))))

def labeled_pattern_with_label(original: hydra.phantoms.TTerm[hydra.grammar.LabeledPattern], new_val: hydra.phantoms.TTerm[hydra.grammar.Label]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.grammar.LabeledPattern"), (hydra.core.Field(hydra.core.Name("label"), new_val.value), hydra.core.Field(hydra.core.Name("pattern"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.grammar.LabeledPattern"), hydra.core.Name("pattern")))))))), original.value)))))))))

def labeled_pattern_with_pattern(original: hydra.phantoms.TTerm[hydra.grammar.LabeledPattern], new_val: hydra.phantoms.TTerm[hydra.grammar.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.grammar.LabeledPattern"), (hydra.core.Field(hydra.core.Name("label"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.grammar.LabeledPattern"), hydra.core.Name("label")))))))), original.value)))), hydra.core.Field(hydra.core.Name("pattern"), new_val.value))))))

def pattern_alternatives(x: hydra.phantoms.TTerm[frozenlist[hydra.grammar.Pattern]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("alternatives"), x.value)))))

def pattern_constant(x: hydra.phantoms.TTerm[hydra.grammar.Constant]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("constant"), x.value)))))

def pattern_ignored(x: hydra.phantoms.TTerm[hydra.grammar.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("ignored"), x.value)))))

def pattern_labeled(x: hydra.phantoms.TTerm[hydra.grammar.LabeledPattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("labeled"), x.value)))))

pattern_nil = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("nil"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def pattern_nonterminal(x: hydra.phantoms.TTerm[hydra.grammar.Symbol]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("nonterminal"), x.value)))))

def pattern_option(x: hydra.phantoms.TTerm[hydra.grammar.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("option"), x.value)))))

def pattern_plus(x: hydra.phantoms.TTerm[hydra.grammar.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("plus"), x.value)))))

def pattern_regex(x: hydra.phantoms.TTerm[hydra.grammar.Regex]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("regex"), x.value)))))

def pattern_sequence(x: hydra.phantoms.TTerm[frozenlist[hydra.grammar.Pattern]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("sequence"), x.value)))))

def pattern_star(x: hydra.phantoms.TTerm[hydra.grammar.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("star"), x.value)))))

def production(symbol: hydra.phantoms.TTerm[hydra.grammar.Symbol], pattern: hydra.phantoms.TTerm[hydra.grammar.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.grammar.Production"), (hydra.core.Field(hydra.core.Name("symbol"), symbol.value), hydra.core.Field(hydra.core.Name("pattern"), pattern.value))))))

def production_pattern(x: hydra.phantoms.TTerm[hydra.grammar.Production]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.grammar.Production"), hydra.core.Name("pattern")))))))), x.value))))

def production_symbol(x: hydra.phantoms.TTerm[hydra.grammar.Production]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.grammar.Production"), hydra.core.Name("symbol")))))))), x.value))))

def production_with_pattern(original: hydra.phantoms.TTerm[hydra.grammar.Production], new_val: hydra.phantoms.TTerm[hydra.grammar.Pattern]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.grammar.Production"), (hydra.core.Field(hydra.core.Name("symbol"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.grammar.Production"), hydra.core.Name("symbol")))))))), original.value)))), hydra.core.Field(hydra.core.Name("pattern"), new_val.value))))))

def production_with_symbol(original: hydra.phantoms.TTerm[hydra.grammar.Production], new_val: hydra.phantoms.TTerm[hydra.grammar.Symbol]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.grammar.Production"), (hydra.core.Field(hydra.core.Name("symbol"), new_val.value), hydra.core.Field(hydra.core.Name("pattern"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.grammar.Production"), hydra.core.Name("pattern")))))))), original.value)))))))))

def regex(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.grammar.Regex"), x.value))))

def symbol(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.grammar.Symbol"), x.value))))

def un_constant(x: hydra.phantoms.TTerm[hydra.grammar.Constant]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.grammar.Constant"))))))), x.value))))

def un_grammar(x: hydra.phantoms.TTerm[hydra.grammar.Grammar]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.grammar.Grammar"))))))), x.value))))

def un_label(x: hydra.phantoms.TTerm[hydra.grammar.Label]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.grammar.Label"))))))), x.value))))

def un_regex(x: hydra.phantoms.TTerm[hydra.grammar.Regex]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.grammar.Regex"))))))), x.value))))

def un_symbol(x: hydra.phantoms.TTerm[hydra.grammar.Symbol]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.grammar.Symbol"))))))), x.value))))
