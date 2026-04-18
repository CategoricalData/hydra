# Note: this is an automatically generated file. Do not edit.

r"""Language constraints for Coq code generation."""

from __future__ import annotations
from functools import lru_cache
from typing import TypeVar, cast
import hydra.coders
import hydra.core
import hydra.lib.sets
import hydra.variants

T0 = TypeVar("T0")

@lru_cache(1)
def coq_language() -> hydra.coders.Language:
    @lru_cache(1)
    def elimination_variants() -> frozenset[hydra.variants.EliminationVariant]:
        return hydra.lib.sets.from_list((hydra.variants.EliminationVariant.RECORD, hydra.variants.EliminationVariant.UNION, hydra.variants.EliminationVariant.WRAP))
    @lru_cache(1)
    def literal_variants() -> frozenset[hydra.variants.LiteralVariant]:
        return hydra.lib.sets.from_list((hydra.variants.LiteralVariant.BOOLEAN, hydra.variants.LiteralVariant.FLOAT, hydra.variants.LiteralVariant.INTEGER, hydra.variants.LiteralVariant.STRING))
    @lru_cache(1)
    def float_types() -> frozenset[hydra.core.FloatType]:
        return hydra.lib.sets.from_list((hydra.core.FloatType.FLOAT64,))
    @lru_cache(1)
    def function_variants() -> frozenset[hydra.variants.FunctionVariant]:
        return hydra.lib.sets.from_list((hydra.variants.FunctionVariant.ELIMINATION, hydra.variants.FunctionVariant.LAMBDA))
    @lru_cache(1)
    def integer_types() -> frozenset[hydra.core.IntegerType]:
        return hydra.lib.sets.from_list((hydra.core.IntegerType.BIGINT,))
    @lru_cache(1)
    def term_variants() -> frozenset[hydra.variants.TermVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TermVariant.APPLICATION, hydra.variants.TermVariant.EITHER, hydra.variants.TermVariant.CASES, hydra.variants.TermVariant.LAMBDA, hydra.variants.TermVariant.PROJECT, hydra.variants.TermVariant.UNWRAP, hydra.variants.TermVariant.TYPE_APPLICATION, hydra.variants.TermVariant.TYPE_LAMBDA, hydra.variants.TermVariant.LET, hydra.variants.TermVariant.LIST, hydra.variants.TermVariant.LITERAL, hydra.variants.TermVariant.MAP, hydra.variants.TermVariant.MAYBE, hydra.variants.TermVariant.PAIR, hydra.variants.TermVariant.RECORD, hydra.variants.TermVariant.SET, hydra.variants.TermVariant.INJECT, hydra.variants.TermVariant.UNIT, hydra.variants.TermVariant.VARIABLE, hydra.variants.TermVariant.WRAP))
    @lru_cache(1)
    def type_variants() -> frozenset[hydra.variants.TypeVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TypeVariant.ANNOTATED, hydra.variants.TypeVariant.APPLICATION, hydra.variants.TypeVariant.EITHER, hydra.variants.TypeVariant.FORALL, hydra.variants.TypeVariant.FUNCTION, hydra.variants.TypeVariant.LIST, hydra.variants.TypeVariant.LITERAL, hydra.variants.TypeVariant.MAP, hydra.variants.TypeVariant.MAYBE, hydra.variants.TypeVariant.PAIR, hydra.variants.TypeVariant.RECORD, hydra.variants.TypeVariant.SET, hydra.variants.TypeVariant.UNION, hydra.variants.TypeVariant.UNIT, hydra.variants.TypeVariant.VARIABLE, hydra.variants.TypeVariant.VOID, hydra.variants.TypeVariant.WRAP))
    def type_predicate(_: T0) -> bool:
        return True
    return hydra.coders.Language(hydra.coders.LanguageName("hydra.coq"), hydra.coders.LanguageConstraints(elimination_variants(), literal_variants(), float_types(), function_variants(), integer_types(), term_variants(), type_variants(), (lambda x1: type_predicate(x1))))

@lru_cache(1)
def coq_reserved_words() -> frozenset[str]:
    return hydra.lib.sets.from_list(("as", "at", "cofix", "do", "else", "end", "exists", "exists2", "fix", "for", "forall", "fun", "if", "IF", "in", "let", "match", "mod", "open", "Prop", "return", "Set", "then", "Type", "using", "where", "with", "Axiom", "Class", "Coercion", "Context", "Definition", "Fixpoint", "Hypothesis", "Inductive", "Instance", "Lemma", "Module", "Notation", "Proof", "Qed", "Record", "Require", "Import", "Section", "End", "Theorem", "Example", "Variable", "Variables", "cons", "pair", "nil", "list", "option", "prod", "sum", "unit", "bool", "nat", "string", "term", "literal", "graph", "element"))

@lru_cache(1)
def coq_stripped_reserved_words() -> frozenset[str]:
    return hydra.lib.sets.from_list(("as", "at", "cofix", "do", "else", "end", "exists", "exists2", "fix", "for", "forall", "fun", "if", "IF", "in", "let", "match", "mod", "Prop", "return", "Set", "then", "Type", "using", "where", "with", "Axiom", "Class", "Coercion", "Context", "Definition", "Fixpoint", "Hypothesis", "Inductive", "Instance", "Lemma", "Module", "Notation", "Proof", "Qed", "Record", "Require", "Import", "Section", "End", "Theorem", "Example", "Variable", "Variables", "bool", "nat", "list", "option", "prod", "sum", "unit", "string", "String", "Empty_set", "true", "false", "None", "Some", "nil", "cons", "pair", "inl", "inr", "tt", "graph"))
