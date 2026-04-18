# Note: this is an automatically generated file. Do not edit.

r"""Language constraints and reserved words for GraphQL."""

from __future__ import annotations
from functools import lru_cache
from typing import TypeVar, cast
import hydra.coders
import hydra.core
import hydra.lib.sets
import hydra.strip
import hydra.variants

T0 = TypeVar("T0")

@lru_cache(1)
def graphql_language() -> hydra.coders.Language:
    r"""Language constraints for GraphQL."""

    @lru_cache(1)
    def elimination_variants() -> frozenset[T0]:
        return hydra.lib.sets.empty()
    @lru_cache(1)
    def literal_variants() -> frozenset[hydra.variants.LiteralVariant]:
        return hydra.lib.sets.from_list((hydra.variants.LiteralVariant.BOOLEAN, hydra.variants.LiteralVariant.FLOAT, hydra.variants.LiteralVariant.INTEGER, hydra.variants.LiteralVariant.STRING))
    @lru_cache(1)
    def float_types() -> frozenset[hydra.core.FloatType]:
        return hydra.lib.sets.from_list((hydra.core.FloatType.FLOAT64,))
    @lru_cache(1)
    def function_variants() -> frozenset[T0]:
        return hydra.lib.sets.empty()
    @lru_cache(1)
    def integer_types() -> frozenset[hydra.core.IntegerType]:
        return hydra.lib.sets.from_list((hydra.core.IntegerType.INT32,))
    @lru_cache(1)
    def term_variants() -> frozenset[hydra.variants.TermVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TermVariant.LIST, hydra.variants.TermVariant.LITERAL, hydra.variants.TermVariant.MAYBE, hydra.variants.TermVariant.RECORD, hydra.variants.TermVariant.INJECT))
    @lru_cache(1)
    def type_variants() -> frozenset[hydra.variants.TypeVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TypeVariant.APPLICATION, hydra.variants.TypeVariant.EITHER, hydra.variants.TypeVariant.FORALL, hydra.variants.TypeVariant.FUNCTION, hydra.variants.TypeVariant.LIST, hydra.variants.TypeVariant.LITERAL, hydra.variants.TypeVariant.MAP, hydra.variants.TypeVariant.PAIR, hydra.variants.TypeVariant.SET, hydra.variants.TypeVariant.UNIT, hydra.variants.TypeVariant.WRAP, hydra.variants.TypeVariant.MAYBE, hydra.variants.TypeVariant.RECORD, hydra.variants.TypeVariant.UNION, hydra.variants.TypeVariant.VARIABLE))
    def type_predicate(typ: hydra.core.Type):
        def _hoist_type_predicate_1(v1):
            match v1:
                case hydra.core.TypeMaybe():
                    return False

                case _:
                    return True
        match hydra.strip.deannotate_type(typ):
            case hydra.core.TypeMaybe(value=inner):
                return _hoist_type_predicate_1(hydra.strip.deannotate_type(inner))

            case _:
                return True
    return hydra.coders.Language(hydra.coders.LanguageName("hydra.graphql"), hydra.coders.LanguageConstraints(elimination_variants(), literal_variants(), float_types(), function_variants(), integer_types(), term_variants(), type_variants(), (lambda x1: type_predicate(x1))))

@lru_cache(1)
def graphql_reserved_words() -> frozenset[str]:
    r"""A set of reserved words in GraphQL."""

    return hydra.lib.sets.from_list(("true", "false"))
