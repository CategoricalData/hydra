# Note: this is an automatically generated file. Do not edit.

r"""Language constraints for Hydra Core."""

from __future__ import annotations
from functools import lru_cache
import hydra.coders
import hydra.core
import hydra.lib.sets
import hydra.reflect
import hydra.variants

@lru_cache(1)
def hydra_language() -> hydra.core.Type:
    r"""Language constraints for Hydra Core, i.e. no constraints."""
    
    @lru_cache(1)
    def elimination_variants() -> frozenset[hydra.variants.EliminationVariant]:
        return hydra.lib.sets.from_list(hydra.reflect.elimination_variants)
    @lru_cache(1)
    def literal_variants() -> frozenset[hydra.variants.LiteralVariant]:
        return hydra.lib.sets.from_list(hydra.reflect.literal_variants)
    @lru_cache(1)
    def float_types() -> frozenset[hydra.core.FloatType]:
        return hydra.lib.sets.from_list(hydra.reflect.float_types)
    @lru_cache(1)
    def function_variants() -> frozenset[hydra.variants.FunctionVariant]:
        return hydra.lib.sets.from_list(hydra.reflect.function_variants)
    @lru_cache(1)
    def integer_types() -> frozenset[hydra.core.IntegerType]:
        return hydra.lib.sets.from_list(hydra.reflect.integer_types)
    @lru_cache(1)
    def term_variants() -> frozenset[hydra.variants.TermVariant]:
        return hydra.lib.sets.from_list(hydra.reflect.term_variants)
    @lru_cache(1)
    def type_variants() -> frozenset[hydra.variants.TypeVariant]:
        return hydra.lib.sets.from_list(hydra.reflect.type_variants)
    def types(t: hydra.core.Type) -> bool:
        match t:
            case _:
                return True
    return hydra.coders.Language(hydra.coders.LanguageName("hydra.core"), hydra.coders.LanguageConstraints(elimination_variants(), literal_variants(), float_types(), function_variants(), integer_types(), term_variants(), type_variants(), types))
