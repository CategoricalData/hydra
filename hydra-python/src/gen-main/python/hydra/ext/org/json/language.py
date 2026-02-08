# Note: this is an automatically generated file. Do not edit.

r"""Language constraints for JSON."""

from __future__ import annotations
from functools import lru_cache
from typing import TypeVar, cast
import hydra.coders
import hydra.core
import hydra.lib.sets
import hydra.rewriting
import hydra.variants

T0 = TypeVar("T0")

@lru_cache(1)
def json_language() -> hydra.coders.Language:
    r"""Language constraints for JSON."""
    
    @lru_cache(1)
    def elimination_variants() -> frozenset[T0]:
        return hydra.lib.sets.empty()
    @lru_cache(1)
    def literal_variants() -> frozenset[hydra.variants.LiteralVariant]:
        return hydra.lib.sets.from_list((hydra.variants.LiteralVariant.BOOLEAN, hydra.variants.LiteralVariant.FLOAT, hydra.variants.LiteralVariant.INTEGER, hydra.variants.LiteralVariant.STRING))
    @lru_cache(1)
    def float_types() -> frozenset[hydra.core.FloatType]:
        return hydra.lib.sets.from_list((hydra.core.FloatType.BIGFLOAT,))
    @lru_cache(1)
    def function_variants() -> frozenset[T0]:
        return hydra.lib.sets.empty()
    @lru_cache(1)
    def integer_types() -> frozenset[hydra.core.IntegerType]:
        return hydra.lib.sets.from_list((hydra.core.IntegerType.BIGINT,))
    @lru_cache(1)
    def term_variants() -> frozenset[hydra.variants.TermVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TermVariant.LIST, hydra.variants.TermVariant.LITERAL, hydra.variants.TermVariant.MAP, hydra.variants.TermVariant.MAYBE, hydra.variants.TermVariant.RECORD))
    @lru_cache(1)
    def type_variants() -> frozenset[hydra.variants.TypeVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TypeVariant.LIST, hydra.variants.TypeVariant.LITERAL, hydra.variants.TypeVariant.MAP, hydra.variants.TypeVariant.MAYBE, hydra.variants.TypeVariant.RECORD))
    def type_predicate(typ: hydra.core.Type) -> bool:
        def _hoist_type_predicate_1(v1: hydra.core.Type) -> bool:
            match v1:
                case hydra.core.TypeMaybe():
                    return False
                
                case _:
                    return True
        match hydra.rewriting.deannotate_type(typ):
            case hydra.core.TypeMaybe(value=inner_type):
                return _hoist_type_predicate_1(inner_type)
            
            case _:
                return True
    return hydra.coders.Language(hydra.coders.LanguageName("hydra.ext.org.json"), hydra.coders.LanguageConstraints(elimination_variants(), literal_variants(), float_types(), function_variants(), integer_types(), term_variants(), type_variants(), (lambda x1: type_predicate(x1))))
