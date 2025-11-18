# Note: this is an automatically generated file. Do not edit.

r"""Language constraints for Hydra Core."""

from __future__ import annotations
import hydra.coders
import hydra.core
import hydra.lib.sets
import hydra.reflect

def hydra_language() -> hydra.coders.Language:
    r"""Language constraints for Hydra Core, i.e. no constraints."""
    
    elimination_variants = hydra.lib.sets.from_list(hydra.reflect.elimination_variants)
    literal_variants = hydra.lib.sets.from_list(hydra.reflect.literal_variants)
    float_types = hydra.lib.sets.from_list(hydra.reflect.float_types)
    function_variants = hydra.lib.sets.from_list(hydra.reflect.function_variants)
    integer_types = hydra.lib.sets.from_list(hydra.reflect.integer_types)
    term_variants = hydra.lib.sets.from_list(hydra.reflect.term_variants)
    type_variants = hydra.lib.sets.from_list(hydra.reflect.type_variants)
    def types(t: hydra.core.Type) -> bool:
        match t:
            case _:
                return True
    return hydra.coders.Language(hydra.coders.LanguageName("hydra.core"), hydra.coders.LanguageConstraints(elimination_variants, literal_variants, float_types, function_variants, integer_types, term_variants, type_variants, types))
