# Note: this is an automatically generated file. Do not edit.

r"""Language constraints for TypeScript."""

from __future__ import annotations
from functools import lru_cache
from typing import TypeVar, cast
import hydra.coders
import hydra.core
import hydra.lib.lists
import hydra.lib.sets
import hydra.strip
import hydra.variants

T0 = TypeVar("T0")

@lru_cache(1)
def type_script_language() -> hydra.coders.Language:
    r"""Language constraints for TypeScript."""

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
        return hydra.lib.sets.from_list((hydra.core.IntegerType.BIGINT,))
    @lru_cache(1)
    def term_variants() -> frozenset[hydra.variants.TermVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TermVariant.LIST, hydra.variants.TermVariant.LITERAL, hydra.variants.TermVariant.MAP, hydra.variants.TermVariant.MAYBE, hydra.variants.TermVariant.RECORD, hydra.variants.TermVariant.INJECT))
    @lru_cache(1)
    def type_variants() -> frozenset[hydra.variants.TypeVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TypeVariant.LIST, hydra.variants.TypeVariant.LITERAL, hydra.variants.TypeVariant.MAP, hydra.variants.TypeVariant.MAYBE, hydra.variants.TypeVariant.RECORD, hydra.variants.TypeVariant.UNION))
    def types(v1: hydra.core.Type) -> bool:
        match v1:
            case hydra.core.TypeMap(value=mt):
                match hydra.strip.deannotate_type(mt.values):
                    case hydra.core.TypeMaybe():
                        return False

                    case _:
                        return True

            case _:
                return True
    return hydra.coders.Language(hydra.coders.LanguageName("hydra.typeScript"), hydra.coders.LanguageConstraints(elimination_variants(), literal_variants(), float_types(), function_variants(), integer_types(), term_variants(), type_variants(), (lambda x1: types(x1))))

@lru_cache(1)
def type_script_reserved_words() -> frozenset[str]:
    r"""A set of reserved words in TypeScript. Taken directly from https://github.com/microsoft/TypeScript/issues/2536."""

    reserved_words = ("delete", "do", "else", "enum", "export", "extends", "false", "finally", "for", "function", "if", "import", "in", "instanceof", "new", "null", "return", "super", "switch", "this", "throw", "true", "try", "typeof", "var", "void", "while", "with")
    strict_mode_reserved_words = ("as", "implements", "interface", "let", "package", "private", "protected", "public", "static", "yield")
    contextuall_keywords = ("any", "boolean", "constructor", "declare", "from", "get", "module", "number", "of", "require", "set", "string", "symbol", "type")
    return hydra.lib.sets.from_list(hydra.lib.lists.concat((reserved_words, strict_mode_reserved_words, contextuall_keywords)))
