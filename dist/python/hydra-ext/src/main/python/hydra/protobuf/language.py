# Note: this is an automatically generated file. Do not edit.

r"""Language constraints for Protobuf v3."""

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
def protobuf_language() -> hydra.coders.Language:
    r"""Language constraints for Protocol Buffers v3."""

    @lru_cache(1)
    def elimination_variants() -> frozenset[T0]:
        return hydra.lib.sets.empty()
    @lru_cache(1)
    def literal_variants() -> frozenset[hydra.variants.LiteralVariant]:
        return hydra.lib.sets.from_list((hydra.variants.LiteralVariant.BINARY, hydra.variants.LiteralVariant.BOOLEAN, hydra.variants.LiteralVariant.FLOAT, hydra.variants.LiteralVariant.INTEGER, hydra.variants.LiteralVariant.STRING))
    @lru_cache(1)
    def float_types() -> frozenset[hydra.core.FloatType]:
        return hydra.lib.sets.from_list((hydra.core.FloatType.FLOAT32, hydra.core.FloatType.FLOAT64))
    @lru_cache(1)
    def function_variants() -> frozenset[T0]:
        return hydra.lib.sets.empty()
    @lru_cache(1)
    def integer_types() -> frozenset[hydra.core.IntegerType]:
        return hydra.lib.sets.from_list((hydra.core.IntegerType.INT32, hydra.core.IntegerType.INT64, hydra.core.IntegerType.UINT32, hydra.core.IntegerType.UINT64))
    @lru_cache(1)
    def term_variants() -> frozenset[hydra.variants.TermVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TermVariant.EITHER, hydra.variants.TermVariant.LIST, hydra.variants.TermVariant.LITERAL, hydra.variants.TermVariant.MAP, hydra.variants.TermVariant.MAYBE, hydra.variants.TermVariant.PAIR, hydra.variants.TermVariant.RECORD, hydra.variants.TermVariant.SET, hydra.variants.TermVariant.INJECT, hydra.variants.TermVariant.UNIT, hydra.variants.TermVariant.WRAP))
    @lru_cache(1)
    def type_variants() -> frozenset[hydra.variants.TypeVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TypeVariant.ANNOTATED, hydra.variants.TypeVariant.EITHER, hydra.variants.TypeVariant.LIST, hydra.variants.TypeVariant.LITERAL, hydra.variants.TypeVariant.MAP, hydra.variants.TypeVariant.MAYBE, hydra.variants.TypeVariant.PAIR, hydra.variants.TypeVariant.RECORD, hydra.variants.TypeVariant.SET, hydra.variants.TypeVariant.UNION, hydra.variants.TypeVariant.UNIT, hydra.variants.TypeVariant.VARIABLE, hydra.variants.TypeVariant.VOID, hydra.variants.TypeVariant.WRAP))
    def type_predicate(typ: hydra.core.Type):
        match typ:
            case hydra.core.TypeMap(value=mt):
                values_type = mt.values
                @lru_cache(1)
                def stripped() -> hydra.core.Type:
                    return hydra.strip.deannotate_type(values_type)
                def _hoist_values_type_body_1(v1):
                    match v1:
                        case hydra.core.TypeMaybe():
                            return False

                        case _:
                            return True
                return _hoist_values_type_body_1(stripped())

            case _:
                return True
    return hydra.coders.Language(hydra.coders.LanguageName("hydra.protobuf"), hydra.coders.LanguageConstraints(elimination_variants(), literal_variants(), float_types(), function_variants(), integer_types(), term_variants(), type_variants(), (lambda x1: type_predicate(x1))))

@lru_cache(1)
def protobuf_reserved_words() -> frozenset[str]:
    r"""A set of reserved words in Protobuf."""

    # See: http://google.github.io/proto-lens/reserved-names.html.
    field_names = ("case", "class", "data", "default", "deriving", "do", "else", "foreign", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "mdo", "module", "newtype", "of", "pattern", "proc", "rec", "then", "type", "where")
    return hydra.lib.sets.from_list(hydra.lib.lists.concat((field_names,)))
