# Note: this is an automatically generated file. Do not edit.

r"""Language constraints and reserved words for Python 3."""

from __future__ import annotations
from functools import lru_cache
from typing import TypeVar, cast
import hydra.coders
import hydra.core
import hydra.lib.lists
import hydra.lib.sets
import hydra.variants

T0 = TypeVar("T0")

@lru_cache(1)
def python_language() -> hydra.coders.Language:
    r"""Language constraints for Python 3."""
    
    @lru_cache(1)
    def elimination_variants() -> frozenset[hydra.variants.EliminationVariant]:
        return hydra.lib.sets.from_list((hydra.variants.EliminationVariant.RECORD, hydra.variants.EliminationVariant.UNION, hydra.variants.EliminationVariant.WRAP))
    @lru_cache(1)
    def literal_variants() -> frozenset[hydra.variants.LiteralVariant]:
        return hydra.lib.sets.from_list((hydra.variants.LiteralVariant.BINARY, hydra.variants.LiteralVariant.BOOLEAN, hydra.variants.LiteralVariant.FLOAT, hydra.variants.LiteralVariant.INTEGER, hydra.variants.LiteralVariant.STRING))
    @lru_cache(1)
    def float_types() -> frozenset[hydra.core.FloatType]:
        return hydra.lib.sets.from_list((hydra.core.FloatType.BIGFLOAT, hydra.core.FloatType.FLOAT64))
    @lru_cache(1)
    def function_variants() -> frozenset[hydra.variants.FunctionVariant]:
        return hydra.lib.sets.from_list((hydra.variants.FunctionVariant.ELIMINATION, hydra.variants.FunctionVariant.LAMBDA, hydra.variants.FunctionVariant.PRIMITIVE))
    @lru_cache(1)
    def integer_types() -> frozenset[hydra.core.IntegerType]:
        return hydra.lib.sets.from_list((hydra.core.IntegerType.BIGINT,))
    @lru_cache(1)
    def term_variants() -> frozenset[hydra.variants.TermVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TermVariant.ANNOTATED, hydra.variants.TermVariant.APPLICATION, hydra.variants.TermVariant.EITHER, hydra.variants.TermVariant.FUNCTION, hydra.variants.TermVariant.LET, hydra.variants.TermVariant.LIST, hydra.variants.TermVariant.LITERAL, hydra.variants.TermVariant.MAP, hydra.variants.TermVariant.MAYBE, hydra.variants.TermVariant.PAIR, hydra.variants.TermVariant.RECORD, hydra.variants.TermVariant.SET, hydra.variants.TermVariant.TYPE_APPLICATION, hydra.variants.TermVariant.TYPE_LAMBDA, hydra.variants.TermVariant.UNION, hydra.variants.TermVariant.UNIT, hydra.variants.TermVariant.VARIABLE, hydra.variants.TermVariant.WRAP))
    @lru_cache(1)
    def type_variants() -> frozenset[hydra.variants.TypeVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TypeVariant.ANNOTATED, hydra.variants.TypeVariant.APPLICATION, hydra.variants.TypeVariant.EITHER, hydra.variants.TypeVariant.FUNCTION, hydra.variants.TypeVariant.FORALL, hydra.variants.TypeVariant.LIST, hydra.variants.TypeVariant.LITERAL, hydra.variants.TypeVariant.MAP, hydra.variants.TypeVariant.MAYBE, hydra.variants.TypeVariant.PAIR, hydra.variants.TypeVariant.RECORD, hydra.variants.TypeVariant.SET, hydra.variants.TypeVariant.UNION, hydra.variants.TypeVariant.UNIT, hydra.variants.TypeVariant.VARIABLE, hydra.variants.TypeVariant.WRAP))
    def type_predicate(_: T0) -> bool:
        return True
    return hydra.coders.Language(hydra.coders.LanguageName("hydra.ext.python"), hydra.coders.LanguageConstraints(elimination_variants(), literal_variants(), float_types(), function_variants(), integer_types(), term_variants(), type_variants(), type_predicate))

@lru_cache(1)
def python_reserved_words() -> frozenset[str]:
    r"""A set of reserved words in Python."""
    
    # Python keywords, as enumerated at https://docs.python.org/3.13/reference/lexical_analysis.html#keywords.
    python_keywords = ("False", "None", "True", "and", "as", "assert", "async", "await", "break", "class", "continue", "def", "del", "elif", "else", "except", "finally", "for", "from", "global", "if", "import", "in", "is", "lambda", "nonlocal", "not", "or", "pass", "raise", "return", "try", "while", "with", "yield")
    # Some additional keywords we reserve in order to avoid collision with built-in functions.
    python_built_in_functions = ("range",)
    # Reserved words which are specific to Hydra-Python.
    hydra_python_keywords = ("Node", "FrozenDict")
    return hydra.lib.sets.from_list(hydra.lib.lists.concat((python_keywords, python_built_in_functions, hydra_python_keywords)))
