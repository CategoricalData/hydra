# Note: this is an automatically generated file. Do not edit.

r"""Language constraints and reserved words for Scala."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import frozenlist
from typing import TypeVar, cast
import hydra.coders
import hydra.core
import hydra.lib.lists
import hydra.lib.sets
import hydra.variants

T0 = TypeVar("T0")

@lru_cache(1)
def scala_language() -> hydra.coders.Language:
    r"""Language constraints for Scala."""

    @lru_cache(1)
    def elimination_variants() -> frozenset[hydra.variants.EliminationVariant]:
        return hydra.lib.sets.from_list((hydra.variants.EliminationVariant.RECORD, hydra.variants.EliminationVariant.UNION, hydra.variants.EliminationVariant.WRAP))
    @lru_cache(1)
    def literal_variants() -> frozenset[hydra.variants.LiteralVariant]:
        return hydra.lib.sets.from_list((hydra.variants.LiteralVariant.BOOLEAN, hydra.variants.LiteralVariant.FLOAT, hydra.variants.LiteralVariant.INTEGER, hydra.variants.LiteralVariant.STRING))
    @lru_cache(1)
    def float_types() -> frozenset[hydra.core.FloatType]:
        return hydra.lib.sets.from_list((hydra.core.FloatType.BIGFLOAT, hydra.core.FloatType.FLOAT32, hydra.core.FloatType.FLOAT64))
    @lru_cache(1)
    def function_variants() -> frozenset[hydra.variants.FunctionVariant]:
        return hydra.lib.sets.from_list((hydra.variants.FunctionVariant.ELIMINATION, hydra.variants.FunctionVariant.LAMBDA, hydra.variants.FunctionVariant.PRIMITIVE))
    @lru_cache(1)
    def integer_types() -> frozenset[hydra.core.IntegerType]:
        return hydra.lib.sets.from_list((hydra.core.IntegerType.BIGINT, hydra.core.IntegerType.INT8, hydra.core.IntegerType.INT16, hydra.core.IntegerType.INT32, hydra.core.IntegerType.INT64, hydra.core.IntegerType.UINT8, hydra.core.IntegerType.UINT16, hydra.core.IntegerType.UINT32, hydra.core.IntegerType.UINT64))
    @lru_cache(1)
    def term_variants() -> frozenset[hydra.variants.TermVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TermVariant.APPLICATION, hydra.variants.TermVariant.EITHER, hydra.variants.TermVariant.FUNCTION, hydra.variants.TermVariant.LET, hydra.variants.TermVariant.LIST, hydra.variants.TermVariant.LITERAL, hydra.variants.TermVariant.MAP, hydra.variants.TermVariant.MAYBE, hydra.variants.TermVariant.PAIR, hydra.variants.TermVariant.RECORD, hydra.variants.TermVariant.SET, hydra.variants.TermVariant.UNION, hydra.variants.TermVariant.UNIT, hydra.variants.TermVariant.VARIABLE, hydra.variants.TermVariant.WRAP))
    @lru_cache(1)
    def type_variants() -> frozenset[hydra.variants.TypeVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TypeVariant.ANNOTATED, hydra.variants.TypeVariant.APPLICATION, hydra.variants.TypeVariant.EITHER, hydra.variants.TypeVariant.FUNCTION, hydra.variants.TypeVariant.LIST, hydra.variants.TypeVariant.LITERAL, hydra.variants.TypeVariant.MAP, hydra.variants.TypeVariant.MAYBE, hydra.variants.TypeVariant.PAIR, hydra.variants.TypeVariant.RECORD, hydra.variants.TypeVariant.SET, hydra.variants.TypeVariant.UNION, hydra.variants.TypeVariant.UNIT, hydra.variants.TypeVariant.FORALL, hydra.variants.TypeVariant.VARIABLE, hydra.variants.TypeVariant.VOID, hydra.variants.TypeVariant.WRAP))
    def type_predicate(_: T0) -> bool:
        return True
    return hydra.coders.Language(hydra.coders.LanguageName("hydra.ext.scala"), hydra.coders.LanguageConstraints(elimination_variants(), literal_variants(), float_types(), function_variants(), integer_types(), term_variants(), type_variants(), (lambda x1: type_predicate(x1))))

@lru_cache(1)
def scala_reserved_words() -> frozenset[str]:
    r"""A set of reserved words in Scala."""

    # Scala keywords.
    keywords = ("abstract", "case", "catch", "class", "def", "do", "else", "end", "enum", "export", "extends", "false", "final", "finally", "for", "forSome", "given", "if", "implicit", "import", "lazy", "macro", "match", "new", "null", "object", "override", "package", "private", "protected", "return", "sealed", "super", "then", "this", "throw", "trait", "true", "try", "type", "val", "var", "while", "with", "yield")
    # Classes in the Scala Standard Library 2.13.8.
    class_names = ("Any", "AnyVal", "App", "Array", "Boolean", "Byte", "Char", "Console", "DelayedInit", "Double", "DummyExplicit", "Dynamic", "Enumeration", "Equals", "Float", "Function", "Int", "Long", "MatchError", "None", "Nothing", "Null", "Option", "PartialFunction", "Predef", "Product", "Proxy", "SerialVersionUID", "Short", "Singleton", "Some", "Specializable", "StringContext", "Symbol", "Unit", "ValueOf")
    @lru_cache(1)
    def hydra_scala_keywords() -> frozenlist[T0]:
        r"""Reserved words which are specific to Hydra."""

        return ()
    return hydra.lib.sets.from_list(hydra.lib.lists.concat((keywords, class_names, hydra_scala_keywords())))
