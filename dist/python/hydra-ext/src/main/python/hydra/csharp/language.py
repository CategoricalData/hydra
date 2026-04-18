# Note: this is an automatically generated file. Do not edit.

r"""Language constraints and reserved words for C Sharp (C#)."""

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
def csharp_language() -> hydra.coders.Language:
    r"""Language constraints for C Sharp (C#)."""

    @lru_cache(1)
    def elimination_variants() -> frozenset[hydra.variants.EliminationVariant]:
        return hydra.lib.sets.from_list((hydra.variants.EliminationVariant.RECORD, hydra.variants.EliminationVariant.UNION, hydra.variants.EliminationVariant.WRAP))
    @lru_cache(1)
    def literal_variants() -> frozenset[hydra.variants.LiteralVariant]:
        return hydra.lib.sets.from_list((hydra.variants.LiteralVariant.BINARY, hydra.variants.LiteralVariant.BOOLEAN, hydra.variants.LiteralVariant.FLOAT, hydra.variants.LiteralVariant.INTEGER, hydra.variants.LiteralVariant.STRING))
    @lru_cache(1)
    def float_types() -> frozenset[hydra.core.FloatType]:
        return hydra.lib.sets.from_list((hydra.core.FloatType.FLOAT32, hydra.core.FloatType.FLOAT64))
    @lru_cache(1)
    def function_variants() -> frozenset[hydra.variants.FunctionVariant]:
        return hydra.lib.sets.from_list((hydra.variants.FunctionVariant.ELIMINATION, hydra.variants.FunctionVariant.LAMBDA))
    @lru_cache(1)
    def integer_types() -> frozenset[hydra.core.IntegerType]:
        return hydra.lib.sets.from_list((hydra.core.IntegerType.INT8, hydra.core.IntegerType.INT16, hydra.core.IntegerType.INT32, hydra.core.IntegerType.INT64, hydra.core.IntegerType.UINT8, hydra.core.IntegerType.UINT16, hydra.core.IntegerType.UINT32, hydra.core.IntegerType.UINT64))
    @lru_cache(1)
    def term_variants() -> frozenset[hydra.variants.TermVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TermVariant.APPLICATION, hydra.variants.TermVariant.CASES, hydra.variants.TermVariant.LAMBDA, hydra.variants.TermVariant.PROJECT, hydra.variants.TermVariant.UNWRAP, hydra.variants.TermVariant.TYPE_APPLICATION, hydra.variants.TermVariant.TYPE_LAMBDA, hydra.variants.TermVariant.LET, hydra.variants.TermVariant.LIST, hydra.variants.TermVariant.LITERAL, hydra.variants.TermVariant.MAP, hydra.variants.TermVariant.MAYBE, hydra.variants.TermVariant.RECORD, hydra.variants.TermVariant.SET, hydra.variants.TermVariant.INJECT, hydra.variants.TermVariant.VARIABLE, hydra.variants.TermVariant.WRAP))
    @lru_cache(1)
    def type_variants() -> frozenset[hydra.variants.TypeVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TypeVariant.ANNOTATED, hydra.variants.TypeVariant.APPLICATION, hydra.variants.TypeVariant.FUNCTION, hydra.variants.TypeVariant.FORALL, hydra.variants.TypeVariant.LIST, hydra.variants.TypeVariant.LITERAL, hydra.variants.TypeVariant.MAP, hydra.variants.TypeVariant.MAYBE, hydra.variants.TypeVariant.RECORD, hydra.variants.TypeVariant.SET, hydra.variants.TypeVariant.UNION, hydra.variants.TypeVariant.VARIABLE, hydra.variants.TypeVariant.WRAP))
    def type_predicate(_: T0) -> bool:
        return True
    return hydra.coders.Language(hydra.coders.LanguageName("hydra.csharp"), hydra.coders.LanguageConstraints(elimination_variants(), literal_variants(), float_types(), function_variants(), integer_types(), term_variants(), type_variants(), (lambda x1: type_predicate(x1))))

@lru_cache(1)
def csharp_reserved_words() -> frozenset[str]:
    r"""A set of reserved words in C#. Both the "keywords" and "contextual keywords" are drawn from section 6.4.4 of the C# documentation:
    https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#64-tokens."""

    keywords = ("DEFAULT", "FALSE", "NULL", "TRUE", "abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked", "class", "const", "continue", "decimal", "delegate", "do", "double", "else", "enum", "event", "explicit", "extern", "finally", "fixed", "float", "for", "foreach", "goto", "if", "implicit", "in", "int", "interface", "internal", "is", "lock", "long", "namespace", "new", "object", "operator", "out", "override", "params", "private", "protected", "public", "readonly", "ref", "return", "sbyte", "sealed", "short", "sizeof", "stackalloc", "static", "string", "struct", "switch", "this", "throw", "try", "typeof", "uint", "ulong", "unchecked", "unsafe", "ushort", "using", "virtual", "void", "volatile", "while")
    contextual_keywords = ("add", "alias", "ascending", "async", "await", "by", "descending", "dynamic", "equals", "from", "get", "global", "group", "into", "join", "let", "nameof", "on", "orderby", "partial", "remove", "select", "set", "unmanaged", "value", "var", "when", "where", "yield")
    return hydra.lib.sets.from_list(hydra.lib.lists.concat((keywords, contextual_keywords)))
