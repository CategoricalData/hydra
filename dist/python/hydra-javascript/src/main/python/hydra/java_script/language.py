# Note: this is an automatically generated file. Do not edit.

r"""Language constraints and reserved words for JavaScript (ECMAScript 2024)."""

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
def java_script_language() -> hydra.coders.Language:
    r"""Language constraints for JavaScript (ECMAScript 2024)."""

    @lru_cache(1)
    def elimination_variants() -> frozenset[hydra.variants.EliminationVariant]:
        return hydra.lib.sets.from_list((hydra.variants.EliminationVariant.RECORD, hydra.variants.EliminationVariant.UNION, hydra.variants.EliminationVariant.WRAP))
    @lru_cache(1)
    def literal_variants() -> frozenset[hydra.variants.LiteralVariant]:
        return hydra.lib.sets.from_list((hydra.variants.LiteralVariant.BINARY, hydra.variants.LiteralVariant.BOOLEAN, hydra.variants.LiteralVariant.FLOAT, hydra.variants.LiteralVariant.INTEGER, hydra.variants.LiteralVariant.STRING))
    @lru_cache(1)
    def float_types() -> frozenset[hydra.core.FloatType]:
        return hydra.lib.sets.from_list((hydra.core.FloatType.FLOAT64,))
    @lru_cache(1)
    def function_variants() -> frozenset[hydra.variants.FunctionVariant]:
        return hydra.lib.sets.from_list((hydra.variants.FunctionVariant.ELIMINATION, hydra.variants.FunctionVariant.LAMBDA))
    @lru_cache(1)
    def integer_types() -> frozenset[hydra.core.IntegerType]:
        return hydra.lib.sets.from_list((hydra.core.IntegerType.INT32, hydra.core.IntegerType.BIGINT))
    @lru_cache(1)
    def term_variants() -> frozenset[hydra.variants.TermVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TermVariant.ANNOTATED, hydra.variants.TermVariant.APPLICATION, hydra.variants.TermVariant.EITHER, hydra.variants.TermVariant.CASES, hydra.variants.TermVariant.LAMBDA, hydra.variants.TermVariant.PROJECT, hydra.variants.TermVariant.UNWRAP, hydra.variants.TermVariant.LET, hydra.variants.TermVariant.LIST, hydra.variants.TermVariant.LITERAL, hydra.variants.TermVariant.MAP, hydra.variants.TermVariant.MAYBE, hydra.variants.TermVariant.PAIR, hydra.variants.TermVariant.RECORD, hydra.variants.TermVariant.SET, hydra.variants.TermVariant.TYPE_APPLICATION, hydra.variants.TermVariant.TYPE_LAMBDA, hydra.variants.TermVariant.INJECT, hydra.variants.TermVariant.UNIT, hydra.variants.TermVariant.VARIABLE, hydra.variants.TermVariant.WRAP))
    @lru_cache(1)
    def type_variants() -> frozenset[hydra.variants.TypeVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TypeVariant.ANNOTATED, hydra.variants.TypeVariant.APPLICATION, hydra.variants.TypeVariant.EITHER, hydra.variants.TypeVariant.FUNCTION, hydra.variants.TypeVariant.FORALL, hydra.variants.TypeVariant.LIST, hydra.variants.TypeVariant.LITERAL, hydra.variants.TypeVariant.MAP, hydra.variants.TypeVariant.MAYBE, hydra.variants.TypeVariant.PAIR, hydra.variants.TypeVariant.RECORD, hydra.variants.TypeVariant.SET, hydra.variants.TypeVariant.UNION, hydra.variants.TypeVariant.UNIT, hydra.variants.TypeVariant.VARIABLE, hydra.variants.TypeVariant.VOID, hydra.variants.TypeVariant.WRAP))
    def type_predicate(_: T0) -> bool:
        return True
    return hydra.coders.Language(hydra.coders.LanguageName("hydra.javaScript"), hydra.coders.LanguageConstraints(elimination_variants(), literal_variants(), float_types(), function_variants(), integer_types(), term_variants(), type_variants(), (lambda x1: type_predicate(x1))))

@lru_cache(1)
def java_script_reserved_words() -> frozenset[str]:
    r"""A set of reserved words in JavaScript."""

    # JavaScript keywords per ECMAScript 2024 specification.
    keywords = ("await", "break", "case", "catch", "class", "const", "continue", "debugger", "default", "delete", "do", "else", "enum", "export", "extends", "false", "finally", "for", "function", "if", "import", "in", "instanceof", "let", "new", "null", "return", "super", "switch", "this", "throw", "true", "try", "typeof", "undefined", "var", "void", "while", "with", "yield")
    # Future reserved words (strict mode).
    future_reserved = ("implements", "interface", "package", "private", "protected", "public")
    # Additional words reserved in strict mode.
    strict_mode_reserved = ("arguments", "eval")
    # Common built-in identifiers we should avoid.
    built_ins = ("Array", "ArrayBuffer", "BigInt", "Boolean", "DataView", "Date", "Error", "Float32Array", "Float64Array", "Function", "Int8Array", "Int16Array", "Int32Array", "JSON", "Map", "Math", "Number", "Object", "Promise", "Proxy", "Reflect", "RegExp", "Set", "String", "Symbol", "Uint8Array", "Uint8ClampedArray", "Uint16Array", "Uint32Array", "WeakMap", "WeakSet", "decodeURI", "decodeURIComponent", "encodeURI", "encodeURIComponent", "eval", "isFinite", "isNaN", "parseFloat", "parseInt", "console", "document", "global", "globalThis", "module", "process", "require", "window")
    # Reserved words specific to Hydra-JavaScript.
    hydra_java_script_keywords = ("Name", "FrozenMap", "TERM_ANNOTATED", "TERM_APPLICATION", "TERM_EITHER", "TERM_FUNCTION", "TERM_LET", "TERM_LIST", "TERM_LITERAL", "TERM_MAP", "TERM_MAYBE", "TERM_PAIR", "TERM_RECORD", "TERM_SET", "TERM_TYPE_APPLICATION", "TERM_TYPE_LAMBDA", "TERM_UNION", "TERM_UNIT", "TERM_VARIABLE", "TERM_WRAP")
    return hydra.lib.sets.from_list(hydra.lib.lists.concat((keywords, future_reserved, strict_mode_reserved, built_ins, hydra_java_script_keywords)))
