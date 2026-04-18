# Note: this is an automatically generated file. Do not edit.

r"""Language constraints and reserved words for Rust."""

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
def rust_language() -> hydra.coders.Language:
    r"""Language constraints for Rust."""

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
        return hydra.lib.sets.from_list((hydra.variants.TermVariant.ANNOTATED, hydra.variants.TermVariant.APPLICATION, hydra.variants.TermVariant.EITHER, hydra.variants.TermVariant.CASES, hydra.variants.TermVariant.LAMBDA, hydra.variants.TermVariant.PROJECT, hydra.variants.TermVariant.UNWRAP, hydra.variants.TermVariant.TYPE_APPLICATION, hydra.variants.TermVariant.TYPE_LAMBDA, hydra.variants.TermVariant.LET, hydra.variants.TermVariant.LIST, hydra.variants.TermVariant.LITERAL, hydra.variants.TermVariant.MAP, hydra.variants.TermVariant.MAYBE, hydra.variants.TermVariant.PAIR, hydra.variants.TermVariant.RECORD, hydra.variants.TermVariant.SET, hydra.variants.TermVariant.INJECT, hydra.variants.TermVariant.UNIT, hydra.variants.TermVariant.VARIABLE, hydra.variants.TermVariant.WRAP))
    @lru_cache(1)
    def type_variants() -> frozenset[hydra.variants.TypeVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TypeVariant.ANNOTATED, hydra.variants.TypeVariant.APPLICATION, hydra.variants.TypeVariant.EITHER, hydra.variants.TypeVariant.FUNCTION, hydra.variants.TypeVariant.FORALL, hydra.variants.TypeVariant.LIST, hydra.variants.TypeVariant.LITERAL, hydra.variants.TypeVariant.MAP, hydra.variants.TypeVariant.MAYBE, hydra.variants.TypeVariant.PAIR, hydra.variants.TypeVariant.RECORD, hydra.variants.TypeVariant.SET, hydra.variants.TypeVariant.UNION, hydra.variants.TypeVariant.UNIT, hydra.variants.TypeVariant.VARIABLE, hydra.variants.TypeVariant.VOID, hydra.variants.TypeVariant.WRAP))
    def type_predicate(_: T0) -> bool:
        return True
    return hydra.coders.Language(hydra.coders.LanguageName("hydra.rust"), hydra.coders.LanguageConstraints(elimination_variants(), literal_variants(), float_types(), function_variants(), integer_types(), term_variants(), type_variants(), (lambda x1: type_predicate(x1))))

@lru_cache(1)
def rust_reserved_words() -> frozenset[str]:
    r"""A set of reserved words in Rust."""

    # Rust strict keywords (cannot be used as identifiers). See https://doc.rust-lang.org/reference/keywords.html.
    strict_keywords = ("as", "async", "await", "break", "const", "continue", "crate", "dyn", "else", "enum", "extern", "false", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return", "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where", "while")
    # Rust reserved keywords (reserved for future use). See https://doc.rust-lang.org/reference/keywords.html.
    reserved_keywords = ("abstract", "become", "box", "do", "final", "macro", "override", "priv", "try", "typeof", "unsized", "virtual", "yield")
    # Rust weak keywords (have special meaning in certain contexts). See https://doc.rust-lang.org/reference/keywords.html.
    weak_keywords = ("macro_rules", "union", "'static")
    # Rust primitive type names that we reserve to avoid confusion.
    primitive_types = ("bool", "char", "str", "u8", "u16", "u32", "u64", "u128", "usize", "i8", "i16", "i32", "i64", "i128", "isize", "f32", "f64")
    # Common types from std::prelude that we reserve to avoid shadowing.
    std_prelude_types = ("Option", "Some", "None", "Result", "Ok", "Err", "Box", "String", "Vec", "Clone", "Copy", "Default", "Drop", "Eq", "PartialEq", "Ord", "PartialOrd", "Hash", "Debug", "Display", "Iterator", "IntoIterator", "From", "Into", "AsRef", "AsMut", "Send", "Sync", "Sized", "Unpin", "Fn", "FnMut", "FnOnce")
    # Reserved words which are specific to Hydra-Rust code generation.
    hydra_rust_keywords = ("Node",)
    return hydra.lib.sets.from_list(hydra.lib.lists.concat((strict_keywords, reserved_keywords, weak_keywords, primitive_types, std_prelude_types, hydra_rust_keywords)))
