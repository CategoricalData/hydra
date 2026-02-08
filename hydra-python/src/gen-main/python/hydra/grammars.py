# Note: this is an automatically generated file. Do not edit.

r"""A utility for converting a BNF grammar to a Hydra module."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Just, Maybe, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.constants
import hydra.core
import hydra.formatting
import hydra.grammar
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.module
import hydra.names

T0 = TypeVar("T0")

def child_name(lname: str, n: str) -> str:
    r"""Generate child name."""
    
    return hydra.lib.strings.cat((lname, "_", hydra.formatting.capitalize(n)))

def raw_name(pat: hydra.grammar.Pattern) -> str:
    r"""Get raw name from pattern."""
    
    match pat:
        case hydra.grammar.PatternAlternatives():
            return "alts"
        
        case hydra.grammar.PatternConstant(value=c):
            return hydra.formatting.capitalize(hydra.formatting.with_character_aliases(c.value))
        
        case hydra.grammar.PatternIgnored():
            return "ignored"
        
        case hydra.grammar.PatternLabeled(value=lp):
            return lp.label.value
        
        case hydra.grammar.PatternNil():
            return "none"
        
        case hydra.grammar.PatternNonterminal(value=s):
            return hydra.formatting.capitalize(s.value)
        
        case hydra.grammar.PatternOption(value=p):
            return hydra.formatting.capitalize(raw_name(p))
        
        case hydra.grammar.PatternPlus(value=p2):
            return hydra.lib.strings.cat2("listOf", hydra.formatting.capitalize(raw_name(p2)))
        
        case hydra.grammar.PatternRegex():
            return "regex"
        
        case hydra.grammar.PatternSequence():
            return "sequence"
        
        case hydra.grammar.PatternStar(value=p3):
            return hydra.lib.strings.cat2("listOf", hydra.formatting.capitalize(raw_name(p3)))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def find_names(pats: frozenlist[hydra.grammar.Pattern]) -> frozenlist[str]:
    r"""Find unique names for patterns."""
    
    def next_name(acc: tuple[frozenlist[str], FrozenDict[str, int]], pat: hydra.grammar.Pattern) -> tuple[frozenlist[str], FrozenDict[str, int]]:
        @lru_cache(1)
        def names() -> frozenlist[str]:
            return hydra.lib.pairs.first(acc)
        @lru_cache(1)
        def name_map() -> FrozenDict[str, int]:
            return hydra.lib.pairs.second(acc)
        @lru_cache(1)
        def rn() -> str:
            return raw_name(pat)
        @lru_cache(1)
        def name_and_index() -> tuple[str, int]:
            return hydra.lib.maybes.maybe((rn(), 1), (lambda i: (hydra.lib.strings.cat2(rn(), hydra.lib.literals.show_int32(hydra.lib.math.add(i, 1))), hydra.lib.math.add(i, 1))), hydra.lib.maps.lookup(rn(), name_map()))
        @lru_cache(1)
        def nn() -> str:
            return hydra.lib.pairs.first(name_and_index())
        @lru_cache(1)
        def ni() -> int:
            return hydra.lib.pairs.second(name_and_index())
        return (hydra.lib.lists.cons(nn(), names()), hydra.lib.maps.insert(rn(), ni(), name_map()))
    return hydra.lib.lists.reverse(hydra.lib.pairs.first(hydra.lib.lists.foldl((lambda x1, x2: next_name(x1, x2)), ((), hydra.lib.maps.empty()), pats)))

def simplify(is_record: bool, pats: frozenlist[hydra.grammar.Pattern]) -> frozenlist[hydra.grammar.Pattern]:
    r"""Remove trivial patterns from records."""
    
    def is_constant(p: hydra.grammar.Pattern) -> bool:
        match p:
            case hydra.grammar.PatternConstant():
                return True
            
            case _:
                return False
    return hydra.lib.logic.if_else(is_record, (lambda : hydra.lib.lists.filter((lambda p: hydra.lib.logic.not_(is_constant(p))), pats)), (lambda : pats))

def is_nontrivial(is_record: bool, pats: frozenlist[hydra.grammar.Pattern]) -> bool:
    r"""Check if patterns are nontrivial."""
    
    @lru_cache(1)
    def min_pats() -> frozenlist[hydra.grammar.Pattern]:
        return simplify(is_record, pats)
    def is_labeled(p: hydra.grammar.Pattern) -> bool:
        match p:
            case hydra.grammar.PatternLabeled():
                return True
            
            case _:
                return False
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(min_pats()), 1), (lambda : is_labeled(hydra.lib.lists.head(min_pats()))), (lambda : True))

def is_complex(pat: hydra.grammar.Pattern) -> bool:
    r"""Check if pattern is complex."""
    
    match pat:
        case hydra.grammar.PatternLabeled(value=lp):
            return is_complex(lp.pattern)
        
        case hydra.grammar.PatternSequence(value=pats):
            return is_nontrivial(True, pats)
        
        case hydra.grammar.PatternAlternatives(value=pats2):
            return is_nontrivial(False, pats2)
        
        case _:
            return False

def to_name(ns: hydra.module.Namespace, local: str) -> hydra.core.Name:
    r"""Convert local name to qualified name."""
    
    return hydra.names.unqualify_name(hydra.module.QualifiedName(Just(ns), local))

def make_elements(omit_trivial: bool, ns: hydra.module.Namespace, lname: str, pat: hydra.grammar.Pattern) -> frozenlist[tuple[str, hydra.core.Type]]:
    r"""Create elements from pattern."""
    
    @lru_cache(1)
    def trivial() -> frozenlist[tuple[str, hydra.core.Type]]:
        return hydra.lib.logic.if_else(omit_trivial, (lambda : ()), (lambda : ((lname, cast(hydra.core.Type, hydra.core.TypeUnit())),)))
    def descend(n: str, f: Callable[[frozenlist[tuple[str, hydra.core.Type]]], T0], p: hydra.grammar.Pattern) -> T0:
        @lru_cache(1)
        def cpairs() -> frozenlist[tuple[str, hydra.core.Type]]:
            return make_elements(False, ns, child_name(lname, n), p)
        return f(hydra.lib.logic.if_else(is_complex(p), (lambda : hydra.lib.lists.cons((lname, cast(hydra.core.Type, hydra.core.TypeVariable(to_name(ns, hydra.lib.pairs.first(hydra.lib.lists.head(cpairs())))))), cpairs())), (lambda : hydra.lib.logic.if_else(hydra.lib.lists.null(cpairs()), (lambda : ((lname, cast(hydra.core.Type, hydra.core.TypeUnit())),)), (lambda : hydra.lib.lists.cons((lname, hydra.lib.pairs.second(hydra.lib.lists.head(cpairs()))), hydra.lib.lists.tail(cpairs())))))))
    def mod(n: str, f: Callable[[hydra.core.Type], hydra.core.Type], p: hydra.grammar.Pattern) -> frozenlist[tuple[str, hydra.core.Type]]:
        return descend(n, (lambda pairs: hydra.lib.lists.cons((lname, f(hydra.lib.pairs.second(hydra.lib.lists.head(pairs)))), hydra.lib.lists.tail(pairs))), p)
    def for_pat(pat2: hydra.grammar.Pattern) -> frozenlist[tuple[str, hydra.core.Type]]:
        match pat2:
            case hydra.grammar.PatternAlternatives(value=pats):
                return for_record_or_union(False, (lambda fields: cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(hydra.constants.placeholder_name, fields)))), pats)
            
            case hydra.grammar.PatternConstant():
                return trivial()
            
            case hydra.grammar.PatternIgnored():
                return ()
            
            case hydra.grammar.PatternLabeled(value=lp):
                return for_pat(lp.pattern)
            
            case hydra.grammar.PatternNil():
                return trivial()
            
            case hydra.grammar.PatternNonterminal(value=s):
                return ((lname, cast(hydra.core.Type, hydra.core.TypeVariable(to_name(ns, s.value)))),)
            
            case hydra.grammar.PatternOption(value=p):
                return mod("Option", (lambda x: cast(hydra.core.Type, hydra.core.TypeMaybe(x))), p)
            
            case hydra.grammar.PatternPlus(value=p2):
                return mod("Elmt", (lambda x: cast(hydra.core.Type, hydra.core.TypeList(x))), p2)
            
            case hydra.grammar.PatternRegex():
                return ((lname, cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))),)
            
            case hydra.grammar.PatternSequence(value=pats2):
                return for_record_or_union(True, (lambda fields: cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(hydra.constants.placeholder_name, fields)))), pats2)
            
            case hydra.grammar.PatternStar(value=p3):
                return mod("Elmt", (lambda x: cast(hydra.core.Type, hydra.core.TypeList(x))), p3)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def for_record_or_union(is_record: bool, construct: Callable[[frozenlist[hydra.core.FieldType]], hydra.core.Type], pats: frozenlist[hydra.grammar.Pattern]) -> frozenlist[tuple[str, hydra.core.Type]]:
        @lru_cache(1)
        def min_pats() -> frozenlist[hydra.grammar.Pattern]:
            return simplify(is_record, pats)
        @lru_cache(1)
        def field_names() -> frozenlist[str]:
            return find_names(min_pats())
        def to_field(n: str, p: hydra.grammar.Pattern) -> tuple[hydra.core.FieldType, frozenlist[tuple[str, hydra.core.Type]]]:
            return descend(n, (lambda pairs: (hydra.core.FieldType(hydra.core.Name(n), hydra.lib.pairs.second(hydra.lib.lists.head(pairs))), hydra.lib.lists.tail(pairs))), p)
        @lru_cache(1)
        def field_pairs() -> frozenlist[tuple[hydra.core.FieldType, frozenlist[tuple[str, hydra.core.Type]]]]:
            return hydra.lib.lists.zip_with((lambda x1, x2: to_field(x1, x2)), field_names(), min_pats())
        @lru_cache(1)
        def fields() -> frozenlist[hydra.core.FieldType]:
            return hydra.lib.lists.map((lambda x1: hydra.lib.pairs.first(x1)), field_pairs())
        @lru_cache(1)
        def els() -> frozenlist[tuple[str, hydra.core.Type]]:
            return hydra.lib.lists.concat(hydra.lib.lists.map((lambda x1: hydra.lib.pairs.second(x1)), field_pairs()))
        return hydra.lib.logic.if_else(is_nontrivial(is_record, pats), (lambda : hydra.lib.lists.cons((lname, construct(fields())), els())), (lambda : for_pat(hydra.lib.lists.head(min_pats()))))
    return for_pat(pat)

def wrap_type(t: hydra.core.Type) -> hydra.core.Type:
    r"""Wrap a type in a placeholder name, unless it is already a wrapper, record, or union type."""
    
    match t:
        case hydra.core.TypeRecord():
            return t
        
        case hydra.core.TypeUnion():
            return t
        
        case hydra.core.TypeWrap():
            return t
        
        case _:
            return cast(hydra.core.Type, hydra.core.TypeWrap(hydra.core.WrappedType(hydra.core.Name("Placeholder"), t)))

def grammar_to_module(ns: hydra.module.Namespace, grammar: hydra.grammar.Grammar, desc: Maybe[str]) -> hydra.module.Module:
    r"""Convert a BNF grammar to a Hydra module."""
    
    @lru_cache(1)
    def prod_pairs() -> frozenlist[tuple[str, hydra.grammar.Pattern]]:
        return hydra.lib.lists.map((lambda prod: (prod.symbol.value, prod.pattern)), grammar.value)
    @lru_cache(1)
    def capitalized_names() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda pair: hydra.formatting.capitalize(hydra.lib.pairs.first(pair))), prod_pairs())
    @lru_cache(1)
    def patterns() -> frozenlist[hydra.grammar.Pattern]:
        return hydra.lib.lists.map((lambda pair: hydra.lib.pairs.second(pair)), prod_pairs())
    @lru_cache(1)
    def element_pairs() -> frozenlist[tuple[str, hydra.core.Type]]:
        return hydra.lib.lists.concat(hydra.lib.lists.zip_with((lambda v1, v2: make_elements(False, ns, v1, v2)), capitalized_names(), patterns()))
    @lru_cache(1)
    def elements() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.map((lambda pair: (lname := hydra.lib.pairs.first(pair), typ := wrap_type(hydra.lib.pairs.second(pair)), hydra.annotations.type_element(to_name(ns, lname), typ))[2]), element_pairs())
    return hydra.module.Module(ns, elements(), (), (), desc)
