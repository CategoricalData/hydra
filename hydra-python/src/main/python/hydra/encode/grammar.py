# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.grammar."""

from __future__ import annotations
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.core
import hydra.grammar
import hydra.lib.lists

def constant(x: hydra.grammar.Constant) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.grammar.Constant"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.value))))

def label(x: hydra.grammar.Label) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.grammar.Label"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.value))))

def regex(x: hydra.grammar.Regex) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.grammar.Regex"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.value))))

def symbol(x: hydra.grammar.Symbol) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.grammar.Symbol"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.value))))

def labeled_pattern(x: hydra.grammar.LabeledPattern) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.grammar.LabeledPattern"), (hydra.core.Field(hydra.core.Name("label"), label(x.label)), hydra.core.Field(hydra.core.Name("pattern"), pattern(x.pattern))))))

def pattern(v1: hydra.grammar.Pattern) -> hydra.core.Type:
    match v1:
        case hydra.grammar.PatternAlternatives(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("alternatives"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(pattern, xs))))(v)))))
        
        case hydra.grammar.PatternConstant(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("constant"), constant(v2)))))
        
        case hydra.grammar.PatternIgnored(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("ignored"), pattern(v3)))))
        
        case hydra.grammar.PatternLabeled(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("labeled"), labeled_pattern(v4)))))
        
        case hydra.grammar.PatternNil():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("nil"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.grammar.PatternNonterminal(value=v6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("nonterminal"), symbol(v6)))))
        
        case hydra.grammar.PatternOption(value=v7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("option"), pattern(v7)))))
        
        case hydra.grammar.PatternPlus(value=v8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("plus"), pattern(v8)))))
        
        case hydra.grammar.PatternRegex(value=v9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("regex"), regex(v9)))))
        
        case hydra.grammar.PatternSequence(value=v10):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("sequence"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(pattern, xs))))(v10)))))
        
        case hydra.grammar.PatternStar(value=v11):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("star"), pattern(v11)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def production(x: hydra.grammar.Production) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.grammar.Production"), (hydra.core.Field(hydra.core.Name("symbol"), symbol(x.symbol)), hydra.core.Field(hydra.core.Name("pattern"), pattern(x.pattern))))))

def grammar(x: hydra.grammar.Grammar) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.grammar.Grammar"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(production, xs))))(x.value))))
