# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.grammar."""

from __future__ import annotations
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.core
import hydra.grammar
import hydra.lib.lists

def constant(x: hydra.grammar.Constant) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.grammar.Constant"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def label(x: hydra.grammar.Label) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.grammar.Label"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def regex(x: hydra.grammar.Regex) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.grammar.Regex"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def symbol(x: hydra.grammar.Symbol) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.grammar.Symbol"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def labeled_pattern(x: hydra.grammar.LabeledPattern) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.grammar.LabeledPattern"), (hydra.core.Field(hydra.core.Name("label"), label(x.label)), hydra.core.Field(hydra.core.Name("pattern"), pattern(x.pattern))))))

def pattern(v1: hydra.grammar.Pattern) -> hydra.core.Type:
    match v1:
        case hydra.grammar.PatternAlternatives(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("alternatives"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(pattern, y)))))))
        
        case hydra.grammar.PatternConstant(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("constant"), constant(y2)))))
        
        case hydra.grammar.PatternIgnored(value=y3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("ignored"), pattern(y3)))))
        
        case hydra.grammar.PatternLabeled(value=y4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("labeled"), labeled_pattern(y4)))))
        
        case hydra.grammar.PatternNil():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("nil"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.grammar.PatternNonterminal(value=y6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("nonterminal"), symbol(y6)))))
        
        case hydra.grammar.PatternOption(value=y7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("option"), pattern(y7)))))
        
        case hydra.grammar.PatternPlus(value=y8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("plus"), pattern(y8)))))
        
        case hydra.grammar.PatternRegex(value=y9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("regex"), regex(y9)))))
        
        case hydra.grammar.PatternSequence(value=y10):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("sequence"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(pattern, y10)))))))
        
        case hydra.grammar.PatternStar(value=y11):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.grammar.Pattern"), hydra.core.Field(hydra.core.Name("star"), pattern(y11)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def production(x: hydra.grammar.Production) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.grammar.Production"), (hydra.core.Field(hydra.core.Name("symbol"), symbol(x.symbol)), hydra.core.Field(hydra.core.Name("pattern"), pattern(x.pattern))))))

def grammar(x: hydra.grammar.Grammar) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.grammar.Grammar"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(production, x.value))))))
