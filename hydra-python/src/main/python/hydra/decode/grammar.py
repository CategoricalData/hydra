# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.grammar."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import cast
import hydra.core
import hydra.extract.helpers
import hydra.grammar
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.util

def constant(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.grammar.Constant]:
    def _hoist_hydra_decode_grammar_constant_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.LiteralString(value=s):
                return cast(Either[hydra.util.DecodingError, str], Right(s))
            
            case _:
                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
    def _hoist_hydra_decode_grammar_constant_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_grammar_constant_1(v)
            
            case _:
                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
    def _hoist_hydra_decode_grammar_constant_3(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.grammar.Constant]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.grammar.Constant(b)), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_hydra_decode_grammar_constant_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, wrapped_term.body))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.grammar.Constant], Left(hydra.util.DecodingError("expected wrapped type hydra.grammar.Constant")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.grammar.Constant], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_grammar_constant_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def label(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.grammar.Label]:
    def _hoist_hydra_decode_grammar_label_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.LiteralString(value=s):
                return cast(Either[hydra.util.DecodingError, str], Right(s))
            
            case _:
                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
    def _hoist_hydra_decode_grammar_label_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_grammar_label_1(v)
            
            case _:
                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
    def _hoist_hydra_decode_grammar_label_3(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.grammar.Label]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.grammar.Label(b)), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_hydra_decode_grammar_label_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, wrapped_term.body))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.grammar.Label], Left(hydra.util.DecodingError("expected wrapped type hydra.grammar.Label")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.grammar.Label], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_grammar_label_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def regex(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.grammar.Regex]:
    def _hoist_hydra_decode_grammar_regex_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.LiteralString(value=s):
                return cast(Either[hydra.util.DecodingError, str], Right(s))
            
            case _:
                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
    def _hoist_hydra_decode_grammar_regex_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_grammar_regex_1(v)
            
            case _:
                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
    def _hoist_hydra_decode_grammar_regex_3(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.grammar.Regex]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.grammar.Regex(b)), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_hydra_decode_grammar_regex_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, wrapped_term.body))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.grammar.Regex], Left(hydra.util.DecodingError("expected wrapped type hydra.grammar.Regex")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.grammar.Regex], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_grammar_regex_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def symbol(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.grammar.Symbol]:
    def _hoist_hydra_decode_grammar_symbol_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.LiteralString(value=s):
                return cast(Either[hydra.util.DecodingError, str], Right(s))
            
            case _:
                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
    def _hoist_hydra_decode_grammar_symbol_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_grammar_symbol_1(v)
            
            case _:
                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
    def _hoist_hydra_decode_grammar_symbol_3(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.grammar.Symbol]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.grammar.Symbol(b)), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_hydra_decode_grammar_symbol_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, wrapped_term.body))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.grammar.Symbol], Left(hydra.util.DecodingError("expected wrapped type hydra.grammar.Symbol")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.grammar.Symbol], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_grammar_symbol_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def labeled_pattern(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.grammar.LabeledPattern]:
    def _hoist_hydra_decode_grammar_labeled_pattern_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.grammar.LabeledPattern]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("label", label, field_map, cx), (lambda field_label: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("pattern", pattern, field_map, cx), (lambda field_pattern: cast(Either[hydra.util.DecodingError, hydra.grammar.LabeledPattern], Right(hydra.grammar.LabeledPattern(field_label, field_pattern)))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.grammar.LabeledPattern], Left(hydra.util.DecodingError("expected record of type hydra.grammar.LabeledPattern")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.grammar.LabeledPattern], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_grammar_labeled_pattern_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def pattern(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.grammar.Pattern]:
    def _hoist_hydra_decode_grammar_pattern_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.grammar.Pattern]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                tname = inj.type_name
                field = inj.field
                fname = field.name
                fterm = field.term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.grammar.Pattern]]]:
                    return cast(FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.grammar.Pattern]]], hydra.lib.maps.from_list((cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.grammar.Pattern]]], (hydra.core.Name("alternatives"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.grammar.Pattern, hydra.grammar.PatternAlternatives(t))), hydra.extract.helpers.decode_list(pattern, cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.grammar.Pattern]]], (hydra.core.Name("constant"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.grammar.Pattern, hydra.grammar.PatternConstant(t))), constant(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.grammar.Pattern]]], (hydra.core.Name("ignored"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.grammar.Pattern, hydra.grammar.PatternIgnored(t))), pattern(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.grammar.Pattern]]], (hydra.core.Name("labeled"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.grammar.Pattern, hydra.grammar.PatternLabeled(t))), labeled_pattern(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.grammar.Pattern]]], (hydra.core.Name("nil"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.grammar.Pattern, hydra.grammar.PatternNil())), hydra.extract.helpers.decode_unit(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.grammar.Pattern]]], (hydra.core.Name("nonterminal"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.grammar.Pattern, hydra.grammar.PatternNonterminal(t))), symbol(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.grammar.Pattern]]], (hydra.core.Name("option"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.grammar.Pattern, hydra.grammar.PatternOption(t))), pattern(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.grammar.Pattern]]], (hydra.core.Name("plus"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.grammar.Pattern, hydra.grammar.PatternPlus(t))), pattern(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.grammar.Pattern]]], (hydra.core.Name("regex"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.grammar.Pattern, hydra.grammar.PatternRegex(t))), regex(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.grammar.Pattern]]], (hydra.core.Name("sequence"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.grammar.Pattern, hydra.grammar.PatternSequence(t))), hydra.extract.helpers.decode_list(pattern, cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.grammar.Pattern]]], (hydra.core.Name("star"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.grammar.Pattern, hydra.grammar.PatternStar(t))), pattern(cx, input))))))))
                return hydra.lib.maybes.maybe(cast(Either[hydra.util.DecodingError, hydra.grammar.Pattern], Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union type ", tname.value))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.grammar.Pattern], Left(hydra.util.DecodingError("expected union of type hydra.grammar.Pattern")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.grammar.Pattern], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_grammar_pattern_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def production(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.grammar.Production]:
    def _hoist_hydra_decode_grammar_production_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.grammar.Production]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("symbol", symbol, field_map, cx), (lambda field_symbol: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("pattern", pattern, field_map, cx), (lambda field_pattern: cast(Either[hydra.util.DecodingError, hydra.grammar.Production], Right(hydra.grammar.Production(field_symbol, field_pattern)))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.grammar.Production], Left(hydra.util.DecodingError("expected record of type hydra.grammar.Production")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.grammar.Production], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_grammar_production_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def grammar(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.grammar.Grammar]:
    def _hoist_hydra_decode_grammar_grammar_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.grammar.Grammar]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.grammar.Grammar(b)), hydra.extract.helpers.decode_list(production, cx, wrapped_term.body))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.grammar.Grammar], Left(hydra.util.DecodingError("expected wrapped type hydra.grammar.Grammar")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.grammar.Grammar], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_grammar_grammar_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
