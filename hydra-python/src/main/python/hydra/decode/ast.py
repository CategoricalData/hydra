# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.ast."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Right
from typing import cast
import hydra.ast
import hydra.core
import hydra.extract.helpers
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.util

def associativity(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Associativity]:
    def _hoist_hydra_decode_ast_associativity_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Associativity]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                tname = inj.type_name
                field = inj.field
                fname = field.name
                fterm = field.term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Associativity]]]:
                    return cast(FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Associativity]]], hydra.lib.maps.from_list((cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Associativity]]], (hydra.core.Name("none"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.ast.Associativity.NONE), hydra.extract.helpers.decode_unit(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Associativity]]], (hydra.core.Name("left"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.ast.Associativity.LEFT), hydra.extract.helpers.decode_unit(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Associativity]]], (hydra.core.Name("right"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.ast.Associativity.RIGHT), hydra.extract.helpers.decode_unit(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Associativity]]], (hydra.core.Name("both"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.ast.Associativity.BOTH), hydra.extract.helpers.decode_unit(cx, input))))))))
                return hydra.lib.maybes.maybe(cast(Either[hydra.util.DecodingError, hydra.ast.Associativity], Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union type ", tname.value))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.ast.Associativity], Left(hydra.util.DecodingError("expected union of type hydra.ast.Associativity")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.Associativity], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_ast_associativity_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def block_style(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.BlockStyle]:
    def _hoist_hydra_decode_ast_block_style_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.BlockStyle]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.LiteralString(value=s):
                            return cast(Either[hydra.util.DecodingError, str], Right(s))
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
                def _hoist_body_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
                def _hoist_body_3(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, bool]:
                    match v1:
                        case hydra.core.LiteralBoolean(value=b):
                            return cast(Either[hydra.util.DecodingError, bool], Right(b))
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, bool], Left(hydra.util.DecodingError("expected boolean literal")))
                def _hoist_body_4(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, bool]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_3(v)
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, bool], Left(hydra.util.DecodingError("expected literal")))
                def _hoist_body_5(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, bool]:
                    match v1:
                        case hydra.core.LiteralBoolean(value=b):
                            return cast(Either[hydra.util.DecodingError, bool], Right(b))
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, bool], Left(hydra.util.DecodingError("expected boolean literal")))
                def _hoist_body_6(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, bool]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_5(v)
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, bool], Left(hydra.util.DecodingError("expected literal")))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("indent", (lambda v1, v2: hydra.extract.helpers.decode_maybe((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v1, v2)), field_map, cx), (lambda field_indent: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("newlineBeforeContent", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, bool], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_body_4(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map, cx), (lambda field_newline_before_content: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("newlineAfterContent", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, bool], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_body_6(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map, cx), (lambda field_newline_after_content: cast(Either[hydra.util.DecodingError, hydra.ast.BlockStyle], Right(hydra.ast.BlockStyle(field_indent, field_newline_before_content, field_newline_after_content)))))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.ast.BlockStyle], Left(hydra.util.DecodingError("expected record of type hydra.ast.BlockStyle")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.BlockStyle], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_ast_block_style_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def symbol(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Symbol]:
    def _hoist_hydra_decode_ast_symbol_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.LiteralString(value=s):
                return cast(Either[hydra.util.DecodingError, str], Right(s))
            
            case _:
                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
    def _hoist_hydra_decode_ast_symbol_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_ast_symbol_1(v)
            
            case _:
                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
    def _hoist_hydra_decode_ast_symbol_3(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Symbol]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.ast.Symbol(b)), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_hydra_decode_ast_symbol_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, wrapped_term.body))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.ast.Symbol], Left(hydra.util.DecodingError("expected wrapped type hydra.ast.Symbol")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.Symbol], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_ast_symbol_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def brackets(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Brackets]:
    def _hoist_hydra_decode_ast_brackets_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Brackets]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("open", symbol, field_map, cx), (lambda field_open: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("close", symbol, field_map, cx), (lambda field_close: cast(Either[hydra.util.DecodingError, hydra.ast.Brackets], Right(hydra.ast.Brackets(field_open, field_close)))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.ast.Brackets], Left(hydra.util.DecodingError("expected record of type hydra.ast.Brackets")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.Brackets], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_ast_brackets_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def indent_style(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.IndentStyle]:
    def _hoist_hydra_decode_ast_indent_style_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.IndentStyle]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                tname = inj.type_name
                field = inj.field
                fname = field.name
                fterm = field.term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.IndentStyle]]]:
                    def _hoist_variant_map_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                        match v1:
                            case hydra.core.LiteralString(value=s):
                                return cast(Either[hydra.util.DecodingError, str], Right(s))
                            
                            case _:
                                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
                    def _hoist_variant_map_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_1(v)
                            
                            case _:
                                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
                    def _hoist_variant_map_3(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                        match v1:
                            case hydra.core.LiteralString(value=s):
                                return cast(Either[hydra.util.DecodingError, str], Right(s))
                            
                            case _:
                                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
                    def _hoist_variant_map_4(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_3(v)
                            
                            case _:
                                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
                    return cast(FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.IndentStyle]]], hydra.lib.maps.from_list((cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.IndentStyle]]], (hydra.core.Name("allLines"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.IndentStyle, hydra.ast.IndentStyleAllLines(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_variant_map_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.IndentStyle]]], (hydra.core.Name("subsequentLines"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.IndentStyle, hydra.ast.IndentStyleSubsequentLines(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_variant_map_4(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input))))))))
                return hydra.lib.maybes.maybe(cast(Either[hydra.util.DecodingError, hydra.ast.IndentStyle], Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union type ", tname.value))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.ast.IndentStyle], Left(hydra.util.DecodingError("expected union of type hydra.ast.IndentStyle")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.IndentStyle], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_ast_indent_style_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def ws(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Ws]:
    def _hoist_hydra_decode_ast_ws_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Ws]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                tname = inj.type_name
                field = inj.field
                fname = field.name
                fterm = field.term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Ws]]]:
                    def _hoist_variant_map_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                        match v1:
                            case hydra.core.LiteralString(value=s):
                                return cast(Either[hydra.util.DecodingError, str], Right(s))
                            
                            case _:
                                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
                    def _hoist_variant_map_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_1(v)
                            
                            case _:
                                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
                    return cast(FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Ws]]], hydra.lib.maps.from_list((cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Ws]]], (hydra.core.Name("none"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.extract.helpers.decode_unit(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Ws]]], (hydra.core.Name("space"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Ws, hydra.ast.WsSpace())), hydra.extract.helpers.decode_unit(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Ws]]], (hydra.core.Name("break"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Ws, hydra.ast.WsBreak())), hydra.extract.helpers.decode_unit(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Ws]]], (hydra.core.Name("breakAndIndent"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Ws, hydra.ast.WsBreakAndIndent(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_variant_map_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Ws]]], (hydra.core.Name("doubleBreak"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Ws, hydra.ast.WsDoubleBreak())), hydra.extract.helpers.decode_unit(cx, input))))))))
                return hydra.lib.maybes.maybe(cast(Either[hydra.util.DecodingError, hydra.ast.Ws], Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union type ", tname.value))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.ast.Ws], Left(hydra.util.DecodingError("expected union of type hydra.ast.Ws")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.Ws], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_ast_ws_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def padding(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Padding]:
    def _hoist_hydra_decode_ast_padding_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Padding]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("left", ws, field_map, cx), (lambda field_left: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("right", ws, field_map, cx), (lambda field_right: cast(Either[hydra.util.DecodingError, hydra.ast.Padding], Right(hydra.ast.Padding(field_left, field_right)))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.ast.Padding], Left(hydra.util.DecodingError("expected record of type hydra.ast.Padding")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.Padding], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_ast_padding_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def precedence(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Precedence]:
    def _hoist_hydra_decode_ast_precedence_1(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
        match v1:
            case hydra.core.IntegerValueInt32(value=i):
                return cast(Either[hydra.util.DecodingError, int], Right(i))
            
            case _:
                return cast(Either[hydra.util.DecodingError, int], Left(hydra.util.DecodingError("expected int32 value")))
    def _hoist_hydra_decode_ast_precedence_2(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
        match v1:
            case hydra.core.LiteralInteger(value=v1):
                return _hoist_hydra_decode_ast_precedence_1(v1)
            
            case _:
                return cast(Either[hydra.util.DecodingError, int], Left(hydra.util.DecodingError("expected int32 literal")))
    def _hoist_hydra_decode_ast_precedence_3(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_ast_precedence_2(v)
            
            case _:
                return cast(Either[hydra.util.DecodingError, int], Left(hydra.util.DecodingError("expected literal")))
    def _hoist_hydra_decode_ast_precedence_4(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Precedence]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.ast.Precedence(b)), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, int], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_hydra_decode_ast_precedence_3(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, wrapped_term.body))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.ast.Precedence], Left(hydra.util.DecodingError("expected wrapped type hydra.ast.Precedence")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.Precedence], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_ast_precedence_4(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def op(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Op]:
    def _hoist_hydra_decode_ast_op_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Op]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("symbol", symbol, field_map, cx), (lambda field_symbol: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("padding", padding, field_map, cx), (lambda field_padding: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("precedence", precedence, field_map, cx), (lambda field_precedence: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("associativity", associativity, field_map, cx), (lambda field_associativity: cast(Either[hydra.util.DecodingError, hydra.ast.Op], Right(hydra.ast.Op(field_symbol, field_padding, field_precedence, field_associativity)))))))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.ast.Op], Left(hydra.util.DecodingError("expected record of type hydra.ast.Op")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.Op], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_ast_op_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def bracket_expr(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.BracketExpr]:
    def _hoist_hydra_decode_ast_bracket_expr_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.BracketExpr]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("brackets", brackets, field_map, cx), (lambda field_brackets: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("enclosed", expr, field_map, cx), (lambda field_enclosed: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("style", block_style, field_map, cx), (lambda field_style: cast(Either[hydra.util.DecodingError, hydra.ast.BracketExpr], Right(hydra.ast.BracketExpr(field_brackets, field_enclosed, field_style)))))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.ast.BracketExpr], Left(hydra.util.DecodingError("expected record of type hydra.ast.BracketExpr")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.BracketExpr], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_ast_bracket_expr_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def expr(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Expr]:
    def _hoist_hydra_decode_ast_expr_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Expr]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                tname = inj.type_name
                field = inj.field
                fname = field.name
                fterm = field.term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Expr]]]:
                    return cast(FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Expr]]], hydra.lib.maps.from_list((cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Expr]]], (hydra.core.Name("const"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Expr, hydra.ast.ExprConst(t))), symbol(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Expr]]], (hydra.core.Name("indent"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Expr, hydra.ast.ExprIndent(t))), indented_expression(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Expr]]], (hydra.core.Name("op"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Expr, hydra.ast.ExprOp(t))), op_expr(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.ast.Expr]]], (hydra.core.Name("brackets"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Expr, hydra.ast.ExprBrackets(t))), bracket_expr(cx, input))))))))
                return hydra.lib.maybes.maybe(cast(Either[hydra.util.DecodingError, hydra.ast.Expr], Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union type ", tname.value))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.ast.Expr], Left(hydra.util.DecodingError("expected union of type hydra.ast.Expr")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.Expr], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_ast_expr_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def indented_expression(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.IndentedExpression]:
    def _hoist_hydra_decode_ast_indented_expression_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.IndentedExpression]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("style", indent_style, field_map, cx), (lambda field_style: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("expr", expr, field_map, cx), (lambda field_expr: cast(Either[hydra.util.DecodingError, hydra.ast.IndentedExpression], Right(hydra.ast.IndentedExpression(field_style, field_expr)))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.ast.IndentedExpression], Left(hydra.util.DecodingError("expected record of type hydra.ast.IndentedExpression")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.IndentedExpression], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_ast_indented_expression_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def op_expr(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.OpExpr]:
    def _hoist_hydra_decode_ast_op_expr_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.OpExpr]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("op", op, field_map, cx), (lambda field_op: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("lhs", expr, field_map, cx), (lambda field_lhs: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("rhs", expr, field_map, cx), (lambda field_rhs: cast(Either[hydra.util.DecodingError, hydra.ast.OpExpr], Right(hydra.ast.OpExpr(field_op, field_lhs, field_rhs)))))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.ast.OpExpr], Left(hydra.util.DecodingError("expected record of type hydra.ast.OpExpr")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.OpExpr], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_ast_op_expr_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
