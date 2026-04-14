# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.ast."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Right, frozenlist
from typing import cast
import hydra.ast
import hydra.core
import hydra.errors
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings

def associativity(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_ast_associativity_1(cx, v1):
        match v1:
            case hydra.core.TermInject(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.ast.Associativity]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("none"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.ast.Associativity.NONE), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("left"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.ast.Associativity.LEFT), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("right"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.ast.Associativity.RIGHT), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("both"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.ast.Associativity.BOTH), hydra.extract.core.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_ast_associativity_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def block_style(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_ast_block_style_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                def _hoist_field_map_body_1(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_field_map_body_2(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_1(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_field_map_body_3(v12):
                    match v12:
                        case hydra.core.LiteralBoolean(value=b):
                            return Right(b)

                        case _:
                            return Left(hydra.errors.DecodingError("expected boolean literal"))
                def _hoist_field_map_body_4(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_3(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_field_map_body_5(v12):
                    match v12:
                        case hydra.core.LiteralBoolean(value=b):
                            return Right(b)

                        case _:
                            return Left(hydra.errors.DecodingError("expected boolean literal"))
                def _hoist_field_map_body_6(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_5(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("indent", (lambda v12, v2: hydra.extract.core.decode_maybe((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), v12, v2)), field_map(), cx), (lambda field_indent: hydra.lib.eithers.bind(hydra.extract.core.require_field("newlineBeforeContent", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_4(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), field_map(), cx), (lambda field_newline_before_content: hydra.lib.eithers.bind(hydra.extract.core.require_field("newlineAfterContent", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_6(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), field_map(), cx), (lambda field_newline_after_content: Right(hydra.ast.BlockStyle(field_indent, field_newline_before_content, field_newline_after_content))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_ast_block_style_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def symbol(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_ast_symbol_1(v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)

            case _:
                return Left(hydra.errors.DecodingError("expected string literal"))
    def _hoist_hydra_decode_ast_symbol_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_ast_symbol_1(v)

            case _:
                return Left(hydra.errors.DecodingError("expected literal"))
    def _hoist_hydra_decode_ast_symbol_3(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.ast.Symbol(b)), hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_hydra_decode_ast_symbol_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx, wrapped_term.body)))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_ast_symbol_3(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def brackets(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_ast_brackets_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("open", (lambda x1, x2: symbol(x1, x2)), field_map(), cx), (lambda field_open: hydra.lib.eithers.bind(hydra.extract.core.require_field("close", (lambda x1, x2: symbol(x1, x2)), field_map(), cx), (lambda field_close: Right(hydra.ast.Brackets(field_open, field_close))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_ast_brackets_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def indent_style(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_ast_indent_style_1(cx, v1):
        match v1:
            case hydra.core.TermInject(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map():
                    def _hoist_variant_map_1(v12):
                        match v12:
                            case hydra.core.LiteralString(value=s):
                                return Right(s)

                            case _:
                                return Left(hydra.errors.DecodingError("expected string literal"))
                    def _hoist_variant_map_2(v12):
                        match v12:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_1(v)

                            case _:
                                return Left(hydra.errors.DecodingError("expected literal"))
                    def _hoist_variant_map_3(v12):
                        match v12:
                            case hydra.core.LiteralString(value=s):
                                return Right(s)

                            case _:
                                return Left(hydra.errors.DecodingError("expected string literal"))
                    def _hoist_variant_map_4(v12):
                        match v12:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_3(v)

                            case _:
                                return Left(hydra.errors.DecodingError("expected literal"))
                    return hydra.lib.maps.from_list(((hydra.core.Name("allLines"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.IndentStyle, hydra.ast.IndentStyleAllLines(t))), hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_variant_map_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx, input))))), (hydra.core.Name("subsequentLines"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.IndentStyle, hydra.ast.IndentStyleSubsequentLines(t))), hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_variant_map_4(stripped2)), hydra.extract.core.strip_with_decoding_error(cx, input)))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_ast_indent_style_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def ws(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_ast_ws_1(cx, v1):
        match v1:
            case hydra.core.TermInject(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map():
                    def _hoist_variant_map_1(v12):
                        match v12:
                            case hydra.core.LiteralString(value=s):
                                return Right(s)

                            case _:
                                return Left(hydra.errors.DecodingError("expected string literal"))
                    def _hoist_variant_map_2(v12):
                        match v12:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_1(v)

                            case _:
                                return Left(hydra.errors.DecodingError("expected literal"))
                    return hydra.lib.maps.from_list(((hydra.core.Name("none"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Ws, hydra.ast.WsNone())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("space"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Ws, hydra.ast.WsSpace())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("break"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Ws, hydra.ast.WsBreak())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("breakAndIndent"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Ws, hydra.ast.WsBreakAndIndent(t))), hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_variant_map_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx, input))))), (hydra.core.Name("doubleBreak"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Ws, hydra.ast.WsDoubleBreak())), hydra.extract.core.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_ast_ws_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def padding(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_ast_padding_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("left", (lambda x1, x2: ws(x1, x2)), field_map(), cx), (lambda field_left: hydra.lib.eithers.bind(hydra.extract.core.require_field("right", (lambda x1, x2: ws(x1, x2)), field_map(), cx), (lambda field_right: Right(hydra.ast.Padding(field_left, field_right))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_ast_padding_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def precedence(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_ast_precedence_1(v1):
        match v1:
            case hydra.core.IntegerValueInt32(value=i):
                return Right(i)

            case _:
                return Left(hydra.errors.DecodingError("expected int32 value"))
    def _hoist_hydra_decode_ast_precedence_2(v1):
        match v1:
            case hydra.core.LiteralInteger(value=_match_value):
                return _hoist_hydra_decode_ast_precedence_1(_match_value)

            case _:
                return Left(hydra.errors.DecodingError("expected int32 literal"))
    def _hoist_hydra_decode_ast_precedence_3(v1):
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_ast_precedence_2(v)

            case _:
                return Left(hydra.errors.DecodingError("expected literal"))
    def _hoist_hydra_decode_ast_precedence_4(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.ast.Precedence(b)), hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_hydra_decode_ast_precedence_3(stripped2)), hydra.extract.core.strip_with_decoding_error(cx, wrapped_term.body)))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_ast_precedence_4(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def op(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_ast_op_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("symbol", (lambda x1, x2: symbol(x1, x2)), field_map(), cx), (lambda field_symbol: hydra.lib.eithers.bind(hydra.extract.core.require_field("padding", (lambda x1, x2: padding(x1, x2)), field_map(), cx), (lambda field_padding: hydra.lib.eithers.bind(hydra.extract.core.require_field("precedence", (lambda x1, x2: precedence(x1, x2)), field_map(), cx), (lambda field_precedence: hydra.lib.eithers.bind(hydra.extract.core.require_field("associativity", (lambda x1, x2: associativity(x1, x2)), field_map(), cx), (lambda field_associativity: Right(hydra.ast.Op(field_symbol, field_padding, field_precedence, field_associativity))))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_ast_op_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def bracket_expr(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_ast_bracket_expr_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("brackets", (lambda x1, x2: brackets(x1, x2)), field_map(), cx), (lambda field_brackets: hydra.lib.eithers.bind(hydra.extract.core.require_field("enclosed", (lambda x1, x2: expr(x1, x2)), field_map(), cx), (lambda field_enclosed: hydra.lib.eithers.bind(hydra.extract.core.require_field("style", (lambda x1, x2: block_style(x1, x2)), field_map(), cx), (lambda field_style: Right(hydra.ast.BracketExpr(field_brackets, field_enclosed, field_style))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_ast_bracket_expr_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def expr(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_ast_expr_1(cx, v1):
        match v1:
            case hydra.core.TermInject(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.ast.Expr]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("const"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Expr, hydra.ast.ExprConst(t))), symbol(cx, input)))), (hydra.core.Name("indent"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Expr, hydra.ast.ExprIndent(t))), indented_expression(cx, input)))), (hydra.core.Name("op"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Expr, hydra.ast.ExprOp(t))), op_expr(cx, input)))), (hydra.core.Name("brackets"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Expr, hydra.ast.ExprBrackets(t))), bracket_expr(cx, input)))), (hydra.core.Name("seq"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.ast.Expr, hydra.ast.ExprSeq(t))), seq_expr(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_ast_expr_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def indented_expression(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_ast_indented_expression_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("style", (lambda x1, x2: indent_style(x1, x2)), field_map(), cx), (lambda field_style: hydra.lib.eithers.bind(hydra.extract.core.require_field("expr", (lambda x1, x2: expr(x1, x2)), field_map(), cx), (lambda field_expr: Right(hydra.ast.IndentedExpression(field_style, field_expr))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_ast_indented_expression_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def op_expr(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_ast_op_expr_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("op", (lambda x1, x2: op(x1, x2)), field_map(), cx), (lambda field_op: hydra.lib.eithers.bind(hydra.extract.core.require_field("lhs", (lambda x1, x2: expr(x1, x2)), field_map(), cx), (lambda field_lhs: hydra.lib.eithers.bind(hydra.extract.core.require_field("rhs", (lambda x1, x2: expr(x1, x2)), field_map(), cx), (lambda field_rhs: Right(hydra.ast.OpExpr(field_op, field_lhs, field_rhs))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_ast_op_expr_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def seq_expr(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_ast_seq_expr_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("op", (lambda x1, x2: op(x1, x2)), field_map(), cx), (lambda field_op: hydra.lib.eithers.bind(hydra.extract.core.require_field("elements", (lambda v12, v2: hydra.extract.core.decode_list((lambda x1, x2: expr(x1, x2)), v12, v2)), field_map(), cx), (lambda field_elements: Right(hydra.ast.SeqExpr(field_op, field_elements))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_ast_seq_expr_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))
