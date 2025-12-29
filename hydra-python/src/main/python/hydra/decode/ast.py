# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.ast."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, Left, Maybe, Right
from typing import cast
import hydra.ast
import hydra.core
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.util

def associativity(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Associativity]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.Associativity], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def block_style(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.BlockStyle]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.BlockStyle], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def symbol(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Symbol]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.Symbol], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def brackets(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Brackets]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.Brackets], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def indent_style(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.IndentStyle]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.IndentStyle], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def ws(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Ws]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.Ws], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def padding(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Padding]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.Padding], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def precedence(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Precedence]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.Precedence], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def op(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Op]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.Op], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def bracket_expr(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.BracketExpr]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.BracketExpr], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def expr(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.Expr]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.Expr], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def indented_expression(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.IndentedExpression]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.IndentedExpression], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def op_expr(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.ast.OpExpr]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.ast.OpExpr], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))
