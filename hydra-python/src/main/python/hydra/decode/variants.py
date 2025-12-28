# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.variants."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, Left, Right
from typing import cast
import hydra.core
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.util
import hydra.variants

def elimination_variant(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.variants.EliminationVariant]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.variants.EliminationVariant], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def function_variant(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.variants.FunctionVariant]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.variants.FunctionVariant], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def literal_variant(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.variants.LiteralVariant]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.variants.LiteralVariant], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def term_variant(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.variants.TermVariant]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.variants.TermVariant], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_variant(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.variants.TypeVariant]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.variants.TypeVariant], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))
