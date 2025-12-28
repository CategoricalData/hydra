# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.typing."""

from __future__ import annotations
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import cast
import hydra.core
import hydra.decode.core
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.typing
import hydra.util

def inference_context(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.InferenceContext]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.typing.InferenceContext], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_subst(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TypeSubst]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.typing.TypeSubst], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def inference_result(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.InferenceResult]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.typing.InferenceResult], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def term_subst(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TermSubst]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.typing.TermSubst], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_constraint(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TypeConstraint]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.typing.TypeConstraint], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_context(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TypeContext]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.typing.TypeContext], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))
