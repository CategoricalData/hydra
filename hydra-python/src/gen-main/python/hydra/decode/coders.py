# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.coders."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Left, Right
from typing import cast
import hydra.coders
import hydra.core
import hydra.error
import hydra.extract.helpers
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings

def coder_direction(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_coders_coder_direction_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.error.DecodingError, hydra.coders.CoderDirection]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("encode"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.coders.CoderDirection.ENCODE), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("decode"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.coders.CoderDirection.DECODE), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.error.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))
            
            case _:
                return Left(hydra.error.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.error.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_coders_coder_direction_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def language_name(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_coders_language_name_1(v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)
            
            case _:
                return Left(hydra.error.DecodingError("expected string literal"))
    def _hoist_hydra_decode_coders_language_name_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_coders_language_name_1(v)
            
            case _:
                return Left(hydra.error.DecodingError("expected literal"))
    def _hoist_hydra_decode_coders_language_name_3(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.coders.LanguageName(b)), hydra.lib.eithers.either((lambda err: Left(hydra.error.DecodingError(err))), (lambda stripped2: _hoist_hydra_decode_coders_language_name_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx, wrapped_term.body)))
            
            case _:
                return Left(hydra.error.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.error.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_coders_language_name_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def traversal_order(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_coders_traversal_order_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.error.DecodingError, hydra.coders.TraversalOrder]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("pre"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.coders.TraversalOrder.PRE), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("post"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.coders.TraversalOrder.POST), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.error.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))
            
            case _:
                return Left(hydra.error.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.error.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_coders_traversal_order_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
