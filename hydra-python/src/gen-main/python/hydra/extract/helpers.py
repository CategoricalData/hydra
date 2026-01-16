# Note: this is an automatically generated file. Do not edit.

r"""Helper functions for decoding terms to domain types."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Right, frozenlist
from typing import TypeVar
import hydra.core
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")
T4 = TypeVar("T4")

def decode_either(left_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], right_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], g: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.util.DecodingError, Either[T0, T1]]:
    def _hoist_hydra_extract_helpers_decode_either_1(g: T2, left_decoder: Callable[[T2, hydra.core.Term], Either[hydra.util.DecodingError, T3]], right_decoder: Callable[[T2, hydra.core.Term], Either[hydra.util.DecodingError, T4]], v1: hydra.core.Term) -> Either[hydra.util.DecodingError, Either[T3, T4]]:
        match v1:
            case hydra.core.TermEither(value=e):
                return hydra.lib.eithers.either((lambda lv: hydra.lib.eithers.map((lambda x: Left(x)), left_decoder(g, lv))), (lambda rv: hydra.lib.eithers.map((lambda x: Right(x)), right_decoder(g, rv))), e)
            
            case _:
                return Left(hydra.util.DecodingError("expected either value"))
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda x: hydra.util.DecodingError(x)), (lambda x: x), hydra.lexical.strip_and_dereference_term_either(g, term)), (lambda stripped: _hoist_hydra_extract_helpers_decode_either_1(g, left_decoder, right_decoder, stripped)))

def decode_list(elem_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], g: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.util.DecodingError, frozenlist[T0]]:
    def _hoist_hydra_extract_helpers_decode_list_1(elem_decoder: Callable[[T1, hydra.core.Term], Either[hydra.util.DecodingError, T2]], g: T1, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, frozenlist[T2]]:
        match v1:
            case hydra.core.TermList(value=els):
                return hydra.lib.eithers.map_list((lambda v1: elem_decoder(g, v1)), els)
            
            case _:
                return Left(hydra.util.DecodingError("expected list"))
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda x: hydra.util.DecodingError(x)), (lambda x: x), hydra.lexical.strip_and_dereference_term_either(g, term)), (lambda stripped: _hoist_hydra_extract_helpers_decode_list_1(elem_decoder, g, stripped)))

def decode_map(key_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], val_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], g: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.util.DecodingError, FrozenDict[T0, T1]]:
    def _hoist_hydra_extract_helpers_decode_map_1(g: T2, key_decoder: Callable[[T2, hydra.core.Term], Either[hydra.util.DecodingError, T3]], val_decoder: Callable[[T2, hydra.core.Term], Either[hydra.util.DecodingError, T4]], v1: hydra.core.Term) -> Either[hydra.util.DecodingError, FrozenDict[T3, T4]]:
        match v1:
            case hydra.core.TermMap(value=m):
                return hydra.lib.eithers.map((lambda x1: hydra.lib.maps.from_list(x1)), hydra.lib.eithers.map_list((lambda kv: hydra.lib.eithers.bind(key_decoder(g, hydra.lib.pairs.first(kv)), (lambda k: hydra.lib.eithers.map((lambda v: (k, v)), val_decoder(g, hydra.lib.pairs.second(kv)))))), hydra.lib.maps.to_list(m)))
            
            case _:
                return Left(hydra.util.DecodingError("expected map"))
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda x: hydra.util.DecodingError(x)), (lambda x: x), hydra.lexical.strip_and_dereference_term_either(g, term)), (lambda stripped: _hoist_hydra_extract_helpers_decode_map_1(g, key_decoder, val_decoder, stripped)))

def decode_maybe(elem_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], g: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.util.DecodingError, Maybe[T0]]:
    def _hoist_hydra_extract_helpers_decode_maybe_1(elem_decoder: Callable[[T1, hydra.core.Term], Either[hydra.util.DecodingError, T2]], g: T1, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, Maybe[T2]]:
        match v1:
            case hydra.core.TermMaybe(value=opt):
                return hydra.lib.eithers.map_maybe((lambda v1: elem_decoder(g, v1)), opt)
            
            case _:
                return Left(hydra.util.DecodingError("expected optional value"))
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda x: hydra.util.DecodingError(x)), (lambda x: x), hydra.lexical.strip_and_dereference_term_either(g, term)), (lambda stripped: _hoist_hydra_extract_helpers_decode_maybe_1(elem_decoder, g, stripped)))

def decode_pair(first_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], second_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], g: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.util.DecodingError, tuple[T0, T1]]:
    def _hoist_hydra_extract_helpers_decode_pair_1(first_decoder: Callable[[T2, hydra.core.Term], Either[hydra.util.DecodingError, T3]], g: T2, second_decoder: Callable[[T2, hydra.core.Term], Either[hydra.util.DecodingError, T4]], v1: hydra.core.Term) -> Either[hydra.util.DecodingError, tuple[T3, T4]]:
        match v1:
            case hydra.core.TermPair(value=p):
                return hydra.lib.eithers.bind(first_decoder(g, hydra.lib.pairs.first(p)), (lambda f: hydra.lib.eithers.map((lambda s: (f, s)), second_decoder(g, hydra.lib.pairs.second(p)))))
            
            case _:
                return Left(hydra.util.DecodingError("expected pair"))
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda x: hydra.util.DecodingError(x)), (lambda x: x), hydra.lexical.strip_and_dereference_term_either(g, term)), (lambda stripped: _hoist_hydra_extract_helpers_decode_pair_1(first_decoder, g, second_decoder, stripped)))

def decode_set(elem_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], g: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.util.DecodingError, frozenset[T0]]:
    def _hoist_hydra_extract_helpers_decode_set_1(elem_decoder: Callable[[T1, hydra.core.Term], Either[hydra.util.DecodingError, T2]], g: T1, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, frozenset[T2]]:
        match v1:
            case hydra.core.TermSet(value=s):
                return hydra.lib.eithers.map((lambda x1: hydra.lib.sets.from_list(x1)), hydra.lib.eithers.map_list((lambda v1: elem_decoder(g, v1)), hydra.lib.sets.to_list(s)))
            
            case _:
                return Left(hydra.util.DecodingError("expected set"))
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda x: hydra.util.DecodingError(x)), (lambda x: x), hydra.lexical.strip_and_dereference_term_either(g, term)), (lambda stripped: _hoist_hydra_extract_helpers_decode_set_1(elem_decoder, g, stripped)))

def decode_unit(g: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.util.DecodingError, None]:
    def _hoist_hydra_extract_helpers_decode_unit_1(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, None]:
        match v1:
            case hydra.core.TermUnit():
                return Right(None)
            
            case _:
                return Left(hydra.util.DecodingError("expected a unit value"))
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda x: hydra.util.DecodingError(x)), (lambda x: x), hydra.lexical.strip_and_dereference_term_either(g, term)), (lambda stripped: _hoist_hydra_extract_helpers_decode_unit_1(stripped)))

def decode_wrapped(body_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], g: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.util.DecodingError, T0]:
    def _hoist_hydra_extract_helpers_decode_wrapped_1(body_decoder: Callable[[T1, hydra.core.Term], Either[hydra.util.DecodingError, T2]], g: T1, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, T2]:
        match v1:
            case hydra.core.TermWrap(value=wt):
                return body_decoder(g, wt.body)
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped value"))
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda x: hydra.util.DecodingError(x)), (lambda x: x), hydra.lexical.strip_and_dereference_term_either(g, term)), (lambda stripped: _hoist_hydra_extract_helpers_decode_wrapped_1(body_decoder, g, stripped)))

def require_field(field_name: str, decoder: Callable[[T0, T1], Either[hydra.util.DecodingError, T2]], field_map: FrozenDict[hydra.core.Name, T1], g: T0) -> Either[hydra.util.DecodingError, T2]:
    return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("missing field ", field_name, " in record")))), (lambda field_term: decoder(g, field_term)), hydra.lib.maps.lookup(hydra.core.Name(field_name), field_map))

def to_field_map(record: hydra.core.Record) -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    r"""Convert a Record's fields to a Map from Name to Term."""
    
    return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda f: (f.name, f.term)), record.fields))
