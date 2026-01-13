# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.classes."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left
import hydra.classes
import hydra.core
import hydra.extract.helpers
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.util

def type_class(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.classes.TypeClass]:
    def _hoist_hydra_decode_classes_type_class_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.classes.TypeClass]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                @lru_cache(1)
                def tname() -> hydra.core.Type:
                    return inj.type_name
                @lru_cache(1)
                def field() -> hydra.core.Type:
                    return inj.field
                @lru_cache(1)
                def fname() -> hydra.core.Type:
                    return field().name
                @lru_cache(1)
                def fterm() -> hydra.core.Type:
                    return field().term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.classes.TypeClass]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("equality"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.classes.TypeClass.EQUALITY), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("ordering"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.classes.TypeClass.ORDERING), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.classes.TypeClass"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_classes_type_class_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
