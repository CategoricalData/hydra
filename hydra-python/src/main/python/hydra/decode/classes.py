# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.classes."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Left
from typing import cast
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
                tname = inj.type_name
                field = inj.field
                fname = field.name
                fterm = field.term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.classes.TypeClass]]]:
                    return cast(FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.classes.TypeClass]]], hydra.lib.maps.from_list((cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.classes.TypeClass]]], (hydra.core.Name("equality"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.classes.TypeClass.EQUALITY), hydra.extract.helpers.decode_unit(cx, input))))), cast(tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.classes.TypeClass]]], (hydra.core.Name("ordering"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.classes.TypeClass.ORDERING), hydra.extract.helpers.decode_unit(cx, input))))))))
                return hydra.lib.maybes.maybe(cast(Either[hydra.util.DecodingError, hydra.classes.TypeClass], Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union type ", tname.value))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.classes.TypeClass], Left(hydra.util.DecodingError("expected union of type hydra.classes.TypeClass")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.classes.TypeClass], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_classes_type_class_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
