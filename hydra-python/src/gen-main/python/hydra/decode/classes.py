# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.classes."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Left
from typing import cast
import hydra.classes
import hydra.core
import hydra.errors
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings

def type_class(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_classes_type_class_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.classes.TypeClass]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("equality"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.classes.TypeClass.EQUALITY), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("ordering"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.classes.TypeClass.ORDERING), hydra.extract.core.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_classes_type_class_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))
