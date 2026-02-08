# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.constraints."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Left, Right
from typing import cast
import hydra.constraints
import hydra.core
import hydra.decode.query
import hydra.extract.helpers
import hydra.lexical
import hydra.lib.eithers
import hydra.util

def path_equation(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.constraints.PathEquation]:
    def _hoist_hydra_decode_constraints_path_equation_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.constraints.PathEquation]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("left", (lambda x1, x2: hydra.decode.query.path(x1, x2)), field_map(), cx), (lambda field_left: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("right", (lambda x1, x2: hydra.decode.query.path(x1, x2)), field_map(), cx), (lambda field_right: Right(hydra.constraints.PathEquation(field_left, field_right))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.constraints.PathEquation"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_constraints_path_equation_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def pattern_implication(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.constraints.PatternImplication]:
    def _hoist_hydra_decode_constraints_pattern_implication_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.constraints.PatternImplication]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("antecedent", (lambda x1, x2: hydra.decode.query.pattern(x1, x2)), field_map(), cx), (lambda field_antecedent: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("consequent", (lambda x1, x2: hydra.decode.query.pattern(x1, x2)), field_map(), cx), (lambda field_consequent: Right(hydra.constraints.PatternImplication(field_antecedent, field_consequent))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.constraints.PatternImplication"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_constraints_pattern_implication_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
