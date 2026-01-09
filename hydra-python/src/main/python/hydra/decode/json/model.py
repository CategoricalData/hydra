# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.json.model."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import cast
import hydra.core
import hydra.extract.helpers
import hydra.graph
import hydra.json.model
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.util

def value(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.json.model.Value]:
    def _hoist_hydra_decode_json_model_value_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.json.model.Value]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                tname = inj.type_name
                field = inj.field
                fname = field.name
                fterm = field.term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.json.model.Value]]]:
                    def _hoist_variant_map_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, bool]:
                        match v1:
                            case hydra.core.LiteralBoolean(value=b):
                                return Right(b)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected boolean literal"))
                    def _hoist_variant_map_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, bool]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_1(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_3(v1: hydra.core.FloatValue) -> Either[hydra.util.DecodingError, Decimal]:
                        match v1:
                            case hydra.core.FloatValueBigfloat(value=f):
                                return Right(f)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected bigfloat value"))
                    def _hoist_variant_map_4(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, Decimal]:
                        match v1:
                            case hydra.core.LiteralFloat(value=v1):
                                return _hoist_variant_map_3(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected bigfloat literal"))
                    def _hoist_variant_map_5(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, Decimal]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_4(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_6(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                        match v1:
                            case hydra.core.LiteralString(value=s):
                                return Right(s)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected string literal"))
                    def _hoist_variant_map_7(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_6(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_8(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                        match v1:
                            case hydra.core.LiteralString(value=s):
                                return Right(s)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected string literal"))
                    def _hoist_variant_map_9(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_8(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    return hydra.lib.maps.from_list(((hydra.core.Name("array"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.json.model.Value, hydra.json.model.ValueArray(t))), hydra.extract.helpers.decode_list(value, cx, input)))), (hydra.core.Name("boolean"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.json.model.Value, hydra.json.model.ValueBoolean(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("null"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.json.model.Value, hydra.json.model.ValueNull())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("number"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.json.model.Value, hydra.json.model.ValueNumber(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_5(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("object"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.json.model.Value, hydra.json.model.ValueObject(t))), hydra.extract.helpers.decode_map((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_7(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), value, cx, input)))), (hydra.core.Name("string"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.json.model.Value, hydra.json.model.ValueString(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_9(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union type ", tname.value)))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.json.model.Value"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_json_model_value_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
