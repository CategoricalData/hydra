# Note: this is an automatically generated file. Do not edit.

r"""Language constraints based on TinkerPop Graph.Features."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Maybe, Nothing
from typing import TypeVar, cast
import hydra.coders
import hydra.core
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.sets
import hydra.strip
import hydra.tinkerpop.features
import hydra.variants

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def tinkerpop_language(name: hydra.coders.LanguageName, features: hydra.tinkerpop.features.Features, extras: hydra.tinkerpop.features.ExtraFeatures[T0]) -> hydra.coders.Language:
    r"""Populate language constraints based on TinkerPop Graph.Features."""

    vp_features = features.vertex.properties.data_type_features
    def cond(v: T1, b: bool) -> Maybe[T1]:
        return hydra.lib.logic.if_else(b, (lambda : hydra.lib.maybes.pure(v)), (lambda : Nothing()))
    @lru_cache(1)
    def supports_lists() -> bool:
        return hydra.lib.logic.or_(vp_features.supports_boolean_array_values, hydra.lib.logic.or_(vp_features.supports_byte_array_values, hydra.lib.logic.or_(vp_features.supports_double_array_values, hydra.lib.logic.or_(vp_features.supports_float_array_values, hydra.lib.logic.or_(vp_features.supports_integer_array_values, hydra.lib.logic.or_(vp_features.supports_long_array_values, vp_features.supports_string_array_values))))))
    supports_literals = True
    supports_maps = vp_features.supports_map_values
    @lru_cache(1)
    def elimination_variants() -> frozenset[T1]:
        return hydra.lib.sets.empty()
    @lru_cache(1)
    def literal_variants() -> frozenset[hydra.variants.LiteralVariant]:
        return hydra.lib.sets.from_list(hydra.lib.maybes.cat((cond(hydra.variants.LiteralVariant.BINARY, vp_features.supports_byte_array_values), cond(hydra.variants.LiteralVariant.BOOLEAN, vp_features.supports_boolean_values), cond(hydra.variants.LiteralVariant.FLOAT, hydra.lib.logic.or_(vp_features.supports_float_values, vp_features.supports_double_values)), cond(hydra.variants.LiteralVariant.INTEGER, hydra.lib.logic.or_(vp_features.supports_integer_values, vp_features.supports_long_values)), cond(hydra.variants.LiteralVariant.STRING, vp_features.supports_string_values))))
    @lru_cache(1)
    def float_types() -> frozenset[hydra.core.FloatType]:
        return hydra.lib.sets.from_list(hydra.lib.maybes.cat((cond(hydra.core.FloatType.FLOAT32, vp_features.supports_float_values), cond(hydra.core.FloatType.FLOAT64, vp_features.supports_double_values))))
    @lru_cache(1)
    def function_variants() -> frozenset[T1]:
        return hydra.lib.sets.empty()
    @lru_cache(1)
    def integer_types() -> frozenset[hydra.core.IntegerType]:
        return hydra.lib.sets.from_list(hydra.lib.maybes.cat((cond(hydra.core.IntegerType.INT32, vp_features.supports_integer_values), cond(hydra.core.IntegerType.INT64, vp_features.supports_long_values))))
    @lru_cache(1)
    def term_variants() -> frozenset[hydra.variants.TermVariant]:
        return hydra.lib.sets.from_list(hydra.lib.maybes.cat((cond(hydra.variants.TermVariant.LIST, supports_lists()), cond(hydra.variants.TermVariant.LITERAL, supports_literals), cond(hydra.variants.TermVariant.MAP, supports_maps), hydra.lib.maybes.pure(hydra.variants.TermVariant.MAYBE))))
    @lru_cache(1)
    def type_variants() -> frozenset[hydra.variants.TypeVariant]:
        return hydra.lib.sets.from_list(hydra.lib.maybes.cat((cond(hydra.variants.TypeVariant.LIST, supports_lists()), cond(hydra.variants.TypeVariant.LITERAL, supports_literals), cond(hydra.variants.TypeVariant.MAP, supports_maps), hydra.lib.maybes.pure(hydra.variants.TypeVariant.MAYBE), hydra.lib.maybes.pure(hydra.variants.TypeVariant.WRAP))))
    def type_predicate(typ: hydra.core.Type):
        @lru_cache(1)
        def dt() -> hydra.core.Type:
            return hydra.strip.deannotate_type(typ)
        def _hoist_dt_body_1(v1):
            match v1:
                case hydra.core.FloatType.FLOAT64:
                    return vp_features.supports_double_array_values

                case hydra.core.FloatType.FLOAT32:
                    return vp_features.supports_float_array_values

                case _:
                    return False
        def _hoist_dt_body_2(v1):
            match v1:
                case hydra.core.IntegerType.UINT8:
                    return vp_features.supports_byte_array_values

                case hydra.core.IntegerType.INT32:
                    return vp_features.supports_integer_array_values

                case hydra.core.IntegerType.INT64:
                    return vp_features.supports_long_array_values

                case _:
                    return False
        def _hoist_dt_body_3(v1):
            match v1:
                case hydra.core.LiteralTypeBoolean():
                    return vp_features.supports_boolean_array_values

                case hydra.core.LiteralTypeFloat(value=ft):
                    return _hoist_dt_body_1(ft)

                case hydra.core.LiteralTypeInteger(value=it):
                    return _hoist_dt_body_2(it)

                case hydra.core.LiteralTypeString():
                    return vp_features.supports_string_array_values

                case _:
                    return False
        def _hoist_dt_body_4(v1):
            match v1:
                case hydra.core.TypeLiteral(value=lt):
                    return _hoist_dt_body_3(lt)

                case _:
                    return False
        def _hoist_dt_body_5(v1):
            match v1:
                case hydra.core.TypeLiteral():
                    return True

                case _:
                    return False
        match dt():
            case hydra.core.TypeList(value=t):
                return _hoist_dt_body_4(hydra.strip.deannotate_type(t))

            case hydra.core.TypeLiteral():
                return True

            case hydra.core.TypeMap(value=mt):
                return extras.supports_map_key(mt.keys)

            case hydra.core.TypeWrap():
                return True

            case hydra.core.TypeMaybe(value=ot):
                return _hoist_dt_body_5(hydra.strip.deannotate_type(ot))

            case _:
                return True
    return hydra.coders.Language(name, hydra.coders.LanguageConstraints(elimination_variants(), literal_variants(), float_types(), function_variants(), integer_types(), term_variants(), type_variants(), (lambda x1: type_predicate(x1))))
