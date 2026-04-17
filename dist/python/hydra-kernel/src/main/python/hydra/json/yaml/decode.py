# Note: this is an automatically generated file. Do not edit.

r"""YAML-to-JSON decoding. Converts YAML Nodes to JSON Values (may fail for non-JSON YAML), and YAML Nodes to Hydra Terms via JSON."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import cast
import hydra.core
import hydra.json.decode
import hydra.json.model
import hydra.lib.eithers
import hydra.lib.literals
import hydra.lib.maps
import hydra.lib.pairs
import hydra.yaml.model

def yaml_to_json(node: hydra.yaml.model.Node_):
    def _hoist_hydra_json_yaml_decode_yaml_to_json_1(v1):
        match v1:
            case hydra.yaml.model.ScalarBool(value=b):
                return Right(cast(hydra.json.model.Value, hydra.json.model.ValueBoolean(b)))

            case hydra.yaml.model.ScalarDecimal(value=d):
                return Right(cast(hydra.json.model.Value, hydra.json.model.ValueNumber(d)))

            case hydra.yaml.model.ScalarFloat(value=f):
                return Right(cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.float64_to_decimal(hydra.lib.literals.bigfloat_to_float64(f)))))

            case hydra.yaml.model.ScalarInt(value=i):
                return Right(cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_decimal(i))))

            case hydra.yaml.model.ScalarNull():
                return Right(cast(hydra.json.model.Value, hydra.json.model.ValueNull()))

            case hydra.yaml.model.ScalarStr(value=str):
                return Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(str)))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match node:
        case hydra.yaml.model.NodeMapping(value=m):
            def convert_entry(kv: tuple[hydra.yaml.model.Node_, hydra.yaml.model.Node_]) -> Either[str, tuple[str, hydra.json.model.Value]]:
                @lru_cache(1)
                def key_node() -> hydra.yaml.model.Node_:
                    return hydra.lib.pairs.first(kv)
                @lru_cache(1)
                def val_node() -> hydra.yaml.model.Node_:
                    return hydra.lib.pairs.second(kv)
                @lru_cache(1)
                def key_result():
                    def _hoist_key_result_1(v1):
                        match v1:
                            case hydra.yaml.model.ScalarStr(value=str):
                                return Right(str)

                            case _:
                                return Left("non-string YAML mapping key")
                    def _hoist_key_result_2(v1):
                        match v1:
                            case hydra.yaml.model.NodeScalar(value=s):
                                return _hoist_key_result_1(s)

                            case _:
                                return Left("non-scalar YAML mapping key")
                    return _hoist_key_result_2(key_node())
                return hydra.lib.eithers.either((lambda err: Left(err)), (lambda key: (val_result := yaml_to_json(val_node()), hydra.lib.eithers.map((lambda v: (key, v)), val_result))[1]), key_result())
            @lru_cache(1)
            def entries() -> Either[str, frozenlist[tuple[str, hydra.json.model.Value]]]:
                return hydra.lib.eithers.map_list((lambda x1: convert_entry(x1)), hydra.lib.maps.to_list(m))
            return hydra.lib.eithers.map((lambda es: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(es)))), entries())

        case hydra.yaml.model.NodeScalar(value=s):
            return _hoist_hydra_json_yaml_decode_yaml_to_json_1(s)

        case hydra.yaml.model.NodeSequence(value=nodes):
            @lru_cache(1)
            def results() -> Either[str, frozenlist[hydra.json.model.Value]]:
                return hydra.lib.eithers.map_list((lambda n: yaml_to_json(n)), nodes)
            return hydra.lib.eithers.map((lambda vs: cast(hydra.json.model.Value, hydra.json.model.ValueArray(vs))), results())

        case _:
            raise AssertionError("Unreachable: all variants handled")

def from_yaml(types: FrozenDict[hydra.core.Name, hydra.core.Type], tname: hydra.core.Name, typ: hydra.core.Type, node: hydra.yaml.model.Node_) -> Either[str, hydra.core.Term]:
    r"""Decode a YAML node to a Hydra term via JSON decoding."""

    @lru_cache(1)
    def json_result() -> Either[str, hydra.json.model.Value]:
        return yaml_to_json(node)
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda json: hydra.json.decode.from_json(types, tname, typ, json)), json_result())
