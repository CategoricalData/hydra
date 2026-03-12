# Note: this is an automatically generated file. Do not edit.

r"""JSON-to-YAML encoding. Converts JSON Values to YAML Nodes (always succeeds), and Hydra Terms to YAML Nodes via JSON."""

from __future__ import annotations
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, frozenlist
from typing import cast
import hydra.core
import hydra.ext.org.yaml.model
import hydra.json.encode
import hydra.json.model
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.pairs

def json_to_yaml(value: hydra.json.model.Value) -> hydra.ext.org.yaml.model.Node_:
    r"""Convert a JSON value to a YAML node. Always succeeds since YAML is a superset of JSON."""
    
    match value:
        case hydra.json.model.ValueArray(value=arr):
            return cast(hydra.ext.org.yaml.model.Node_, hydra.ext.org.yaml.model.NodeSequence(hydra.lib.lists.map((lambda v: json_to_yaml(v)), arr)))
        
        case hydra.json.model.ValueBoolean(value=b):
            return cast(hydra.ext.org.yaml.model.Node_, hydra.ext.org.yaml.model.NodeScalar(cast(hydra.ext.org.yaml.model.Scalar, hydra.ext.org.yaml.model.ScalarBool(b))))
        
        case hydra.json.model.ValueNull():
            return cast(hydra.ext.org.yaml.model.Node_, hydra.ext.org.yaml.model.NodeScalar(cast(hydra.ext.org.yaml.model.Scalar, hydra.ext.org.yaml.model.ScalarNull())))
        
        case hydra.json.model.ValueNumber(value=n):
            return cast(hydra.ext.org.yaml.model.Node_, hydra.ext.org.yaml.model.NodeScalar(cast(hydra.ext.org.yaml.model.Scalar, hydra.ext.org.yaml.model.ScalarFloat(n))))
        
        case hydra.json.model.ValueObject(value=obj):
            return cast(hydra.ext.org.yaml.model.Node_, hydra.ext.org.yaml.model.NodeMapping(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda kv: (cast(hydra.ext.org.yaml.model.Node_, hydra.ext.org.yaml.model.NodeScalar(cast(hydra.ext.org.yaml.model.Scalar, hydra.ext.org.yaml.model.ScalarStr(hydra.lib.pairs.first(kv))))), json_to_yaml(hydra.lib.pairs.second(kv)))), hydra.lib.maps.to_list(obj)))))
        
        case hydra.json.model.ValueString(value=s):
            return cast(hydra.ext.org.yaml.model.Node_, hydra.ext.org.yaml.model.NodeScalar(cast(hydra.ext.org.yaml.model.Scalar, hydra.ext.org.yaml.model.ScalarStr(s))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def to_yaml(term: hydra.core.Term) -> Either[str, hydra.ext.org.yaml.model.Node_]:
    r"""Encode a Hydra term to a YAML node via JSON encoding."""
    
    return hydra.lib.eithers.map((lambda v: json_to_yaml(v)), hydra.json.encode.to_json(term))
