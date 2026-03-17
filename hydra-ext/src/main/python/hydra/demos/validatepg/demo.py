"""Python driver for the PG validation translingual demo.

Reads a schema JSON file and graph JSON files (produced by GenerateData using
hydra.encode.pg.model), validates each graph against the schema using
hydra.pg.validation, and prints the results.

Usage: python validate_demo.py <data-directory>
"""

import json
import os
import sys
import time

# hydra-ext root is six levels up from this file (src/main/python/hydra/demos/validatepg/demo.py)
_hydra_ext_root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))))))
# hydra-python root is sibling to hydra-ext
_hydra_python_root = os.path.join(os.path.dirname(_hydra_ext_root), "hydra-python")

# Add paths for modules (order matters - earlier paths take precedence):
# 1. hydra-ext hand-written modules (this demo)
sys.path.insert(0, os.path.join(_hydra_ext_root, "src/main/python"))
# 2. hydra-ext generated modules (hydra.pg.*, hydra.pg.validation, etc.)
sys.path.insert(0, os.path.join(_hydra_ext_root, "src/gen-main/python"))
# 3. hydra-python generated modules (hydra.core, etc.)
sys.path.insert(0, os.path.join(_hydra_python_root, "src/gen-main/python"))
# 4. hydra-python hand-written modules (hydra.dsl.python, etc.)
sys.path.insert(0, os.path.join(_hydra_python_root, "src/main/python"))

import hydra.core
import hydra.pg.model as pg_model
import hydra.pg.validation as pg_validation
from hydra.dsl.python import FrozenDict, Just, Nothing, frozenlist


# ============================================================================
# Literal type checking
# ============================================================================

_FLOAT_TYPE_NAMES = {
    hydra.core.FloatType.BIGFLOAT: "bigfloat",
    hydra.core.FloatType.FLOAT32: "float32",
    hydra.core.FloatType.FLOAT64: "float64",
}

_INTEGER_TYPE_NAMES = {
    hydra.core.IntegerType.BIGINT: "bigint",
    hydra.core.IntegerType.INT8: "int8",
    hydra.core.IntegerType.INT16: "int16",
    hydra.core.IntegerType.INT32: "int32",
    hydra.core.IntegerType.INT64: "int64",
    hydra.core.IntegerType.UINT8: "uint8",
    hydra.core.IntegerType.UINT16: "uint16",
    hydra.core.IntegerType.UINT32: "uint32",
    hydra.core.IntegerType.UINT64: "uint64",
}

def show_literal_type(lt):
    if isinstance(lt, hydra.core.LiteralTypeBinary): return "binary"
    if isinstance(lt, hydra.core.LiteralTypeBoolean): return "boolean"
    if isinstance(lt, hydra.core.LiteralTypeString): return "string"
    if isinstance(lt, hydra.core.LiteralTypeFloat): return "float:" + _FLOAT_TYPE_NAMES[lt.value]
    if isinstance(lt, hydra.core.LiteralTypeInteger): return "integer:" + _INTEGER_TYPE_NAMES[lt.value]
    raise ValueError(f"Unknown literal type: {lt}")


_FLOAT_VALUE_FAMILY = {
    hydra.core.FloatValueBigfloat: "float:bigfloat",
    hydra.core.FloatValueFloat32: "float:float32",
    hydra.core.FloatValueFloat64: "float:float64",
}

_INTEGER_VALUE_FAMILY = {
    hydra.core.IntegerValueBigint: "integer:bigint",
    hydra.core.IntegerValueInt8: "integer:int8",
    hydra.core.IntegerValueInt16: "integer:int16",
    hydra.core.IntegerValueInt32: "integer:int32",
    hydra.core.IntegerValueInt64: "integer:int64",
    hydra.core.IntegerValueUint8: "integer:uint8",
    hydra.core.IntegerValueUint16: "integer:uint16",
    hydra.core.IntegerValueUint32: "integer:uint32",
    hydra.core.IntegerValueUint64: "integer:uint64",
}

def literal_family(lit):
    if isinstance(lit, hydra.core.LiteralBinary): return "binary"
    if isinstance(lit, hydra.core.LiteralBoolean): return "boolean"
    if isinstance(lit, hydra.core.LiteralString): return "string"
    if isinstance(lit, hydra.core.LiteralFloat):
        return _FLOAT_VALUE_FAMILY.get(type(lit.value), "float:unknown")
    if isinstance(lit, hydra.core.LiteralInteger):
        return _INTEGER_VALUE_FAMILY.get(type(lit.value), "integer:unknown")
    raise ValueError(f"Unknown literal: {lit}")


def show_literal(lit):
    if isinstance(lit, hydra.core.LiteralBinary): return "binary:..."
    if isinstance(lit, hydra.core.LiteralBoolean): return f"boolean:{lit.value}"
    if isinstance(lit, hydra.core.LiteralString): return f'string:"{lit.value}"'
    if isinstance(lit, hydra.core.LiteralFloat):
        fv = lit.value
        family = _FLOAT_VALUE_FAMILY.get(type(fv), "float:unknown")
        return f"{family}:{fv.value}"
    if isinstance(lit, hydra.core.LiteralInteger):
        iv = lit.value
        family = _INTEGER_VALUE_FAMILY.get(type(iv), "integer:unknown")
        return f"{family}:{iv.value}"
    raise ValueError(f"Unknown literal: {lit}")


def check_literal(lt, lv):
    expected = show_literal_type(lt)
    actual = literal_family(lv)
    if expected == actual:
        return Nothing()
    return Just(f"expected {expected}, got {actual}")


# ============================================================================
# JSON -> PG model decoders
# ============================================================================

def _expect_obj(json_val):
    if not isinstance(json_val, dict):
        raise ValueError(f"Expected JSON object, got {type(json_val)}")
    return json_val


def _expect_list(json_val):
    if not isinstance(json_val, list):
        raise ValueError(f"Expected JSON array, got {type(json_val)}")
    return json_val


def _decode_map(json_val, decode_key, decode_value):
    from hydra.dsl.python import FrozenDict
    entries = _expect_list(json_val)
    result = {}
    for entry in entries:
        obj = _expect_obj(entry)
        k = decode_key(obj["@key"])
        v = decode_value(obj["@value"])
        result[k] = v
    return FrozenDict(result)


def _decode_list(json_val, decode_fn):
    from hydra.dsl.python import frozenlist
    return tuple(decode_fn(x) for x in _expect_list(json_val))


def decode_graph_schema(json_val):
    obj = _expect_obj(json_val)
    return pg_model.GraphSchema(
        vertices=_decode_map(obj["vertices"],
                             lambda v: pg_model.VertexLabel(v),
                             decode_vertex_type),
        edges=_decode_map(obj["edges"],
                          lambda v: pg_model.EdgeLabel(v),
                          decode_edge_type),
    )


def decode_graph(json_val):
    obj = _expect_obj(json_val)
    return pg_model.Graph(
        vertices=_decode_map(obj["vertices"], decode_literal, decode_vertex),
        edges=_decode_map(obj["edges"], decode_literal, decode_edge),
    )


def decode_vertex_type(json_val):
    obj = _expect_obj(json_val)
    return pg_model.VertexType(
        label=pg_model.VertexLabel(obj["label"]),
        id=decode_literal_type(obj["id"]),
        properties=_decode_list(obj["properties"], decode_property_type),
    )


def decode_edge_type(json_val):
    obj = _expect_obj(json_val)
    return pg_model.EdgeType(
        label=pg_model.EdgeLabel(obj["label"]),
        id=decode_literal_type(obj["id"]),
        out=pg_model.VertexLabel(obj["out"]),
        in_=pg_model.VertexLabel(obj["in"]),
        properties=_decode_list(obj["properties"], decode_property_type),
    )


def decode_property_type(json_val):
    obj = _expect_obj(json_val)
    return pg_model.PropertyType(
        key=pg_model.PropertyKey(obj["key"]),
        value=decode_literal_type(obj["value"]),
        required=obj["required"],
    )


def decode_vertex(json_val):
    obj = _expect_obj(json_val)
    return pg_model.Vertex(
        label=pg_model.VertexLabel(obj["label"]),
        id=decode_literal(obj["id"]),
        properties=_decode_map(obj["properties"],
                               lambda v: pg_model.PropertyKey(v),
                               decode_literal),
    )


def decode_edge(json_val):
    obj = _expect_obj(json_val)
    return pg_model.Edge(
        label=pg_model.EdgeLabel(obj["label"]),
        id=decode_literal(obj["id"]),
        out=decode_literal(obj["out"]),
        in_=decode_literal(obj["in"]),
        properties=_decode_map(obj["properties"],
                               lambda v: pg_model.PropertyKey(v),
                               decode_literal),
    )


def decode_literal_type(json_val):
    obj = _expect_obj(json_val)
    if "binary" in obj: return hydra.core.LiteralTypeBinary()
    if "boolean" in obj: return hydra.core.LiteralTypeBoolean()
    if "string" in obj: return hydra.core.LiteralTypeString()
    if "float" in obj: return hydra.core.LiteralTypeFloat(_decode_float_type(obj["float"]))
    if "integer" in obj: return hydra.core.LiteralTypeInteger(_decode_integer_type(obj["integer"]))
    raise ValueError(f"Unknown literal type: {obj}")


def _decode_float_type(json_val):
    obj = _expect_obj(json_val)
    if "bigfloat" in obj: return hydra.core.FloatType.BIGFLOAT
    if "float32" in obj: return hydra.core.FloatType.FLOAT32
    if "float64" in obj: return hydra.core.FloatType.FLOAT64
    raise ValueError(f"Unknown float type: {obj}")


def _decode_integer_type(json_val):
    obj = _expect_obj(json_val)
    if "bigint" in obj: return hydra.core.IntegerType.BIGINT
    if "int8" in obj: return hydra.core.IntegerType.INT8
    if "int16" in obj: return hydra.core.IntegerType.INT16
    if "int32" in obj: return hydra.core.IntegerType.INT32
    if "int64" in obj: return hydra.core.IntegerType.INT64
    if "uint8" in obj: return hydra.core.IntegerType.UINT8
    if "uint16" in obj: return hydra.core.IntegerType.UINT16
    if "uint32" in obj: return hydra.core.IntegerType.UINT32
    if "uint64" in obj: return hydra.core.IntegerType.UINT64
    raise ValueError(f"Unknown integer type: {obj}")


def decode_literal(json_val):
    obj = _expect_obj(json_val)
    if "binary" in obj: return hydra.core.LiteralBinary(obj["binary"].encode("utf-8"))
    if "boolean" in obj: return hydra.core.LiteralBoolean(obj["boolean"])
    if "string" in obj: return hydra.core.LiteralString(obj["string"])
    if "float" in obj: return hydra.core.LiteralFloat(_decode_float_value(obj["float"]))
    if "integer" in obj: return hydra.core.LiteralInteger(_decode_integer_value(obj["integer"]))
    raise ValueError(f"Unknown literal: {obj}")


def _decode_float_value(json_val):
    from decimal import Decimal
    obj = _expect_obj(json_val)
    if "bigfloat" in obj: return hydra.core.FloatValueBigfloat(Decimal(str(obj["bigfloat"])))
    if "float32" in obj: return hydra.core.FloatValueFloat32(float(obj["float32"]))
    if "float64" in obj: return hydra.core.FloatValueFloat64(float(obj["float64"]))
    raise ValueError(f"Unknown float value: {obj}")


def _decode_integer_value(json_val):
    obj = _expect_obj(json_val)
    if "bigint" in obj: return hydra.core.IntegerValueBigint(int(obj["bigint"]))
    if "int8" in obj: return hydra.core.IntegerValueInt8(int(obj["int8"]))
    if "int16" in obj: return hydra.core.IntegerValueInt16(int(obj["int16"]))
    if "int32" in obj: return hydra.core.IntegerValueInt32(int(obj["int32"]))
    if "int64" in obj: return hydra.core.IntegerValueInt64(int(obj["int64"]))
    if "uint8" in obj: return hydra.core.IntegerValueUint8(int(obj["uint8"]))
    if "uint16" in obj: return hydra.core.IntegerValueUint16(int(obj["uint16"]))
    if "uint32" in obj: return hydra.core.IntegerValueUint32(int(obj["uint32"]))
    if "uint64" in obj: return hydra.core.IntegerValueUint64(int(obj["uint64"]))
    raise ValueError(f"Unknown integer value: {obj}")


# ============================================================================
# Main
# ============================================================================

GRAPH_NAMES = [
    "valid_social_network",
    "missing_required_property",
    "wrong_id_type",
    "unknown_edge_endpoint",
    "unexpected_vertex_label",
    "unexpected_edge_label",
    "property_value_type_mismatch",
    "unexpected_property_key",
    "wrong_in_vertex_label",
    "wrong_out_vertex_label",
    "missing_required_edge_property",
]


def main():
    if len(sys.argv) < 2:
        print("Usage: python validate_demo.py <data-directory>", file=sys.stderr)
        sys.exit(1)

    data_dir = sys.argv[1]

    # Load data (I/O, not timed)
    with open(os.path.join(data_dir, "schema.json")) as f:
        schema_json = json.load(f)
    schema = decode_graph_schema(schema_json)

    loaded = []
    for name in GRAPH_NAMES:
        graph_path = os.path.join(data_dir, name + ".json")
        if not os.path.exists(graph_path):
            continue
        with open(graph_path) as f:
            graph_json = json.load(f)
        loaded.append((name, decode_graph(graph_json)))

    # Validate (timed: Hydra computation only)
    start_time = time.perf_counter_ns()

    for name, graph in loaded:
        result = pg_validation.validate_graph(check_literal, show_literal, schema, graph)
        match result:
            case Nothing():
                print(f'Graph "{name}": VALID')
            case Just(msg):
                print(f'Graph "{name}": INVALID - {msg}')

    elapsed_ns = time.perf_counter_ns() - start_time
    print(f"HYDRA_TIME_MS={elapsed_ns / 1_000_000.0}", file=sys.stderr)


if __name__ == "__main__":
    main()
