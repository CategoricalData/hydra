"""Bidirectional bridge between Hydra property graphs and TinkerPop (Gremlin) graphs, in Python.

This is the Python counterpart of ``hydra.overlay.java.tinkerpop.coder`` (the
``HydraGremlinBridge`` + ``Validate`` classes). It converts between a live TinkerPop
graph (read/written via a gremlinpython ``GraphTraversalSource``) and Hydra's
``hydra.pg.model.Graph``, and validates a TinkerPop graph against a Hydra ``GraphSchema``
using the ``hydra.validate.pg`` engine.

Host-specific code: it depends on gremlinpython, so it lives under the ``hydra.overlay.python.*``
namespace (only translingual/generated code lives outside ``hydra.overlay``). This module is
currently shipped as a binding; it is authored at its eventual overlay namespace so the future
migration to ``overlay/`` is a file move, not a rewrite.
"""

import hydra.core
import hydra.error.pg
import hydra.pg.model as pg_model

from hydra.overlay.python.dsl.python import FrozenDict, Given, None_

# gremlinpython is imported lazily (inside the graph functions) so that the pure value-mapping
# and validation-logic functions — the unit-testable surface — can be imported and exercised
# without gremlinpython installed. Touching a live graph requires gremlinpython and a Gremlin
# Server, which is integration territory.


def _gremlin_imports():
    from gremlin_python.process.graph_traversal import __ as gremlin_anon
    from gremlin_python.process.traversal import Direction, T
    return gremlin_anon, Direction, T


# --- Value mapping ---------------------------------------------------------------------------

def object_to_literal(obj):
    """Convert a Python value from gremlinpython to a Hydra Literal.

    Handles the types that TinkerPop's Modern graph uses: str, int, and float
    (Python float is 64-bit).
    """
    if isinstance(obj, str):
        return hydra.core.LiteralString(obj)
    if isinstance(obj, int):
        return hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(obj))
    if isinstance(obj, float):
        return hydra.core.LiteralFloat(hydra.core.FloatValueFloat64(obj))
    raise ValueError(f"Unsupported value type: {type(obj)}")


def literal_to_object(literal):
    """Convert a Hydra Literal to a plain Python value. Inverse of ``object_to_literal``."""
    if isinstance(literal, hydra.core.LiteralString):
        return literal.value
    if isinstance(literal, hydra.core.LiteralBoolean):
        return literal.value
    if isinstance(literal, hydra.core.LiteralInteger):
        return literal.value.value
    if isinstance(literal, hydra.core.LiteralFloat):
        return literal.value.value
    if isinstance(literal, hydra.core.LiteralBinary):
        return literal.value
    raise ValueError(f"Unsupported literal type: {type(literal).__name__}")


# --- Graph <-> pg.model ----------------------------------------------------------------------

def gremlin_to_hydra(g, value_mapper=object_to_literal):
    """Read a TinkerPop graph via traversals and convert to a Hydra Graph.

    ``g`` is a gremlinpython ``GraphTraversalSource`` (remote or embedded). Returns a
    ``hydra.pg.model.Graph`` with Literal values.
    """
    _, Direction, T = _gremlin_imports()
    vertices = {}
    for v in g.V().element_map().to_list():
        vid = value_mapper(v[T.id])
        label = pg_model.VertexLabel(v[T.label])
        properties = FrozenDict({
            pg_model.PropertyKey(k): value_mapper(val)
            for k, val in v.items()
            if k not in (T.id, T.label)
        })
        vertices[vid] = pg_model.Vertex(label=label, id=vid, properties=properties)

    edges = {}
    for e in g.E().element_map().to_list():
        eid = value_mapper(e[T.id])
        label = pg_model.EdgeLabel(e[T.label])
        out_id = value_mapper(e[Direction.OUT][T.id])
        in_id = value_mapper(e[Direction.IN][T.id])
        properties = FrozenDict({
            pg_model.PropertyKey(k): value_mapper(val)
            for k, val in e.items()
            if k not in (T.id, T.label, Direction.OUT, Direction.IN)
        })
        edges[eid] = pg_model.Edge(
            label=label, id=eid, out=out_id, in_=in_id, properties=properties,
        )

    return pg_model.Graph(vertices=FrozenDict(vertices), edges=FrozenDict(edges))


def hydra_to_gremlin(graph, g, value_unmapper=literal_to_object):
    """Write a Hydra Graph into a live TinkerPop graph via traversals.

    Upserts by id: existing vertices/edges keep their identity and have properties
    overwritten; new ones are added. Vertices are written before edges to avoid
    dangling references within a single call. ``g`` may be remote or embedded but
    must allow writes.
    """
    for vertex in graph.vertices.values():
        _upsert_vertex(g, vertex, value_unmapper)

    for edge in graph.edges.values():
        _upsert_edge(g, edge, value_unmapper)


def _upsert_vertex(g, vertex, value_unmapper):
    gremlin_anon, _, T = _gremlin_imports()
    vid = value_unmapper(vertex.id)
    label = vertex.label.value
    traversal = g.V(vid).fold().coalesce(
        gremlin_anon.unfold(),
        gremlin_anon.addV(label).property(T.id, vid),
    )
    for key, literal in vertex.properties.items():
        traversal = traversal.property(key.value, value_unmapper(literal))
    traversal.iterate()


def _upsert_edge(g, edge, value_unmapper):
    gremlin_anon, _, T = _gremlin_imports()
    eid = value_unmapper(edge.id)
    label = edge.label.value
    out_id = value_unmapper(edge.out)
    in_id = value_unmapper(edge.in_)

    _ensure_vertex_exists(g, out_id)
    _ensure_vertex_exists(g, in_id)

    traversal = g.E(eid).fold().coalesce(
        gremlin_anon.unfold(),
        gremlin_anon.V(out_id).addE(label).to(gremlin_anon.V(in_id)).property(T.id, eid),
    )
    for key, literal in edge.properties.items():
        traversal = traversal.property(key.value, value_unmapper(literal))
    traversal.iterate()


def _ensure_vertex_exists(g, vid):
    """Idempotent: create a placeholder ("_stub") vertex with id ``vid`` if absent."""
    gremlin_anon, _, T = _gremlin_imports()
    g.V(vid).fold().coalesce(
        gremlin_anon.unfold(),
        gremlin_anon.addV("_stub").property(T.id, vid),
    ).iterate()


# --- Validation ------------------------------------------------------------------------------

_FLOAT_TYPE_NAMES = {
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

_FLOAT_VALUE_FAMILY = {
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


def show_literal_type(lt):
    if isinstance(lt, hydra.core.LiteralTypeBinary):
        return "binary"
    if isinstance(lt, hydra.core.LiteralTypeBoolean):
        return "boolean"
    if isinstance(lt, hydra.core.LiteralTypeString):
        return "string"
    if isinstance(lt, hydra.core.LiteralTypeFloat):
        return "float:" + _FLOAT_TYPE_NAMES[lt.value]
    if isinstance(lt, hydra.core.LiteralTypeInteger):
        return "integer:" + _INTEGER_TYPE_NAMES[lt.value]
    raise ValueError(f"Unknown literal type: {lt}")


def literal_family(lit):
    if isinstance(lit, hydra.core.LiteralBinary):
        return "binary"
    if isinstance(lit, hydra.core.LiteralBoolean):
        return "boolean"
    if isinstance(lit, hydra.core.LiteralString):
        return "string"
    if isinstance(lit, hydra.core.LiteralFloat):
        return _FLOAT_VALUE_FAMILY.get(type(lit.value), "float:unknown")
    if isinstance(lit, hydra.core.LiteralInteger):
        return _INTEGER_VALUE_FAMILY.get(type(lit.value), "integer:unknown")
    raise ValueError(f"Unknown literal: {lit}")


def show_literal(lit):
    if isinstance(lit, hydra.core.LiteralBinary):
        return "binary:..."
    if isinstance(lit, hydra.core.LiteralBoolean):
        return f"boolean:{lit.value}"
    if isinstance(lit, hydra.core.LiteralString):
        return f'string:"{lit.value}"'
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
    """The ``check_value`` callback for ``hydra.validate.pg.validate_graph``.

    Returns ``None_()`` if the literal value matches the expected type,
    ``Given(InvalidValueError(...))`` otherwise.
    """
    expected = show_literal_type(lt)
    actual = literal_family(lv)
    if expected == actual:
        return None_()
    return Given(hydra.error.pg.InvalidValueError(expected, show_literal(lv)))


class Result:
    """The result of validating a graph against a schema. ``error`` carries the typed
    ``InvalidGraphError`` (or None when valid)."""

    def __init__(self, error):
        self._error = error

    @property
    def is_valid(self):
        return self._error is None

    @property
    def error(self):
        return self._error

    def __repr__(self):
        return "VALID" if self._error is None else f"INVALID - {self._error}"


def validate(schema, g):
    """Validate a TinkerPop graph (via a gremlinpython traversal source) against a schema.

    Returns a ``Result`` whose ``repr()`` is "VALID" or "INVALID - ..." and whose ``error``,
    when present, is a typed ``InvalidGraphError``.
    """
    import hydra.validate.pg as pg_validation
    import hydra.validation

    hydra_graph = gremlin_to_hydra(g)
    result = pg_validation.validate_graph(
        pg_validation.default_pg_profile(),
        hydra.validation.ValidationResult(errors=[], warnings=[]),
        check_literal,
        schema,
        hydra_graph,
    )
    return Result(result.errors[0] if result.errors else None)
