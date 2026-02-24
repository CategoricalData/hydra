"""Meta-DSL for constructing graph-related terms (Graph, Primitive, Comparison, TypeClass).

Mirrors the Haskell module Hydra.Dsl.Meta.Graph, providing phantom-typed constructors,
accessors, and modifiers for graph-related Hydra types.
"""

import hydra.dsl.meta.phantoms as Phantoms
from hydra.classes import TypeClass
from hydra.core import Name
from hydra.graph import Graph, Primitive
from hydra.phantoms import TTerm


# ============================================================
# Comparison
# ============================================================

def comparison_less_than() -> TTerm:
    """The lessThan Comparison variant."""
    _COMPARISON_NAME = Name("hydra.util.Comparison")
    _COMPARISON_LESS_THAN = Name("lessThan")
    return Phantoms.inject_unit(_COMPARISON_NAME, _COMPARISON_LESS_THAN)


def comparison_equal_to() -> TTerm:
    """The equalTo Comparison variant."""
    _COMPARISON_NAME = Name("hydra.util.Comparison")
    _COMPARISON_EQUAL_TO = Name("equalTo")
    return Phantoms.inject_unit(_COMPARISON_NAME, _COMPARISON_EQUAL_TO)


def comparison_greater_than() -> TTerm:
    """The greaterThan Comparison variant."""
    _COMPARISON_NAME = Name("hydra.util.Comparison")
    _COMPARISON_GREATER_THAN = Name("greaterThan")
    return Phantoms.inject_unit(_COMPARISON_NAME, _COMPARISON_GREATER_THAN)


# ============================================================
# Graph
# ============================================================

def graph(
        elements: TTerm,
        environment: TTerm,
        types: TTerm,
        body: TTerm,
        primitives: TTerm,
        schema: TTerm,
) -> TTerm:
    """Construct a Graph."""
    return Phantoms.record(Graph.TYPE_, [
        Phantoms.field(Graph.ELEMENTS, elements),
        Phantoms.field(Graph.ENVIRONMENT, environment),
        Phantoms.field(Graph.TYPES, types),
        Phantoms.field(Graph.BODY, body),
        Phantoms.field(Graph.PRIMITIVES, primitives),
        Phantoms.field(Graph.SCHEMA, schema)])


def graph_elements(g: TTerm) -> TTerm:
    """Get the elements of a Graph."""
    return Phantoms.apply(
        Phantoms.project(Graph.TYPE_, Graph.ELEMENTS), g)


def graph_environment(g: TTerm) -> TTerm:
    """Get the environment of a Graph."""
    return Phantoms.apply(
        Phantoms.project(Graph.TYPE_, Graph.ENVIRONMENT), g)


def graph_types(g: TTerm) -> TTerm:
    """Get the types of a Graph."""
    return Phantoms.apply(
        Phantoms.project(Graph.TYPE_, Graph.TYPES), g)


def graph_body(g: TTerm) -> TTerm:
    """Get the body of a Graph."""
    return Phantoms.apply(
        Phantoms.project(Graph.TYPE_, Graph.BODY), g)


def graph_primitives(g: TTerm) -> TTerm:
    """Get the primitives of a Graph."""
    return Phantoms.apply(
        Phantoms.project(Graph.TYPE_, Graph.PRIMITIVES), g)


def graph_schema(g: TTerm) -> TTerm:
    """Get the schema of a Graph."""
    return Phantoms.apply(
        Phantoms.project(Graph.TYPE_, Graph.SCHEMA), g)


def graph_with_elements(g: TTerm, new_elements: TTerm) -> TTerm:
    """Return a Graph with new elements."""
    return graph(new_elements, graph_environment(g), graph_types(g),
                 graph_body(g), graph_primitives(g), graph_schema(g))


def graph_with_environment(g: TTerm, new_environment: TTerm) -> TTerm:
    """Return a Graph with a new environment."""
    return graph(graph_elements(g), new_environment, graph_types(g),
                 graph_body(g), graph_primitives(g), graph_schema(g))


def graph_with_types(g: TTerm, new_types: TTerm) -> TTerm:
    """Return a Graph with new types."""
    return graph(graph_elements(g), graph_environment(g), new_types,
                 graph_body(g), graph_primitives(g), graph_schema(g))


def graph_with_body(g: TTerm, new_body: TTerm) -> TTerm:
    """Return a Graph with a new body."""
    return graph(graph_elements(g), graph_environment(g), graph_types(g),
                 new_body, graph_primitives(g), graph_schema(g))


def graph_with_primitives(g: TTerm, new_primitives: TTerm) -> TTerm:
    """Return a Graph with new primitives."""
    return graph(graph_elements(g), graph_environment(g), graph_types(g),
                 graph_body(g), new_primitives, graph_schema(g))


def graph_with_schema(g: TTerm, new_schema: TTerm) -> TTerm:
    """Return a Graph with a new schema."""
    return graph(graph_elements(g), graph_environment(g), graph_types(g),
                 graph_body(g), graph_primitives(g), new_schema)


# ============================================================
# Primitive
# ============================================================

def primitive(name: TTerm, typ: TTerm, implementation: TTerm) -> TTerm:
    """Construct a Primitive."""
    return Phantoms.record(Primitive.TYPE_, [
        Phantoms.field(Primitive.NAME, name),
        Phantoms.field(Primitive.TYPE, typ),
        Phantoms.field(Primitive.IMPLEMENTATION, implementation)])


def primitive_name(p: TTerm) -> TTerm:
    """Get the name of a Primitive."""
    return Phantoms.apply(
        Phantoms.project(Primitive.TYPE_, Primitive.NAME), p)


def primitive_type(p: TTerm) -> TTerm:
    """Get the type of a Primitive."""
    return Phantoms.apply(
        Phantoms.project(Primitive.TYPE_, Primitive.TYPE), p)


def primitive_implementation(p: TTerm) -> TTerm:
    """Get the implementation of a Primitive."""
    return Phantoms.apply(
        Phantoms.project(Primitive.TYPE_, Primitive.IMPLEMENTATION), p)


def primitive_with_type(p: TTerm, new_type: TTerm) -> TTerm:
    """Return a Primitive with a new type."""
    return primitive(primitive_name(p), new_type, primitive_implementation(p))


# ============================================================
# TypeClass
# ============================================================

def type_class_equality() -> TTerm:
    """The equality TypeClass variant."""
    return Phantoms.inject_unit(TypeClass.TYPE_, TypeClass.EQUALITY.value)


def type_class_ordering() -> TTerm:
    """The ordering TypeClass variant."""
    return Phantoms.inject_unit(TypeClass.TYPE_, TypeClass.ORDERING.value)
