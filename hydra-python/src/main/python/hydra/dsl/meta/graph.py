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
        bound_terms: TTerm,
        bound_types: TTerm,
        class_constraints: TTerm,
        lambda_variables: TTerm,
        metadata: TTerm,
        primitives: TTerm,
        schema_types: TTerm,
        type_variables: TTerm,
) -> TTerm:
    """Construct a Graph."""
    return Phantoms.record(Graph.TYPE_, [
        Phantoms.field(Graph.BOUND_TERMS, bound_terms),
        Phantoms.field(Graph.BOUND_TYPES, bound_types),
        Phantoms.field(Graph.CLASS_CONSTRAINTS, class_constraints),
        Phantoms.field(Graph.LAMBDA_VARIABLES, lambda_variables),
        Phantoms.field(Graph.METADATA, metadata),
        Phantoms.field(Graph.PRIMITIVES, primitives),
        Phantoms.field(Graph.SCHEMA_TYPES, schema_types),
        Phantoms.field(Graph.TYPE_VARIABLES, type_variables)])


def graph_bound_terms(g: TTerm) -> TTerm:
    """Get the bound terms of a Graph."""
    return Phantoms.apply(
        Phantoms.project(Graph.TYPE_, Graph.BOUND_TERMS), g)


def graph_bound_types(g: TTerm) -> TTerm:
    """Get the bound types of a Graph."""
    return Phantoms.apply(
        Phantoms.project(Graph.TYPE_, Graph.BOUND_TYPES), g)


def graph_class_constraints(g: TTerm) -> TTerm:
    """Get the class constraints of a Graph."""
    return Phantoms.apply(
        Phantoms.project(Graph.TYPE_, Graph.CLASS_CONSTRAINTS), g)


def graph_lambda_variables(g: TTerm) -> TTerm:
    """Get the lambda variables of a Graph."""
    return Phantoms.apply(
        Phantoms.project(Graph.TYPE_, Graph.LAMBDA_VARIABLES), g)


def graph_metadata(g: TTerm) -> TTerm:
    """Get the metadata of a Graph."""
    return Phantoms.apply(
        Phantoms.project(Graph.TYPE_, Graph.METADATA), g)


def graph_primitives(g: TTerm) -> TTerm:
    """Get the primitives of a Graph."""
    return Phantoms.apply(
        Phantoms.project(Graph.TYPE_, Graph.PRIMITIVES), g)


def graph_schema_types(g: TTerm) -> TTerm:
    """Get the schema types of a Graph."""
    return Phantoms.apply(
        Phantoms.project(Graph.TYPE_, Graph.SCHEMA_TYPES), g)


def graph_type_variables(g: TTerm) -> TTerm:
    """Get the type variables of a Graph."""
    return Phantoms.apply(
        Phantoms.project(Graph.TYPE_, Graph.TYPE_VARIABLES), g)


def graph_with_bound_terms(g: TTerm, new_bound_terms: TTerm) -> TTerm:
    """Return a Graph with new bound terms."""
    return graph(new_bound_terms, graph_bound_types(g), graph_class_constraints(g),
                 graph_lambda_variables(g), graph_metadata(g), graph_primitives(g),
                 graph_schema_types(g), graph_type_variables(g))


def graph_with_bound_types(g: TTerm, new_bound_types: TTerm) -> TTerm:
    """Return a Graph with new bound types."""
    return graph(graph_bound_terms(g), new_bound_types, graph_class_constraints(g),
                 graph_lambda_variables(g), graph_metadata(g), graph_primitives(g),
                 graph_schema_types(g), graph_type_variables(g))


def graph_with_class_constraints(g: TTerm, new_class_constraints: TTerm) -> TTerm:
    """Return a Graph with new class constraints."""
    return graph(graph_bound_terms(g), graph_bound_types(g), new_class_constraints,
                 graph_lambda_variables(g), graph_metadata(g), graph_primitives(g),
                 graph_schema_types(g), graph_type_variables(g))


def graph_with_lambda_variables(g: TTerm, new_lambda_variables: TTerm) -> TTerm:
    """Return a Graph with new lambda variables."""
    return graph(graph_bound_terms(g), graph_bound_types(g), graph_class_constraints(g),
                 new_lambda_variables, graph_metadata(g), graph_primitives(g),
                 graph_schema_types(g), graph_type_variables(g))


def graph_with_metadata(g: TTerm, new_metadata: TTerm) -> TTerm:
    """Return a Graph with new metadata."""
    return graph(graph_bound_terms(g), graph_bound_types(g), graph_class_constraints(g),
                 graph_lambda_variables(g), new_metadata, graph_primitives(g),
                 graph_schema_types(g), graph_type_variables(g))


def graph_with_primitives(g: TTerm, new_primitives: TTerm) -> TTerm:
    """Return a Graph with new primitives."""
    return graph(graph_bound_terms(g), graph_bound_types(g), graph_class_constraints(g),
                 graph_lambda_variables(g), graph_metadata(g), new_primitives,
                 graph_schema_types(g), graph_type_variables(g))


def graph_with_schema_types(g: TTerm, new_schema_types: TTerm) -> TTerm:
    """Return a Graph with new schema types."""
    return graph(graph_bound_terms(g), graph_bound_types(g), graph_class_constraints(g),
                 graph_lambda_variables(g), graph_metadata(g), graph_primitives(g),
                 new_schema_types, graph_type_variables(g))


def graph_with_type_variables(g: TTerm, new_type_variables: TTerm) -> TTerm:
    """Return a Graph with new type variables."""
    return graph(graph_bound_terms(g), graph_bound_types(g), graph_class_constraints(g),
                 graph_lambda_variables(g), graph_metadata(g), graph_primitives(g),
                 graph_schema_types(g), new_type_variables)


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
