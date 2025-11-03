"""Python DSL for the hydra.graph module."""

import hydra.dsl.terms as terms
import hydra.graph
from hydra.core import Binding, Field, Name, Term, TypeScheme
from hydra.dsl.python import FrozenDict, Maybe
from hydra.graph import Graph, Primitive
from hydra.phantoms import TTerm


def graph(
        elements: TTerm[dict[Name, Binding]],
        environment: TTerm[dict[Name, Maybe[Term]]],
        types: TTerm[dict[Name, TypeScheme]],
        body: TTerm[Term],
        primitives: TTerm[dict[Name, Primitive]],
        schema: TTerm[Maybe[Graph]],
) -> TTerm[Graph]:
    """Create a graph from a map of name/element bindings, a map of name/term bindings, a map of name/type scheme bindings, a body term, a map of name/primitive bindings, and an optional schema graph.

    Parameters
    ----------
    elements : TTerm[dict[Name, Binding]]
        A map of name/element bindings.
    environment : TTerm[dict[Name, Maybe[Term]]]
        A map of name/term bindings.
    types : TTerm[dict[Name, TypeScheme]]
        A map of name/type scheme bindings.
    body : TTerm[Term]
        The body term.
    primitives : TTerm[dict[Name, Primitive]]
        A map of name/primitive bindings.
    schema : TTerm[Maybe[Graph]]
        An optional schema graph.

    Returns
    -------
    TTerm[Graph]
        A graph phantom term
    """
    return TTerm[Graph](
        terms.record(
            hydra.graph.GRAPH__NAME,
            [
                Field(hydra.graph.GRAPH__ELEMENTS__NAME, elements.value),
                Field(hydra.graph.GRAPH__ENVIRONMENT__NAME, environment.value),
                Field(hydra.graph.GRAPH__TYPES__NAME, types.value),
                Field(hydra.graph.GRAPH__BODY__NAME, body.value),
                Field(hydra.graph.GRAPH__PRIMITIVES__NAME, primitives.value),
                Field(hydra.graph.GRAPH__SCHEMA__NAME, schema.value),
            ],
        )
    )


def graph_elements(g: TTerm[Graph]) -> TTerm[FrozenDict[Name, Binding]]:
    """Get the elements of a graph."""
    return TTerm[FrozenDict[Name, Binding]](
        terms.apply(
            terms.project(hydra.graph.GRAPH__NAME, hydra.graph.GRAPH__ELEMENTS__NAME),
            g.value
        )
    )


def graph_environment(g: TTerm[Graph]) -> TTerm[FrozenDict[Name, Maybe[Term]]]:
    """Get the environment of a graph."""
    return TTerm[FrozenDict[Name, Maybe[Term]]](
        terms.apply(
            terms.project(hydra.graph.GRAPH__NAME, hydra.graph.GRAPH__ENVIRONMENT__NAME),
            g.value
        )
    )


def graph_types(g: TTerm[Graph]) -> TTerm[FrozenDict[Name, TypeScheme]]:
    """Get the types of a graph."""
    return TTerm[FrozenDict[Name, TypeScheme]](
        terms.apply(
            terms.project(hydra.graph.GRAPH__NAME, hydra.graph.GRAPH__TYPES__NAME),
            g.value
        )
    )


def graph_body(g: TTerm[Graph]) -> TTerm[Term]:
    """Get the body of a graph."""
    return TTerm[Term](
        terms.apply(
            terms.project(hydra.graph.GRAPH__NAME, hydra.graph.GRAPH__BODY__NAME),
            g.value
        )
    )


def graph_primitives(g: TTerm[Graph]) -> TTerm[FrozenDict[Name, Primitive]]:
    """Get the primitives of a graph."""
    return TTerm[FrozenDict[Name, Primitive]](
        terms.apply(
            terms.project(hydra.graph.GRAPH__NAME, hydra.graph.GRAPH__PRIMITIVES__NAME),
            g.value
        )
    )


def graph_schema(g: TTerm[Graph]) -> TTerm[Maybe[Graph]]:
    """Get the schema of a graph."""
    return TTerm[Maybe[Graph]](
        terms.apply(
            terms.project(hydra.graph.GRAPH__NAME, hydra.graph.GRAPH__SCHEMA__NAME),
            g.value
        )
    )


def primitive_name() -> TTerm[Name]:
    """Get the name of a primitive."""
    return TTerm[Name](
        terms.project(hydra.graph.PRIMITIVE__NAME, hydra.graph.PRIMITIVE__NAME__NAME)
    )


def primitive_type() -> TTerm[TypeScheme]:
    """Get the type of a primitive."""
    return TTerm[TypeScheme](
        terms.project(hydra.graph.PRIMITIVE__NAME, hydra.graph.PRIMITIVE__TYPE__NAME)
    )


# primitiveImplementation :: TTerm (Primitive -> ([Term] -> Flow Graph Term))
# primitiveImplementation = project _Primitive _Primitive_type
