"""Python implementations of hydra.dsl.graph primitives."""

import hydra.dsl.terms as terms
import hydra.graph
from hydra.core import Binding, Field, Name, Term, TypeScheme
from hydra.dsl.python import FrozenDict
from hydra.graph import Graph, Primitive
from hydra.phantoms import TTerm


def element(
    name: TTerm[Name], term: TTerm[Term], mtyp: TTerm[TypeScheme | None]
) -> TTerm[Binding]:
    """Create an element from a name, term, and optional type scheme."""
    return TTerm[Binding](
        terms.record(
            hydra.graph.ELEMENT__NAME,
            [
                Field(hydra.graph.ELEMENT__NAME__NAME, name.value),
                Field(hydra.graph.ELEMENT__TERM__NAME, term.value),
                Field(hydra.graph.ELEMENT__TYPE__NAME, mtyp.value),
            ],
        )
    )


def element_name() -> TTerm[Name]:
    """Get the name of an element."""
    return TTerm[Name](
        terms.project(hydra.graph.ELEMENT__NAME, hydra.graph.ELEMENT__NAME__NAME)
    )


def graph(
    elements: TTerm[dict[Name, Binding]],
    environment: TTerm[dict[Name, Term | None]],
    types: TTerm[dict[Name, TypeScheme]],
    body: TTerm[Term],
    primitives: TTerm[dict[Name, Primitive]],
    schema: TTerm[Graph | None],
) -> TTerm[Graph]:
    """Create a graph from a map of name/element bindings, a map of name/term bindings, a map of name/type scheme bindings, a body term, a map of name/primitive bindings, and an optional schema graph.

    Parameters
    ----------
    elements : TTerm[dict[Name, Binding]]
        A map of name/element bindings.
    environment : TTerm[dict[Name, Term | None]]
        A map of name/term bindings.
    types : TTerm[dict[Name, TypeScheme]]
        A map of name/type scheme bindings.
    body : TTerm[Term]
        The body term.
    primitives : TTerm[dict[Name, Primitive]]
        A map of name/primitive bindings.
    schema : TTerm[Graph | None]
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


def graph_elements() -> TTerm[FrozenDict[Name, Binding]]:
    """Get the elements of a graph."""
    return TTerm[FrozenDict[Name, Binding]](
        terms.project(hydra.graph.GRAPH__NAME, hydra.graph.GRAPH__ELEMENTS__NAME)
    )


def graph_environment() -> TTerm[FrozenDict[Name, Term | None]]:
    """Get the environment of a graph."""
    return TTerm[FrozenDict[Name, Term | None]](
        terms.project(hydra.graph.GRAPH__NAME, hydra.graph.GRAPH__ENVIRONMENT__NAME)
    )


def graph_types() -> TTerm[FrozenDict[Name, TypeScheme]]:
    """Get the types of a graph."""
    return TTerm[FrozenDict[Name, TypeScheme]](
        terms.project(hydra.graph.GRAPH__NAME, hydra.graph.GRAPH__TYPES__NAME)
    )


def graph_body() -> TTerm[Term]:
    """Get the body of a graph."""
    return TTerm[Term](
        terms.project(hydra.graph.GRAPH__NAME, hydra.graph.GRAPH__BODY__NAME)
    )


def graph_primitives() -> TTerm[FrozenDict[Name, Primitive]]:
    """Get the primitives of a graph."""
    return TTerm[FrozenDict[Name, Primitive]](
        terms.project(hydra.graph.GRAPH__NAME, hydra.graph.GRAPH__PRIMITIVES__NAME)
    )


def graph_schema() -> TTerm[Graph | None]:
    """Get the schema of a graph."""
    return TTerm[Graph | None](
        terms.project(hydra.graph.GRAPH__NAME, hydra.graph.GRAPH__SCHEMA__NAME)
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
