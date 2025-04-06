"""Python implementations of hydra.dsl.graph primitives."""

import hydra.dsl.terms as terms
from hydra.core import Field, Name, Term, TypeScheme
from hydra.dsl.python import FrozenDict
from hydra.graph import Element, Graph, Primitive
from hydra.phantoms import TTerm

ELEMENT = Name("Element")
ELEMENT_NAME = Name("name")
ELEMENT_TERM = Name("term")
ELEMENT_TYPE = Name("type")

GRAPH = Name("Graph")
GRAPH_ELEMENTS = Name("elements")
GRAPH_ENVIRONMENT = Name("environment")
GRAPH_TYPES = Name("types")
GRAPH_BODY = Name("body")
GRAPH_PRIMITIVES = Name("primitives")
GRAPH_SCHEMA = Name("schema")

PRIMITIVE = Name("Primitive")
PRIMITIVE_NAME = Name("name")
PRIMITIVE_TYPE = Name("type")
PRIMITIVE_IMPLEMENTATION = Name("implementation")

TERM_CODER = Name("TermCoder")
TERM_CODER_TYPE = Name("type")
TERM_CODER_CODER = Name("coder")

TYPE_CLASS = Name("TypeClass")
TYPE_CLASS_EQUALITY = Name("equality")
TYPE_CLASS_ORDERING = Name("ordering")


def element(
    name: TTerm[Name], term: TTerm[Term], mtyp: TTerm[TypeScheme | None]
) -> TTerm[Element]:
    """Create an element from a name, term, and optional type scheme."""
    return TTerm[Element](
        terms.record(
            ELEMENT,
            [
                Field(ELEMENT_NAME, name.value),
                Field(ELEMENT_TERM, term.value),
                Field(ELEMENT_TYPE, mtyp.value),
            ],
        )
    )


def element_name() -> TTerm[Name]:
    """Get the name of an element."""
    return TTerm[Name](terms.project(ELEMENT, ELEMENT_NAME))


def graph(
    elements: TTerm[dict[Name, Element]],
    environment: TTerm[dict[Name, Term | None]],
    types: TTerm[dict[Name, TypeScheme]],
    body: TTerm[Term],
    primitives: TTerm[dict[Name, Primitive]],
    schema: TTerm[Graph | None],
) -> TTerm[Graph]:
    """Create a graph from a map of name/element bindings, a map of name/term bindings, a map of name/type scheme bindings, a body term, a map of name/primitive bindings, and an optional schema graph.

    Parameters
    ----------
    elements : TTerm[dict[Name, Element]]
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
            GRAPH,
            [
                Field(GRAPH_ELEMENTS, elements.value),
                Field(GRAPH_ENVIRONMENT, environment.value),
                Field(GRAPH_TYPES, types.value),
                Field(GRAPH_BODY, body.value),
                Field(GRAPH_PRIMITIVES, primitives.value),
                Field(GRAPH_SCHEMA, schema.value),
            ],
        )
    )


def graph_elements() -> TTerm[FrozenDict[Name, Element]]:
    """Get the elements of a graph."""
    return TTerm[FrozenDict[Name, Element]](terms.project(GRAPH, GRAPH_ELEMENTS))


def graph_environment() -> TTerm[FrozenDict[Name, Term | None]]:
    """Get the environment of a graph."""
    return TTerm[FrozenDict[Name, Term | None]](terms.project(GRAPH, GRAPH_ENVIRONMENT))


def graph_types() -> TTerm[FrozenDict[Name, TypeScheme]]:
    """Get the types of a graph."""
    return TTerm[FrozenDict[Name, TypeScheme]](terms.project(GRAPH, GRAPH_TYPES))


def graph_body() -> TTerm[Term]:
    """Get the body of a graph."""
    return TTerm[Term](terms.project(GRAPH, GRAPH_BODY))


def graph_primitives() -> TTerm[FrozenDict[Name, Primitive]]:
    """Get the primitives of a graph."""
    return TTerm[FrozenDict[Name, Primitive]](terms.project(GRAPH, GRAPH_PRIMITIVES))


def graph_schema() -> TTerm[Graph | None]:
    """Get the schema of a graph."""
    return TTerm[Graph | None](terms.project(GRAPH, GRAPH_SCHEMA))


def primitive_name() -> TTerm[Name]:
    """Get the name of a primitive."""
    return TTerm[Name](terms.project(PRIMITIVE, PRIMITIVE_NAME))


def primitive_type() -> TTerm[TypeScheme]:
    """Get the type of a primitive."""
    return TTerm[TypeScheme](terms.project(PRIMITIVE, PRIMITIVE_TYPE))


# primitiveImplementation :: TTerm (Primitive -> ([Term] -> Flow Graph Term))
# primitiveImplementation = project _Primitive _Primitive_type
