"""Interpreter-friendly implementations for hydra.lib.sets primitives.

These functions work with Term values directly, applying function terms
to construct new term structures for the interpreter.
"""

from hydra.compute import Flow
from hydra.core import Application, Term, TermApplication, TermSet
from hydra.graph import Graph
from hydra.lib.flows import pure, fail


def map_(fun: Term, set_term: Term) -> Flow[Graph, Term]:
    """Map a function over the elements of a set.

    (a -> b) -> Set a -> Set b
    """
    match set_term:
        case TermSet(value=elements):
            # Create new set with function applied to each element
            new_elements = frozenset(
                TermApplication(Application(fun, elem))
                for elem in elements
            )
            return pure(TermSet(new_elements))
        case _:
            return fail(f"expected set value, got: {set_term}")
