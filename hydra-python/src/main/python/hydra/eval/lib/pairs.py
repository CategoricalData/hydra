"""Interpreter-friendly implementations for hydra.lib.pairs primitives.

These functions work with Term values directly, applying function terms
to construct new term structures for the interpreter.
"""

from hydra.compute import Flow
from hydra.core import Application, Term, TermApplication, TermPair
from hydra.graph import Graph
from hydra.lib.flows import pure, fail


def bimap(first_fun: Term, second_fun: Term, pair_term: Term) -> Flow[Graph, Term]:
    """Apply first_fun to the first element and second_fun to the second element.

    This creates new term structures for the interpreter by wrapping the
    function applications in TermApplication.
    """
    match pair_term:
        case TermPair(value=p):
            fst, snd = p
            # Apply first_fun to first element and second_fun to second element
            new_first = TermApplication(Application(first_fun, fst))
            new_second = TermApplication(Application(second_fun, snd))
            return pure(TermPair((new_first, new_second)))
        case _:
            return fail(f"expected pair value, got: {pair_term}")
