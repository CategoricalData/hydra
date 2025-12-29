"""Interpreter-friendly implementations for hydra.lib.maps primitives.

These functions work with Term values directly, applying function terms
to construct new term structures for the interpreter.
"""

from hydra.compute import Flow
from hydra.core import Application, Term, TermApplication, TermMap
from hydra.graph import Graph
from hydra.lib.flows import pure, fail


def map_(fun: Term, map_term: Term) -> Flow[Graph, Term]:
    """Map a function over the values of a map.

    (v1 -> v2) -> Map k v1 -> Map k v2
    """
    match map_term:
        case TermMap(value=entries):
            # Create new map with function applied to each value
            new_entries: dict[Term, Term] = {}
            for k, v in entries.items():
                new_entries[k] = TermApplication(Application(fun, v))
            return pure(TermMap(new_entries))
        case _:
            return fail(f"expected map value, got: {map_term}")


def map_keys(fun: Term, map_term: Term) -> Flow[Graph, Term]:
    """Map a function over the keys of a map.

    (k1 -> k2) -> Map k1 v -> Map k2 v
    """
    match map_term:
        case TermMap(value=entries):
            # Create new map with function applied to each key
            new_entries: dict[Term, Term] = {}
            for k, v in entries.items():
                new_k = TermApplication(Application(fun, k))
                new_entries[new_k] = v
            return pure(TermMap(new_entries))
        case _:
            return fail(f"expected map value, got: {map_term}")


def bimap(key_fun: Term, val_fun: Term, map_term: Term) -> Flow[Graph, Term]:
    """Map functions over both keys and values of a map.

    (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2
    """
    match map_term:
        case TermMap(value=entries):
            # Create new map with functions applied to keys and values
            new_entries: dict[Term, Term] = {}
            for k, v in entries.items():
                new_k = TermApplication(Application(key_fun, k))
                new_v = TermApplication(Application(val_fun, v))
                new_entries[new_k] = new_v
            return pure(TermMap(new_entries))
        case _:
            return fail(f"expected map value, got: {map_term}")
