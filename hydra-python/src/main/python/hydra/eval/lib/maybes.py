"""Interpreter-friendly implementations for hydra.lib.maybes primitives.

These functions work with Term values directly, applying function terms
to construct new term structures for the interpreter.
"""

from hydra.compute import Flow
from hydra.core import Application, Term, TermApplication, TermMaybe, TermList
from hydra.graph import Graph
from hydra.lib.flows import pure, fail
from hydra.dsl.python import Just, Nothing, NOTHING


def apply(fun_maybe: Term, val_maybe: Term) -> Flow[Graph, Term]:
    """Apply a function inside a Maybe to a value inside a Maybe.

    Maybe (a -> b) -> Maybe a -> Maybe b
    """
    match fun_maybe:
        case TermMaybe(value=mf):
            match mf:
                case Nothing():
                    return pure(TermMaybe(NOTHING))
                case Just(value=f):
                    match val_maybe:
                        case TermMaybe(value=mv):
                            match mv:
                                case Nothing():
                                    return pure(TermMaybe(NOTHING))
                                case Just(value=v):
                                    result = TermApplication(Application(f, v))
                                    return pure(TermMaybe(Just(result)))
                        case _:
                            return fail(f"expected maybe value for second arg, got: {val_maybe}")
        case _:
            return fail(f"expected maybe value for first arg, got: {fun_maybe}")


def bind(maybe_term: Term, fun: Term) -> Flow[Graph, Term]:
    """Chain operations on optional values.

    Maybe a -> (a -> Maybe b) -> Maybe b
    """
    match maybe_term:
        case TermMaybe(value=m):
            match m:
                case Nothing():
                    return pure(TermMaybe(NOTHING))
                case Just(value=v):
                    # Return the application - the result should be a Maybe
                    return pure(TermApplication(Application(fun, v)))
        case _:
            return fail(f"expected maybe value, got: {maybe_term}")


def cases(maybe_term: Term, nothing_result: Term, just_fun: Term) -> Flow[Graph, Term]:
    """Handle an optional value with different parameter order than maybe.

    Maybe a -> b -> (a -> b) -> b
    """
    match maybe_term:
        case TermMaybe(value=m):
            match m:
                case Nothing():
                    return pure(nothing_result)
                case Just(value=v):
                    return pure(TermApplication(Application(just_fun, v)))
        case _:
            return fail(f"expected maybe value, got: {maybe_term}")


def compose(f: Term, g: Term) -> Flow[Graph, Term]:
    """Compose two Kleisli arrows.

    (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)

    Note: This requires returning a function term, which is complex.
    For now, fail with a descriptive message.
    """
    return fail(
        "compose requires interpreter support for function composition; "
        "it can currently only be used in compiled code"
    )


def map_(fun: Term, maybe_term: Term) -> Flow[Graph, Term]:
    """Map a function over an optional value.

    (a -> b) -> Maybe a -> Maybe b
    """
    match maybe_term:
        case TermMaybe(value=m):
            match m:
                case Nothing():
                    return pure(TermMaybe(NOTHING))
                case Just(value=v):
                    result = TermApplication(Application(fun, v))
                    return pure(TermMaybe(Just(result)))
        case _:
            return fail(f"expected maybe value, got: {maybe_term}")


def map_maybe(fun: Term, list_term: Term) -> Flow[Graph, Term]:
    """Map a function over a list and collect Just results.

    (a -> Maybe b) -> [a] -> [b]

    Note: This requires evaluating the function on each element and inspecting results.
    For now, fail with a descriptive message.
    """
    return fail(
        "mapMaybe requires interpreter support for evaluating functions during iteration; "
        "it can currently only be used in compiled code"
    )


def maybe(default: Term, fun: Term, maybe_term: Term) -> Flow[Graph, Term]:
    """Handle an optional value, with transformation.

    b -> (a -> b) -> Maybe a -> b
    """
    match maybe_term:
        case TermMaybe(value=m):
            match m:
                case Nothing():
                    return pure(default)
                case Just(value=v):
                    return pure(TermApplication(Application(fun, v)))
        case _:
            return fail(f"expected maybe value, got: {maybe_term}")
