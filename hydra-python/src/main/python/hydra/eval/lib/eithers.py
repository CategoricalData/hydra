"""Interpreter-friendly implementations for hydra.lib.eithers primitives.

These functions work with Term values directly, applying function terms
to construct new term structures for the interpreter.
"""

from hydra.compute import Flow
from hydra.core import Application, Term, TermApplication, TermEither
from hydra.graph import Graph
from hydra.lib.flows import pure, fail
from hydra.lib.eithers import either as native_either
from hydra.dsl.python import Left, Right


def bimap(left_fun: Term, right_fun: Term, either_term: Term) -> Flow[Graph, Term]:
    """Apply left_fun to Left values or right_fun to Right values in an Either term.

    This creates new term structures for the interpreter by wrapping the
    function application in a TermApplication.
    """
    match either_term:
        case TermEither(value=e):
            match e:
                case Left(value=val):
                    # Apply left_fun to the left value
                    result = TermApplication(Application(left_fun, val))
                    return pure(TermEither(Left(result)))
                case Right(value=val):
                    # Apply right_fun to the right value
                    result = TermApplication(Application(right_fun, val))
                    return pure(TermEither(Right(result)))
        case _:
            return fail(f"expected either value, got: {either_term}")


def either(left_fun: Term, right_fun: Term, either_term: Term) -> Flow[Graph, Term]:
    """Apply left_fun to Left values or right_fun to Right values, returning the result.

    Unlike bimap, this doesn't rewrap in Either - it just returns the result
    of applying the appropriate function.
    """
    match either_term:
        case TermEither(value=e):
            match e:
                case Left(value=val):
                    # Apply left_fun to the left value
                    return pure(TermApplication(Application(left_fun, val)))
                case Right(value=val):
                    # Apply right_fun to the right value
                    return pure(TermApplication(Application(right_fun, val)))
        case _:
            return fail(f"expected either value, got: {either_term}")
