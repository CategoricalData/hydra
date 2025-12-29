"""Interpreter-friendly implementations for hydra.lib.lists primitives.

These functions work with Term values directly, applying function terms
to construct new term structures for the interpreter.
"""

from hydra.compute import Flow
from hydra.core import Application, Term, TermApplication, TermList, TermPair
from hydra.graph import Graph
from hydra.lib.flows import pure, fail


def drop_while(pred_fun: Term, list_term: Term) -> Flow[Graph, Term]:
    """Drop elements from the list while the predicate returns true.

    Note: This creates application terms that must be evaluated by the interpreter.
    For now, this is a placeholder - full interpreter support would be needed.
    """
    # This is complex because we need to evaluate the predicate on each element.
    # For now, fail with a descriptive message.
    return fail(
        "dropWhile requires interpreter support for evaluating predicates during iteration; "
        "it can currently only be used in compiled code"
    )


def sort_on(key_fun: Term, list_term: Term) -> Flow[Graph, Term]:
    """Sort a list using a key function.

    Note: This creates application terms that must be evaluated by the interpreter.
    For now, this is a placeholder - full interpreter support would be needed.
    """
    # This is complex because we need to evaluate the key function on each element
    # and then compare the results.
    return fail(
        "sortOn requires interpreter support for evaluating key functions during sorting; "
        "it can currently only be used in compiled code"
    )


def span(pred_fun: Term, list_term: Term) -> Flow[Graph, Term]:
    """Split a list at the first element where predicate fails.

    Note: This creates application terms that must be evaluated by the interpreter.
    For now, this is a placeholder - full interpreter support would be needed.
    """
    # This is complex because we need to evaluate the predicate on each element.
    return fail(
        "span requires interpreter support for evaluating predicates during iteration; "
        "it can currently only be used in compiled code"
    )


def map_(fun: Term, list_term: Term) -> Flow[Graph, Term]:
    """Map a function over a list.

    (a -> b) -> [a] -> [b]

    This creates application terms for each element. The interpreter will
    reduce these applications when evaluating the result.
    """
    match list_term:
        case TermList(value=elements):
            # Create a new list with function applied to each element
            mapped = tuple(
                TermApplication(Application(fun, elem))
                for elem in elements
            )
            return pure(TermList(mapped))
        case _:
            return fail(f"expected list value, got: {list_term}")


def apply_(funs: Term, list_term: Term) -> Flow[Graph, Term]:
    """Apply a list of functions to a list of values (applicative style).

    [a -> b] -> [a] -> [b]

    Applies each function to each value, producing a list of all results.
    """
    match funs:
        case TermList(value=fun_elements):
            match list_term:
                case TermList(value=val_elements):
                    # Cartesian product: apply each function to each value
                    results: list[Term] = []
                    for f in fun_elements:
                        for v in val_elements:
                            results.append(TermApplication(Application(f, v)))
                    return pure(TermList(tuple(results)))
                case _:
                    return fail(f"expected list value for second arg, got: {list_term}")
        case _:
            return fail(f"expected list of functions for first arg, got: {funs}")


def bind_(list_term: Term, fun: Term) -> Flow[Graph, Term]:
    """Apply a function that returns lists to each element and flatten.

    [a] -> (a -> [b]) -> [b]

    This creates application terms that should reduce to lists, then they
    need to be flattened. Since we can't evaluate at this point, we create
    a nested structure that the reducer should handle.
    """
    match list_term:
        case TermList(value=elements):
            # For each element, apply the function (which returns a list)
            # The result needs to be concatenated
            # We can express this as: concat (map f xs)
            mapped = tuple(
                TermApplication(Application(fun, elem))
                for elem in elements
            )
            # Return a list of applications - the reducer should flatten this
            # Actually, this is tricky - we need to evaluate and concatenate
            # For now, return the mapped list and hope the test structure handles it
            # A proper implementation would need to invoke concat after reduction
            return fail(
                "bind requires full interpreter support for evaluating and flattening; "
                "it can currently only be used in compiled code"
            )
        case _:
            return fail(f"expected list value, got: {list_term}")


def filter_(pred: Term, list_term: Term) -> Flow[Graph, Term]:
    """Filter a list based on a predicate.

    (a -> Bool) -> [a] -> [a]

    Note: This is complex because we need to evaluate the predicate on each
    element to decide whether to include it. We can't do this at term
    construction time without actually evaluating.
    """
    return fail(
        "filter requires full interpreter support for evaluating predicates; "
        "it can currently only be used in compiled code"
    )


def foldl_(fun: Term, init: Term, list_term: Term) -> Flow[Graph, Term]:
    """Left fold over a list.

    (b -> a -> b) -> b -> [a] -> b

    This builds nested applications: f (f (f init x1) x2) x3 ...
    """
    match list_term:
        case TermList(value=elements):
            # Build the nested application structure
            result = init
            for elem in elements:
                # f acc elem = f(acc)(elem) since f is curried
                inner = TermApplication(Application(fun, result))
                result = TermApplication(Application(inner, elem))
            return pure(result)
        case _:
            return fail(f"expected list value, got: {list_term}")


def zipWith_(fun: Term, list1: Term, list2: Term) -> Flow[Graph, Term]:
    """Zip two lists with a function.

    (a -> b -> c) -> [a] -> [b] -> [c]

    Applies the function pairwise to elements from both lists.
    """
    match list1:
        case TermList(value=elements1):
            match list2:
                case TermList(value=elements2):
                    # Zip the elements and apply function
                    min_len = min(len(elements1), len(elements2))
                    results: list[Term] = []
                    for i in range(min_len):
                        # f a b = f(a)(b) since f is curried
                        inner = TermApplication(Application(fun, elements1[i]))
                        result = TermApplication(Application(inner, elements2[i]))
                        results.append(result)
                    return pure(TermList(tuple(results)))
                case _:
                    return fail(f"expected list value for second arg, got: {list2}")
        case _:
            return fail(f"expected list value for first arg, got: {list1}")
