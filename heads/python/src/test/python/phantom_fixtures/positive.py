"""Well-typed phantom DSL expressions that pyright must accept with zero errors (#732).

Positive control for adversarial.py: proves the D2 tightening doesn't false-positive
on legitimate composition, and that inference flows through without extra annotations.
This module is not meant to run -- only to be type-checked.
"""

from typing import Callable

from hydra.core.overlay.python.dsl.phantoms import apply, cases, compose, int_, lam, match, string, var
from hydra.core.typed import TypedTerm

good_fn: TypedTerm[Callable[[int], str]] = var("f")
good_arg: TypedTerm[int] = int_(1)
good_result: TypedTerm[str] = apply(good_fn, good_arg)

good_fn2: TypedTerm[Callable[[str], bool]] = var("g")
composed: TypedTerm[Callable[[int], bool]] = compose(good_fn2, good_fn)
composed_applied: TypedTerm[bool] = apply(composed, good_arg)

# cases() feeding directly into apply() -- the real pattern used in
# packages/hydra-python/src/main/python/hydra/sources/python/utils.py.
cases_fn = cases("Result")
cases_applied = apply(cases_fn, good_arg)

# match() consumes its argument directly and yields a value, not a function.
match_result: TypedTerm[int] = match("Foo", int_(1))

# lam() produces a function consumed by apply().
lam_fn: TypedTerm[Callable[[int], str]] = lam("x", string("s"))
lam_applied: TypedTerm[str] = apply(lam_fn, good_arg)
