"""Adversarial phantom-type mistakes that pyright must reject (#732, D2 fidelity fix).

Each case mirrors a mistake class from docs/history/dsl-phantom-fidelity.md's M1-M5
battery (M4/M5 are out of scope for this module; see positive.py for well-typed
counterparts). This module is not meant to run — only to be type-checked.
"""

from typing import Callable

from hydra.core.overlay.python.dsl.phantoms import apply, cases, compose, int_, string, var
from hydra.core.typed import TypedTerm

# M1: ill-typed application -- apply a non-function TypedTerm as if it were a function.
m1_not_a_function: TypedTerm[int] = int_(5)
m1_bad = apply(m1_not_a_function, int_(1))

# M2: domain mismatch -- function's declared domain is int, argument is str.
m2_int_fn: TypedTerm[Callable[[int], int]] = var("intFn")
m2_bad = apply(m2_int_fn, string("hello"))

# M3: value/function confusion (#615) -- cases() yields a function phantom;
# treating its result as a bare value must be rejected.
m3_bad: TypedTerm[int] = cases("Foo")

# M2b: domain mismatch through compose -- g's codomain (str) must match f's domain (int).
m2b_str_to_bool: TypedTerm[Callable[[str], bool]] = var("f")
m2b_int_to_int: TypedTerm[Callable[[int], int]] = var("g")
m2b_bad = compose(m2b_str_to_bool, m2b_int_to_int)
