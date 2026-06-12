"""One-shot memoized thunk.

Mirrors hydra.util.Lazy from Hydra-Java. Holds a zero-argument callable; the
first ``get()`` evaluates it and caches the result, subsequent ``get()`` calls
return the cached value without re-evaluating.

Used by generated Python code wherever the kernel's Haskell sources rely on
lazy ``let`` semantics: a binding's value is constructed once even when it is
referenced multiple times, and is not evaluated at all when no use site is
forced. The Java head emits ``new Lazy<>(...)``; Python emits ``Lazy(...)``.
"""

from __future__ import annotations

from collections.abc import Callable
from typing import Generic, TypeVar

T = TypeVar("T")


class Lazy(Generic[T]):
    """A one-shot memoizer for a zero-argument computation."""

    __slots__ = ("_fn", "_value")

    _UNSET: object = object()

    def __init__(self, fn: Callable[[], T]) -> None:
        self._fn: Callable[[], T] | None = fn
        self._value: object = Lazy._UNSET

    def get(self) -> T:
        if self._value is Lazy._UNSET:
            assert self._fn is not None
            self._value = self._fn()
            self._fn = None
        return self._value  # type: ignore[return-value]

    def __call__(self) -> T:
        # Allow `name()` syntax in addition to `name.get()`. This lets generated
        # code that previously emitted `@lru_cache(1) def name(): ...; name()`
        # be migrated to `name = Lazy(lambda: ...)` without rewriting every use
        # site — `name()` continues to work, dispatching to `get()`.
        return self.get()
