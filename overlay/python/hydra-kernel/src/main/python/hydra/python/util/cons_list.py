"""Persistent (immutable) sequence backed by a native ``tuple``.

Mirrors hydra.util.ConsList from Hydra-Java. Implements
``collections.abc.Sequence`` so a ConsList IS a Sequence. Public APIs can
stay typed as ``Sequence[E]`` while internals get C-speed indexing,
iteration, and concatenation.

This implementation uses a native ``tuple`` as the backing store. Native
operations are C-speed, but **prepend (``cons``) is O(n)** rather than O(1)
because we copy the tail. In codegen-heavy workloads, the dominant cost
turned out to be allocation per cell in the linked-list version, so the
tuple copy wins despite the asymptotic loss for prepend.
"""

from __future__ import annotations

from collections.abc import Callable, Iterable, Iterator, Sequence
from functools import total_ordering
from typing import Any, Generic, TypeVar, overload

T = TypeVar("T")
U = TypeVar("U")
R = TypeVar("R")


@total_ordering
class ConsList(Sequence[T], Generic[T]):
    """A persistent (immutable) sequence backed by a native ``tuple``.

    Construct via ``ConsList.empty()``, ``ConsList.cons(x, xs)``,
    ``ConsList.singleton(x)``, ``ConsList.of(...)``, or
    ``ConsList.from_iterable(...)``. Direct ``__init__`` is gated.
    """

    __slots__ = ("_inner", "_hash")

    _EMPTY: ConsList[Any]  # set after class body

    def __init__(self, _inner: tuple[Any, ...] | None = None, *, _internal: bool = False) -> None:
        if not _internal:
            raise TypeError(
                "ConsList cannot be constructed directly; use ConsList.empty(), "
                "ConsList.cons(x, xs), ConsList.singleton(x), or ConsList.of(...)"
            )
        self._inner: tuple[Any, ...] = _inner if _inner is not None else ()
        self._hash: int | None = None

    # --- Factories ---

    @staticmethod
    def empty() -> ConsList[Any]:
        return ConsList._EMPTY

    @staticmethod
    def cons(value: T, tail: ConsList[T]) -> ConsList[T]:
        return ConsList((value,) + tail._inner, _internal=True)

    @staticmethod
    def singleton(value: T) -> ConsList[T]:
        return ConsList((value,), _internal=True)

    @staticmethod
    def of(*elements: T) -> ConsList[T]:
        if not elements:
            return ConsList._EMPTY
        return ConsList(elements, _internal=True)

    @staticmethod
    def from_iterable(values: Iterable[T]) -> ConsList[T]:
        if isinstance(values, ConsList):
            return values
        if isinstance(values, tuple):
            if not values:
                return ConsList._EMPTY
            return ConsList(values, _internal=True)
        t = tuple(values)
        if not t:
            return ConsList._EMPTY
        return ConsList(t, _internal=True)

    # --- Core accessors ---

    def is_empty(self) -> bool:
        return not self._inner

    @property
    def head(self) -> T:
        if not self._inner:
            raise IndexError("head of empty list")
        return self._inner[0]

    @property
    def tail(self) -> ConsList[T]:
        if not self._inner:
            raise IndexError("tail of empty list")
        if len(self._inner) == 1:
            return ConsList._EMPTY
        return ConsList(self._inner[1:], _internal=True)

    def __len__(self) -> int:
        return len(self._inner)

    @overload
    def __getitem__(self, index: int) -> T: ...
    @overload
    def __getitem__(self, index: slice) -> ConsList[T]: ...
    def __getitem__(self, index: int | slice) -> T | ConsList[T]:
        if isinstance(index, slice):
            sliced = self._inner[index]
            if not sliced:
                return ConsList._EMPTY
            return ConsList(sliced, _internal=True)
        return self._inner[index]

    def _get_at(self, i: int) -> T:
        return self._inner[i]

    def __iter__(self) -> Iterator[T]:
        return iter(self._inner)

    def __contains__(self, item: object) -> bool:
        return item in self._inner

    def __reversed__(self) -> Iterator[T]:
        return reversed(self._inner)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, ConsList):
            return self._inner == other._inner
        if isinstance(other, Sequence) and not isinstance(other, str):
            return tuple(other) == self._inner
        return NotImplemented

    def __hash__(self) -> int:
        if self._hash is None:
            self._hash = hash(self._inner)
        return self._hash

    def __lt__(self, other: object) -> bool:
        if isinstance(other, ConsList):
            return self._inner < other._inner
        if isinstance(other, Sequence) and not isinstance(other, str):
            return self._inner < tuple(other)
        return NotImplemented

    def __repr__(self) -> str:
        body = ", ".join(repr(x) for x in self._inner)
        return f"ConsList({body})"

    # --- Functional ops ---

    def reverse(self) -> ConsList[T]:
        if len(self._inner) <= 1:
            return self
        return ConsList(self._inner[::-1], _internal=True)

    def map(self, f: Callable[[T], U]) -> ConsList[U]:
        if not self._inner:
            return ConsList._EMPTY  # type: ignore[return-value]
        return ConsList(tuple(f(x) for x in self._inner), _internal=True)

    def filter(self, predicate: Callable[[T], bool]) -> ConsList[T]:
        kept = tuple(x for x in self._inner if predicate(x))
        if len(kept) == len(self._inner):
            return self
        if not kept:
            return ConsList._EMPTY
        return ConsList(kept, _internal=True)

    def foldl(self, f: Callable[[R, T], R], initial: R) -> R:
        acc = initial
        for x in self._inner:
            acc = f(acc, x)
        return acc

    def foldr(self, f: Callable[[T, R], R], initial: R) -> R:
        acc = initial
        for x in reversed(self._inner):
            acc = f(x, acc)
        return acc

    def take(self, n: int) -> ConsList[T]:
        if n <= 0:
            return ConsList._EMPTY
        if n >= len(self._inner):
            return self
        return ConsList(self._inner[:n], _internal=True)

    def drop(self, n: int) -> ConsList[T]:
        if n <= 0:
            return self
        if n >= len(self._inner):
            return ConsList._EMPTY
        return ConsList(self._inner[n:], _internal=True)

    def init(self) -> ConsList[T]:
        if not self._inner:
            raise IndexError("init of empty list")
        if len(self._inner) == 1:
            return ConsList._EMPTY
        return ConsList(self._inner[:-1], _internal=True)

    def last(self) -> T:
        if not self._inner:
            raise IndexError("last of empty list")
        return self._inner[-1]

    def concat(self, other: ConsList[T]) -> ConsList[T]:
        if not self._inner:
            return other
        if not other._inner:
            return self
        return ConsList(self._inner + other._inner, _internal=True)


ConsList._EMPTY = ConsList((), _internal=True)
