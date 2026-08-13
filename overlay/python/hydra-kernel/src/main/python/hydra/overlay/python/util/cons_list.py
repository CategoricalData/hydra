"""Persistent (immutable) sequence backed by a native ``tuple``.

Mirrors hydra.util.ConsList from Hydra-Java. Implements
``collections.abc.Sequence`` so a ConsList IS a Sequence. Public APIs can
stay typed as ``Sequence[E]`` while internals get C-speed indexing,
iteration, and concatenation.

This implementation uses a native ``tuple`` as the backing store, with a
``_start`` offset marking the logical beginning of the list within that
tuple. ``head``/``tail``/``drop`` are O(1): ``tail`` shares the same backing
tuple with ``_start`` incremented, no copy. ``cons`` (prepend) is still O(n)
by design -- in codegen-heavy workloads, the dominant cost turned out to be
allocation per cell in a linked-list version, so the tuple copy wins despite
the asymptotic loss for prepend.
"""

from __future__ import annotations

from collections.abc import Callable, Iterable, Iterator, Sequence
from functools import total_ordering
from itertools import islice
from typing import Any, Generic, TypeVar, overload

T = TypeVar("T")
U = TypeVar("U")
R = TypeVar("R")


@total_ordering
class ConsList(Sequence[T], Generic[T]):
    """A persistent (immutable) sequence backed by a native ``tuple`` plus offset.

    Construct via ``ConsList.empty()``, ``ConsList.cons(x, xs)``,
    ``ConsList.singleton(x)``, ``ConsList.of(...)``, or
    ``ConsList.from_iterable(...)``. Direct ``__init__`` is gated.
    """

    __slots__ = ("_inner", "_start", "_hash")

    _EMPTY: ConsList[Any]  # set after class body

    def __init__(
        self,
        _inner: tuple[Any, ...] | None = None,
        _start: int = 0,
        *,
        _internal: bool = False,
    ) -> None:
        if not _internal:
            raise TypeError(
                "ConsList cannot be constructed directly; use ConsList.empty(), "
                "ConsList.cons(x, xs), ConsList.singleton(x), or ConsList.of(...)"
            )
        self._inner: tuple[Any, ...] = _inner if _inner is not None else ()
        self._start: int = _start
        self._hash: int | None = None

    # --- Factories ---

    @staticmethod
    def empty() -> ConsList[Any]:
        return ConsList._EMPTY

    @staticmethod
    def cons(value: T, tail: ConsList[T]) -> ConsList[T]:
        return ConsList((value,) + tail._inner[tail._start:], _internal=True)

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
        return self._start >= len(self._inner)

    @property
    def head(self) -> T:
        if self.is_empty():
            raise IndexError("head of empty list")
        return self._inner[self._start]

    @property
    def tail(self) -> ConsList[T]:
        if self.is_empty():
            raise IndexError("tail of empty list")
        new_start = self._start + 1
        if new_start >= len(self._inner):
            return ConsList._EMPTY
        return ConsList(self._inner, new_start, _internal=True)

    def __len__(self) -> int:
        return len(self._inner) - self._start

    @overload
    def __getitem__(self, index: int) -> T: ...
    @overload
    def __getitem__(self, index: slice) -> ConsList[T]: ...
    def __getitem__(self, index: int | slice) -> T | ConsList[T]:
        n = len(self)
        if isinstance(index, slice):
            start, stop, step = index.indices(n)
            sliced = self._inner[self._start + start:self._start + stop:step]
            if not sliced:
                return ConsList._EMPTY
            return ConsList(sliced, _internal=True)
        if index < 0:
            index += n
        if index < 0 or index >= n:
            raise IndexError("ConsList index out of range")
        return self._inner[self._start + index]

    def _get_at(self, i: int) -> T:
        return self._inner[self._start + i]

    def __iter__(self) -> Iterator[T]:
        return islice(self._inner, self._start, None)

    def __contains__(self, item: object) -> bool:
        return any(x == item for x in self)

    def __reversed__(self) -> Iterator[T]:
        return reversed(self._inner[self._start:])

    def __eq__(self, other: object) -> bool:
        if isinstance(other, ConsList):
            return self._inner[self._start:] == other._inner[other._start:]
        if isinstance(other, Sequence) and not isinstance(other, str):
            return tuple(other) == self._inner[self._start:]
        return NotImplemented

    def __hash__(self) -> int:
        if self._hash is None:
            self._hash = hash(self._inner[self._start:])
        return self._hash

    def __lt__(self, other: object) -> bool:
        if isinstance(other, ConsList):
            return self._inner[self._start:] < other._inner[other._start:]
        if isinstance(other, Sequence) and not isinstance(other, str):
            return self._inner[self._start:] < tuple(other)
        return NotImplemented

    def __repr__(self) -> str:
        body = ", ".join(repr(x) for x in self)
        return f"ConsList({body})"

    # --- Functional ops ---

    def reverse(self) -> ConsList[T]:
        if len(self) <= 1:
            return self
        return ConsList(self._inner[self._start:][::-1], _internal=True)

    def map(self, f: Callable[[T], U]) -> ConsList[U]:
        if self.is_empty():
            return ConsList._EMPTY  # type: ignore[return-value]
        return ConsList(tuple(f(x) for x in self), _internal=True)

    def filter(self, predicate: Callable[[T], bool]) -> ConsList[T]:
        kept = tuple(x for x in self if predicate(x))
        if len(kept) == len(self):
            return self
        if not kept:
            return ConsList._EMPTY
        return ConsList(kept, _internal=True)

    def foldl(self, f: Callable[[R, T], R], initial: R) -> R:
        acc = initial
        for x in self:
            acc = f(acc, x)
        return acc

    def foldr(self, f: Callable[[T, R], R], initial: R) -> R:
        acc = initial
        for x in reversed(self):
            acc = f(x, acc)
        return acc

    def take(self, n: int) -> ConsList[T]:
        if n <= 0:
            return ConsList._EMPTY
        if n >= len(self):
            return self
        return ConsList(self._inner[self._start:self._start + n], _internal=True)

    def drop(self, n: int) -> ConsList[T]:
        if n <= 0:
            return self
        new_start = self._start + n
        if new_start >= len(self._inner):
            return ConsList._EMPTY
        return ConsList(self._inner, new_start, _internal=True)

    def init(self) -> ConsList[T]:
        if self.is_empty():
            raise IndexError("init of empty list")
        if len(self) == 1:
            return ConsList._EMPTY
        return ConsList(self._inner[self._start:-1], _internal=True)

    def last(self) -> T:
        if self.is_empty():
            raise IndexError("last of empty list")
        # _inner is never trimmed at the end (only _start moves), so the true
        # last element is always at _inner[-1] regardless of offset.
        return self._inner[-1]

    def concat(self, other: ConsList[T]) -> ConsList[T]:
        if self.is_empty():
            return other
        if other.is_empty():
            return self
        return ConsList(self._inner[self._start:] + other._inner[other._start:], _internal=True)


ConsList._EMPTY = ConsList((), _internal=True)
