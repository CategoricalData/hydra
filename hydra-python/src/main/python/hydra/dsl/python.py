"""A collection of foundational typing constructs specific to Hydra-Python."""

from __future__ import annotations
from collections.abc import Iterator, Mapping
from dataclasses import dataclass, field
from typing import Any, Generic, TypeVar, cast, final, TypeAlias


K = TypeVar("K")
L = TypeVar("L")
R = TypeVar("R")
T = TypeVar("T")
V = TypeVar("V")


# Note: frozenlist is just a tuple at runtime, but we need a subscriptable alias
# for runtime use in cast() calls under Python 3.10/PyPy
class _FrozenListMeta(type):
    """Metaclass that makes frozenlist subscriptable at runtime."""
    def __getitem__(cls, item: Any) -> Any:
        return tuple

class frozenlist(metaclass=_FrozenListMeta):
    """A type alias for immutable lists (tuples). Subscriptable at runtime for cast() compatibility."""
    pass


@final
@dataclass(frozen=True)
class Just(Generic[T]):
    value: T

@final
class Nothing:
    __slots__ = ()
    def __repr__(self) -> str:
        return "Nothing"
    def __eq__(self, other: object) -> bool:
        return isinstance(other, Nothing)
    def __hash__(self) -> int:
        return hash("Nothing")

# Note: unused
NOTHING = Nothing()

# Maybe needs to be subscriptable at runtime for cast() compatibility in Python 3.10/PyPy
class _MaybeMeta(type):
    """Metaclass that makes Maybe subscriptable at runtime."""
    def __getitem__(cls, item: Any) -> Any:
        # Return a runtime-usable type for cast() - this is just for type checking
        return object

class Maybe(metaclass=_MaybeMeta):
    """A type alias for optional values (Just[T] | Nothing). Subscriptable at runtime for cast() compatibility."""
    pass


@final
@dataclass(frozen=True)
class Left(Generic[L]):
    value: L

@final
@dataclass(frozen=True)
class Right(Generic[R]):
    value: R

# Either needs to be subscriptable at runtime for cast() compatibility in Python 3.10/PyPy
class _EitherMeta(type):
    """Metaclass that makes Either subscriptable at runtime."""
    def __getitem__(cls, item: Any) -> Any:
        # Return a runtime-usable type for cast() - this is just for type checking
        return object

class Either(metaclass=_EitherMeta):
    """A type alias for sum types (Left[L] | Right[R]). Subscriptable at runtime for cast() compatibility."""
    pass


@dataclass(frozen=True, unsafe_hash=True, eq=True, order=True)
class Node(Generic[T]):
    """A wrapper for another type; a NewType alternative which allows type parameters."""

    value: T

    def __matmul__(self, other: Node) -> Node:
        """Apply this term to an argument (left-associative function application).

        Mirrors Haskell's @@ operator: f @@ a @@ b  becomes  f @ a @ b in Python.
        """
        from hydra.core import Application, TermApplication
        return type(self)(TermApplication(Application(self.value, other.value)))


def freeze(obj: Any) -> Any:
    """Recursively freeze an object into an immutable variant."""
    if isinstance(obj, dict):
        return FrozenDict(obj)
    elif isinstance(obj, set):
        return frozenset(freeze(item) for item in obj)
    elif isinstance(obj, list):
        return tuple(freeze(item) for item in obj)
    elif isinstance(obj, tuple):
        # Only recurse if there are mutable items (dicts/lists/sets) inside
        needs_freeze = False
        for item in obj:
            if isinstance(item, (dict, list, set)):
                needs_freeze = True
                break
        if needs_freeze:
            return tuple(freeze(item) for item in obj)
        return obj
    return obj


class FrozenDict(Mapping[K, V]):
    """An immutable variant of the Python dict."""

    __slots__ = ("_data", "_hash")

    def __init__(self, data: Any = None, *, _trusted: bool = False) -> None:
        """Create a FrozenDict. If _trusted=True, data is used directly without freezing."""
        if data is None:
            d: dict[Any, Any] = {}
        elif _trusted:
            d = data if isinstance(data, dict) else dict(data)
        elif isinstance(data, FrozenDict):
            d = data._data  # type: ignore[attr-defined]
        else:
            d = {}
            for k, v in (data.items() if isinstance(data, dict) else data):
                d[k] = freeze(v)
        object.__setattr__(self, "_data", d)
        object.__setattr__(self, "_hash", None)

    @property
    def data(self) -> dict[Any, Any]:
        return self._data  # type: ignore[attr-defined]

    def __getitem__(self, key: K) -> V:
        return self._data[key]  # type: ignore[attr-defined]

    def __iter__(self) -> Iterator[K]:
        return iter(self._data)  # type: ignore[attr-defined]

    def __len__(self) -> int:
        return len(self._data)  # type: ignore[attr-defined]

    def __hash__(self) -> int:
        h = self._hash  # type: ignore[attr-defined]
        if h is None:
            h = hash(frozenset(self._data.items()))  # type: ignore[attr-defined]
            object.__setattr__(self, "_hash", h)
        return h

    def __eq__(self, other: object) -> bool:
        if isinstance(other, FrozenDict):
            return self._data == other._data  # type: ignore[attr-defined]
        return NotImplemented

    def __or__(self, other: Mapping[K, V]) -> FrozenDict[K, V]:
        if isinstance(other, FrozenDict):
            new = {**self._data, **other._data}  # type: ignore[attr-defined]
        else:
            new = {**self._data, **other}  # type: ignore[attr-defined]
        return FrozenDict(new, _trusted=True)

    def copy(self, add_or_replace: dict[K, V] | None = None) -> FrozenDict[K, V]:
        new_data = dict(self._data)  # type: ignore[attr-defined]
        if add_or_replace is not None:
            new_data.update(add_or_replace)
        return FrozenDict(new_data, _trusted=True)

    def __repr__(self) -> str:
        return f"FrozenDict({self._data!r})"  # type: ignore[attr-defined]

    def _insert(self, key: K, value: V) -> FrozenDict[K, V]:
        """Fast insert without freeze overhead. Values must already be frozen."""
        new = dict(self._data)  # type: ignore[attr-defined]
        new[key] = value
        return FrozenDict(new, _trusted=True)

    def _union(self, other: FrozenDict[K, V]) -> FrozenDict[K, V]:
        """Fast union of two FrozenDicts without freeze overhead."""
        new = {**self._data, **other._data}  # type: ignore[attr-defined]
        return FrozenDict(new, _trusted=True)


def unsupported(message: str) -> None:
    """Raise NotImplementedError for unsupported features.

    This function is called by generated code when a feature is not yet supported
    in the Python backend (e.g., inline match expressions).
    """
    raise NotImplementedError(message)
