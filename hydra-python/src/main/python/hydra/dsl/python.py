"""A collection of foundational typing constructs specific to Hydra-Python."""

from __future__ import annotations
from collections.abc import Iterator, Mapping
from dataclasses import dataclass, field
from typing import Any, Generic, TypeVar, cast, final


K = TypeVar("K")
T = TypeVar("T")
V = TypeVar("V")


type frozenlist[T] = tuple[T, ...]


@final
@dataclass(frozen=True)
class Just(Generic[T]):
    value: T

@final
class Nothing:
    __slots__ = ()
    def __repr__(self) -> str:
        return "Nothing"

# Note: unused
NOTHING = Nothing()

type Maybe[T] = Just[T] | Nothing


@dataclass(frozen=True, unsafe_hash=True, eq=True, order=True)
class Node(Generic[T]):
    """A wrapper for another type; a NewType alternative which allows type parameters."""

    value: T


def freeze(obj: Any) -> Any:
    """Recursively freeze an object into an immutable variant."""
    match obj:
        case dict():
            obj = cast(dict[Any, Any], obj)
            return FrozenDict(obj)
        case set():
            obj = cast(set[Any], obj)
            return frozenset(freeze(item) for item in obj)
        case tuple() | list():
            obj = cast(tuple[Any, ...], obj)
            return tuple(freeze(item) for item in obj)
        case _:
            return obj


@dataclass(frozen=True)
class FrozenDict(Mapping[K, V]):
    """An immutable variant of the Python dict."""

    data: Mapping[K, V] = field(default_factory=dict)

    def __post_init__(self) -> None:
        """Convert the provided data dict into an immutable mapping."""
        frozen_data: dict[Any, Any] = {}
        for k, v in self.data.items():
            frozen_data[k] = freeze(v)
        object.__setattr__(self, "data", frozen_data)

    def __getitem__(self, key: K) -> V:
        """Get the value for a given key from the FrozenDict."""
        return self.data[key]

    def __iter__(self) -> Iterator[K]:
        """Get an iterator over the keys of the FrozenDict."""
        return iter(self.data)

    def __len__(self) -> int:
        """Get the number of items in the FrozenDict."""
        return len(self.data)

    def __hash__(self) -> int:
        """Compute a hash based on the frozenset of items so that order does not matter."""
        return hash(frozenset(self.data.items()))

    def __or__(self, other: Mapping[K, V]) -> FrozenDict[K, V]:
        """Merge two FrozenDicts."""
        return FrozenDict[K, V]({**self.data, **other})

    def copy(self, add_or_replace: dict[K, V] | None = None) -> FrozenDict[K, V]:
        """Create a new FrozenDict with updated entries."""
        if add_or_replace is None:
            add_or_replace = {}

        new_data = dict(self.data)
        new_data.update(add_or_replace)
        return FrozenDict(new_data)

    def __repr__(self) -> str:
        """Get a string representation of the FrozenDict."""
        return f"FrozenDict({dict(self.data)})"
