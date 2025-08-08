"""A collection of foundational typing constructs specific to Hydra-Python."""

from __future__ import annotations

from abc import ABC, abstractmethod
from collections.abc import Callable, Iterator, Mapping
from dataclasses import dataclass, field
from typing import Any, Generic, TypeVar, cast, override

T = TypeVar("T")
K = TypeVar("K")
V = TypeVar("V")


A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")

M = TypeVar("M", bound="Monad[Any]")

S = TypeVar("S")


type frozenlist[T] = tuple[T, ...]


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


class Monad(ABC, Generic[A]):
    """A monad is a type that supports a bind operation."""

    @classmethod
    @abstractmethod
    def pure(cls: type[M], value: A) -> Monad[A]:
        """Construct a monad with a value."""
        raise NotImplementedError("Not implemented")

    @abstractmethod
    def bind(self, f: Callable[[A], Monad[B]]) -> Monad[B]:
        """Bind a function to a monad."""
        raise NotImplementedError("Not implemented")

    def then(self, f: Callable[[A], B]) -> Monad[B]:
        """Compose a function with a monad."""
        return self.lift(f)(self)

    @classmethod
    def map(cls: type[M], f: Callable[[A], B], ma: Monad[A]) -> Monad[B]:
        """Map a function over a monadic value."""
        return ma.bind(lambda x: cast(Monad[B], cls).pure(f(x)))

    @classmethod
    def join(cls: type[M], mma: Monad[Monad[A]]) -> Monad[A]:
        """Flatten a nested monad."""
        return mma.bind(lambda ma: ma)

    @classmethod
    def apply(cls, mf: Monad[Callable[[A], B]], ma: Monad[A]) -> Monad[B]:
        """Apply a monadic function to a monadic value (Applicative functor operation)."""
        return mf.bind(lambda f: ma.bind(lambda a: cast(Monad[B], cls).pure(f(a))))

    @staticmethod
    def compose(
        f: Callable[[A], Monad[B]], g: Callable[[B], Monad[C]]
    ) -> Callable[[A], Monad[C]]:
        """Compose two monadic functions (Kleisli composition)."""
        return lambda x: f(x).bind(g)

    @classmethod
    def lift(cls, f: Callable[[A], B]) -> Callable[[Monad[A]], Monad[B]]:
        """Lift a function to a monadic function."""
        return lambda ma: cls.map(f, ma)


class MonadPlus(Monad[A], ABC):
    """A monad that supports a plus operation."""

    @classmethod
    @abstractmethod
    def empty(cls) -> MonadPlus[A]:
        """Construct an empty monad."""
        raise NotImplementedError("Not implemented")

    @abstractmethod
    def plus(self, other: MonadPlus[A]) -> MonadPlus[A]:
        """Combine two monads."""
        raise NotImplementedError("Not implemented")

    def __add__(self, other: MonadPlus[A]) -> MonadPlus[A]:
        """Combine two monads."""
        return self.plus(other)


@dataclass
class Maybe(MonadPlus[A]):
    """A monad that represents an optional value."""

    value: A | None

    @classmethod
    @override
    def pure(cls, value: A) -> Maybe[A]:
        return cls(value)

    @override
    def bind(self, f: Callable[[A], Monad[B]]) -> Monad[B]:
        return f(self.value) if self.value is not None else Maybe(None)

    @classmethod
    @override
    def empty(cls) -> Maybe[A]:
        return cls(None)

    @override
    def plus(self, other: MonadPlus[A]) -> MonadPlus[A]:
        other = cast(Maybe[A], other)
        return Maybe[A](self.value if self.value is not None else other.value)


@dataclass
class Either(Generic[A, B], MonadPlus[A]):
    """A monad that represents an either value."""


@dataclass
class List(MonadPlus[A]):
    """A monad that represents a list."""

    value: list[A]

    @classmethod
    @override
    def pure(cls, value: A) -> List[A]:
        return List[A]([value])

    @override
    def bind(self, f: Callable[[A], Monad[B]]) -> List[B]:
        return List[B]([b for a in self.value for b in cast(List[B], f(a)).value])

    @classmethod
    @override
    def empty(cls) -> List[A]:
        return List[A]([])

    @override
    def plus(self, other: MonadPlus[A]) -> MonadPlus[A]:
        other = cast(List[A], other)
        return List[A](self.value + other.value)


@dataclass
class State(Monad[A], Generic[A, S]):
    """A monad that represents a stateful computation."""

    run: Callable[[S], tuple[S, A]]

    @classmethod
    @override
    def pure(cls, value: A) -> State[A, S]:
        def run(s: S) -> tuple[S, A]:
            return (s, value)

        return State[A, S](run)

    @override
    def bind(self, f: Callable[[A], Monad[B]]) -> State[B, S]:
        def run(s: S) -> tuple[S, B]:
            s, a = self.run(s)
            return cast(State[B, S], f(a)).run(s)

        return State[B, S](run)
