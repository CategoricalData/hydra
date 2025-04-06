"""Python implementations of hydra.lib.flows primitives. These are simply wrappers around hydra.flows functions."""
# module Hydra.Lib.Flows where

# import Hydra.Compute
# import qualified Hydra.Flows as Flows

# import qualified Control.Monad as CM

# -- Haskell-specific helpers

# instance Functor (Flow s) where
#   fmap = CM.liftM
# instance Applicative (Flow s) where
#   pure = Flows.pureInternal
#   (<*>) = CM.ap
# instance Monad (Flow s) where
#   (>>=) = Flows.bind
# instance MonadFail (Flow s) where
#   fail = Flows.failInternal

# -- Primitive functions

# apply :: Flow s (x -> y) -> Flow s x -> Flow s y
# apply = (<*>)


# bind :: Flow s x -> (x -> Flow s y) -> Flow s y
# bind = Flows.bind

# fail :: String -> Flow s x
# fail = Flows.failInternal

# map :: (x -> y) -> Flow s x -> Flow s y
# map = fmap

# mapList :: (x -> Flow s y) -> [x] -> Flow s [y]
# mapList = CM.mapM

# pure :: x -> Flow s x
# pure = Flows.pureInternal

# sequence :: [Flow s x] -> Flow s [x]
# sequence = CM.sequence
from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass
from typing import TypeVar, cast, override

from hydra.dsl.python import Monad
from hydra.graph import Graph

A = TypeVar("A")
B = TypeVar("B")


@dataclass
class Flow(Monad[A]):
    """A state monad for Graph computation."""

    run_fn: Callable[[Graph], tuple[Graph, A]]

    def __call__(self, state: Graph) -> tuple[Graph, A]:
        """Run the flow computation with the given state."""
        return self.run_fn(state)

    @classmethod
    @override
    def pure(cls, value: A) -> Flow[A]:
        """Lift a value into the Flow monad."""
        return Flow(lambda s: (s, value))

    @override
    def bind(self, f: Callable[[A], Monad[B]]) -> Flow[B]:
        def run_bound(state: Graph) -> tuple[Graph, B]:
            new_state, a = self(state)
            return cast(Flow[B], f(a))(new_state)

        return Flow(run_bound)

    @classmethod
    def get_state(cls) -> Flow[Graph]:
        """Get the current state."""
        return Flow(lambda s: (s, s))
