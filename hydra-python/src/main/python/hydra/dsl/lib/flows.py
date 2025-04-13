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
from typing import TypeVar

from hydra.compute import Flow, FlowState, Trace
from hydra.core import Unit
from hydra.dsl.python import FrozenDict  # Monad

# from hydra.graph import Graph

A = TypeVar("A")
B = TypeVar("B")
S = TypeVar("S")

# @dataclass
# class Flow(Monad[A]):
#     """A state monad for Graph computation."""

#     run: Callable[[Graph], tuple[Graph, A]]

#     def __call__(self, state: Graph) -> tuple[Graph, A]:
#         """Run the flow computation with the given state."""
#         return self.run(state)

#     @classmethod
#     @override
#     def pure(cls, value: A) -> Flow[A]:
#         """Lift a value into the Flow monad."""
#         return Flow(lambda s: (s, value))

#     @override
#     def bind(self, f: Callable[[A], Monad[B]]) -> Flow[B]:
#         def run(state: Graph) -> tuple[Graph, B]:
#             state, a = self(state)
#             return cast(Flow[B], f(a))(state)

#         return Flow(run)

#     @classmethod
#     def get_state(cls) -> Flow[Graph]:
#         """Get the current state."""
#         return Flow(lambda s: (s, s))


# module Hydra.Dsl.Lib.Flows where

# import Hydra.Dsl.Base
# import Hydra.Core
# import Hydra.Compute
# import Hydra.Phantoms
# import Hydra.Sources.Libraries
# import qualified Hydra.Dsl.Types as Types

# import qualified Data.Map as M


# -- Primitives


# apply :: TTerm (Flow s (x -> y)) -> TTerm (Flow s x) -> TTerm (Flow s y)
# apply = primitive2 _flows_apply


# bind :: TTerm (Flow s x) -> TTerm (x -> Flow s y) -> TTerm (Flow s y)
# bind = primitive2 _flows_bind


# fail :: TTerm String -> TTerm (Flow s x)
# fail = primitive1 _flows_fail

# map :: TTerm (x -> y) -> TTerm (Flow s x) -> TTerm (Flow s y)
# map = primitive2 _flows_map

# mapList :: TTerm (x -> Flow s y) -> TTerm [x] -> TTerm (Flow s [y])
# mapList = primitive2 _flows_mapList

# pure :: TTerm x -> TTerm (Flow s x)
# pure = primitive1 _flows_pure

# sequence :: TTerm [Flow s a] -> TTerm (Flow s [a])
# sequence = primitive1 _flows_sequence

# -- Accessors

# flowState :: TTerm (Maybe x) -> TTerm s -> TTerm Trace -> TTerm (FlowState s x)
# flowState value state trace = record _FlowState [
#   _FlowState_value>>: value,
#   _FlowState_state>>: state,
#   _FlowState_trace>>: trace]

# flowStateState :: TTerm (FlowState s x -> s)
# flowStateState = project _FlowState _FlowState_state

# flowStateTrace :: TTerm (FlowState s x -> Trace)
# flowStateTrace = project _FlowState _FlowState_trace

# flowStateValue :: TTerm (FlowState s x -> Maybe x)
# flowStateValue = project _FlowState _FlowState_value

# trace :: TTerm [String] -> TTerm [String] -> TTerm (M.Map String (Term)) -> TTerm Trace
# trace stack messages other = record _Trace [
#   _Trace_stack>>: stack,
#   _Trace_messages>>: messages,
#   _Trace_other>>: other]

# traceStack :: TTerm (Trace -> [String])
# traceStack = project _Trace _Trace_stack

# traceMessages :: TTerm (Trace -> [String])
# traceMessages = project _Trace _Trace_messages

# traceOther :: TTerm (Trace -> M.Map String (Term))
# traceOther = project _Trace _Trace_other

# unFlow :: TTerm (Flow s x -> s -> Trace -> FlowState s x)
# unFlow = unwrap _Flow


# class Flow(Node["Callable[[S, Trace], FlowState[S, V]]"], Generic[S, V]):
#     """A variant of the State monad with built-in logging and error handling."""

# FLOW__NAME = hydra.core.Name("hydra.compute.Flow")

# @dataclass
# class FlowState(Generic[S, V]):
#     """The result of evaluating a Flow."""

#     value: V | None
#     state: S
#     trace: Trace

# FLOW_STATE__NAME = hydra.core.Name("hydra.compute.FlowState")
# FLOW_STATE__VALUE__NAME = hydra.core.Name("value")
# FLOW_STATE__STATE__NAME = hydra.core.Name("state")
# FLOW_STATE__TRACE__NAME = hydra.core.Name("trace")

# @dataclass
# class Trace:
#     """A container for logging and error information."""

#     stack: frozenlist[str]
#     messages: frozenlist[str]
#     other: Annotated[FrozenDict[hydra.core.Name, hydra.core.Term], "A map of string keys to arbitrary terms as values, for application-specific use"]

# TRACE__NAME = hydra.core.Name("hydra.compute.Trace")
# TRACE__STACK__NAME = hydra.core.Name("stack")
# TRACE__MESSAGES__NAME = hydra.core.Name("messages")
# TRACE__OTHER__NAME = hydra.core.Name("other")

# from hydra.compute import Trace


class Flows:
    """A collection of convenience methods for constructing and composing flows, or stateful computations."""

    EMPTY_TRACE = Trace(
        tuple(),
        tuple(),
        FrozenDict(),
    )

    MAX_MAPM_SIZE = 1000

    UNIT = Unit()

    @staticmethod
    def pure[S, A](value: A, meaningless: S | None = None) -> Flow[S, A]:
        """Lift a value into the Flow monad."""

        def run(state: S, trace: Trace) -> FlowState[S, A]:
            return FlowState[S, A](value=value, state=state, trace=trace)

        return Flow[S, A](run)

    @staticmethod
    def bind[S, A, B](flow: Flow[S, A], f: Callable[[A], Flow[S, B]]) -> Flow[S, B]:
        """Monadic bind function for flows."""

        def run(state: S, trace: Trace) -> FlowState[S, B]:
            flow_state = flow.value(state, trace)

            return (
                FlowState[S, B](
                    value=None,
                    state=state,
                    trace=trace,
                )
                if flow_state.value is None
                else f(flow_state.value).value(flow_state.state, flow_state.trace)
            )

        return Flow(run)

    @staticmethod
    def apply[S, A, B](flow: Flow[S, Callable[[A], B]], x: Flow[S, A]) -> Flow[S, B]:
        """Apply a function flow to a domain value flow."""

        def run(state: S, trace: Trace) -> FlowState[S, B]:
            flow_state = flow.value(state, trace)

            return (
                FlowState[S, B](
                    value=None,
                    state=state,
                    trace=trace,
                )
                if flow_state.value is None
                else Flows.map(flow_state.value, x).value(
                    flow_state.state, flow_state.trace
                )
            )

        return Flow(run)

    @staticmethod
    def map[S, A, B](f: Callable[[A], B], x: Flow[S, A]) -> Flow[S, B]:
        """Map a function over a flow."""

        def run(state: S, trace: Trace) -> FlowState[S, B]:
            flow_state = x.value(state, trace)
            return FlowState[S, B](
                value=f(flow_state.value) if flow_state.value is not None else None,
                state=flow_state.state,
                trace=flow_state.trace,
            )

        return Flow[S, B](run)

    @staticmethod
    def compose[S, A, B, C](
        f: Callable[[A], Flow[S, B]], g: Callable[[B], Flow[S, C]]
    ) -> Callable[[A], Flow[S, C]]:
        """Compose two monadic functions, feeding the output of the first into the second."""

        def run(a: A) -> Flow[S, C]:
            return Flows.bind(f(a), g)

        return run

    @staticmethod
    def consume[S, A](flow: Flow[S, A], consumer: Callable[[A], None]) -> Flow[S, Unit]:
        """Evaluate a flow and consume the result."""

        def f(a: A) -> Unit:
            consumer(a)
            return Unit()

        return Flows.map(f, flow)

    @staticmethod
    def fail[S, A](msg: str, meaningless: S | A | None = None) -> Flow[S, A]:
        """Produce a failure flow with the provided message."""

        def run(state: S, trace: Trace) -> FlowState[S, A]:
            return FlowState[S, A](
                value=None,
                state=state,
                trace=Trace(
                    stack=trace.stack,
                    messages=trace.messages + tuple([msg]),
                    other=trace.other,
                ),
            )

        return Flow[S, A](run)

    @staticmethod
    def fail_with_exception[S, A](
        msg: str, cause: Exception, meaningless: S | A | None = None
    ) -> Flow[S, A]:
        """Produce a failure flow with the provided message and additional information from a Throwable."""
        return Flows.fail(msg + ": " + str(cause))

    @staticmethod
    def check[A](x: A, *predicates: Callable[[A], str | None]) -> Flow[Unit, A]:
        """Check whether a given value satisfies a list of predicates.

        Returns the value itself if all checks are successful, or a
        failure flow for the first predicate that fails.
        """
        for predicate in predicates:
            msg = predicate(x)
            if msg is not None:
                return Flows.fail(msg)

        return Flows.pure(x)

    @staticmethod
    def from_flow[S, A](state: S, flow: Flow[S, A]) -> A:
        """Extract the value from a flow, throwing an exception if the flow failed."""
        flow_state = flow.value(state, Flows.EMPTY_TRACE)
        if flow_state.value is None:
            raise Exception(flow_state.trace)
        return flow_state.value

    @staticmethod
    def get_state[S]() -> Flow[S, S]:
        """Get the current state."""

        def run(state: S, trace: Trace) -> FlowState[S, S]:
            return FlowState[S, S](value=state, state=state, trace=trace)

        return Flow[S, S](run)
