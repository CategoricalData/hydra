"""Tests for lazy Flow evaluation with if_else.

This module tests that the if_else function correctly implements lazy evaluation
for Flow (state monad) values, ensuring only the chosen branch is executed.

See: https://github.com/CategoricalData/hydra/issues/239
"""

import pytest
import sys
from pathlib import Path

# Add src/main/python to path
_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(_root / "main" / "python"))

import hydra.lib.flows as flows
import hydra.lib.logic as logic
import hydra.compute
import hydra.monads
from hydra.dsl.python import Just, Nothing


# Shared test fixtures
@pytest.fixture
def empty_trace():
    return hydra.monads.empty_trace


@pytest.fixture
def execution_log():
    """A mutable list to track which flows execute."""
    return []


def make_tracking_flow(log: list, name: str, value):
    """Create a flow that logs when it executes."""
    def run(state, trace):
        log.append(name)
        return hydra.compute.FlowState(Just(value), state, trace)
    return hydra.compute.Flow(run)


class TestIfElseLazyEvaluation:
    """Tests for if_else lazy evaluation with thunks."""

    def test_basic_true_branch(self):
        """if_else with non-callable values works correctly."""
        assert logic.if_else(True, "yes", "no") == "yes"
        assert logic.if_else(False, "yes", "no") == "no"

    def test_thunk_true_branch_only_called(self):
        """When condition is True, only the first thunk is called."""
        called = []

        def thunk_a():
            called.append("a")
            return "a_value"

        def thunk_b():
            called.append("b")
            return "b_value"

        result = logic.if_else(True, thunk_a, thunk_b)
        assert result == "a_value"
        assert called == ["a"], f"Expected only 'a' called, got {called}"

    def test_thunk_false_branch_only_called(self):
        """When condition is False, only the second thunk is called."""
        called = []

        def thunk_a():
            called.append("a")
            return "a_value"

        def thunk_b():
            called.append("b")
            return "b_value"

        result = logic.if_else(False, thunk_a, thunk_b)
        assert result == "b_value"
        assert called == ["b"], f"Expected only 'b' called, got {called}"


class TestFlowLazyEvaluation:
    """Tests for if_else lazy evaluation with Flow monads."""

    def test_flow_true_branch_only_executes(self, empty_trace, execution_log):
        """When condition is True, only the first Flow executes."""
        flow_a = make_tracking_flow(execution_log, "flow_a", "result_a")
        flow_b = make_tracking_flow(execution_log, "flow_b", "result_b")

        result_flow = logic.if_else(True, (lambda: flow_a), (lambda: flow_b))
        result = result_flow.value(0, empty_trace)

        assert execution_log == ["flow_a"], f"Expected only flow_a, got {execution_log}"
        assert result.value == Just("result_a")

    def test_flow_false_branch_only_executes(self, empty_trace, execution_log):
        """When condition is False, only the second Flow executes."""
        flow_a = make_tracking_flow(execution_log, "flow_a", "result_a")
        flow_b = make_tracking_flow(execution_log, "flow_b", "result_b")

        result_flow = logic.if_else(False, (lambda: flow_a), (lambda: flow_b))
        result = result_flow.value(0, empty_trace)

        assert execution_log == ["flow_b"], f"Expected only flow_b, got {execution_log}"
        assert result.value == Just("result_b")

    def test_nested_if_else_true_true(self, empty_trace, execution_log):
        """Nested if_else: outer=True, inner=True executes only flow_1."""
        flow_1 = make_tracking_flow(execution_log, "flow_1", 1)
        flow_2 = make_tracking_flow(execution_log, "flow_2", 2)
        flow_3 = make_tracking_flow(execution_log, "flow_3", 3)

        inner = logic.if_else(True, (lambda: flow_1), (lambda: flow_2))
        result_flow = logic.if_else(True, (lambda: inner), (lambda: flow_3))
        result = result_flow.value(0, empty_trace)

        assert execution_log == ["flow_1"], f"Expected [flow_1], got {execution_log}"
        assert result.value == Just(1)

    def test_nested_if_else_true_false(self, empty_trace, execution_log):
        """Nested if_else: outer=True, inner=False executes only flow_2."""
        flow_1 = make_tracking_flow(execution_log, "flow_1", 1)
        flow_2 = make_tracking_flow(execution_log, "flow_2", 2)
        flow_3 = make_tracking_flow(execution_log, "flow_3", 3)

        inner = logic.if_else(False, (lambda: flow_1), (lambda: flow_2))
        result_flow = logic.if_else(True, (lambda: inner), (lambda: flow_3))
        result = result_flow.value(0, empty_trace)

        assert execution_log == ["flow_2"], f"Expected [flow_2], got {execution_log}"
        assert result.value == Just(2)

    def test_nested_if_else_false(self, empty_trace, execution_log):
        """Nested if_else: outer=False skips inner entirely, executes flow_3."""
        flow_1 = make_tracking_flow(execution_log, "flow_1", 1)
        flow_2 = make_tracking_flow(execution_log, "flow_2", 2)
        flow_3 = make_tracking_flow(execution_log, "flow_3", 3)

        inner = logic.if_else(True, (lambda: flow_1), (lambda: flow_2))
        result_flow = logic.if_else(False, (lambda: inner), (lambda: flow_3))
        result = result_flow.value(0, empty_trace)

        assert execution_log == ["flow_3"], f"Expected [flow_3], got {execution_log}"
        assert result.value == Just(3)

    def test_if_else_inside_bind_true_branch(self, empty_trace, execution_log):
        """if_else inside bind: condition True executes only yes flow."""
        def conditional_continuation(x):
            flow_yes = make_tracking_flow(execution_log, "flow_yes", f"yes_{x}")
            flow_no = make_tracking_flow(execution_log, "flow_no", f"no_{x}")
            return logic.if_else(x > 5, (lambda: flow_yes), (lambda: flow_no))

        initial_flow = flows.pure(10)
        result_flow = flows.bind(initial_flow, conditional_continuation)
        result = result_flow.value(0, empty_trace)

        assert execution_log == ["flow_yes"], f"Expected [flow_yes], got {execution_log}"
        assert result.value == Just("yes_10")

    def test_if_else_inside_bind_false_branch(self, empty_trace, execution_log):
        """if_else inside bind: condition False executes only no flow."""
        def conditional_continuation(x):
            flow_yes = make_tracking_flow(execution_log, "flow_yes", f"yes_{x}")
            flow_no = make_tracking_flow(execution_log, "flow_no", f"no_{x}")
            return logic.if_else(x > 5, (lambda: flow_yes), (lambda: flow_no))

        initial_flow = flows.pure(3)
        result_flow = flows.bind(initial_flow, conditional_continuation)
        result = result_flow.value(0, empty_trace)

        assert execution_log == ["flow_no"], f"Expected [flow_no], got {execution_log}"
        assert result.value == Just("no_3")


class TestFlowBasicOperations:
    """Basic Flow operation tests to ensure the monad works correctly."""

    def test_pure_and_map(self, empty_trace):
        """pure followed by map produces correct result."""
        flow = flows.map(lambda x: x * 2, flows.pure(42))
        result = flow.value(None, empty_trace)
        assert result.value == Just(84)

    def test_bind(self, empty_trace):
        """bind correctly chains flows."""
        flow = flows.bind(flows.pure(10), lambda x: flows.pure(x + 5))
        result = flow.value(None, empty_trace)
        assert result.value == Just(15)

    def test_fail(self, empty_trace):
        """fail produces Nothing with error message."""
        flow = flows.fail("test error")
        result = flow.value(None, empty_trace)
        assert isinstance(result.value, Nothing)
        assert "test error" in result.trace.messages

    def test_map_list(self, empty_trace):
        """map_list applies flow-producing function to list."""
        flow = flows.map_list(lambda x: flows.pure(x * 2), (1, 2, 3))
        result = flow.value(None, empty_trace)
        assert result.value == Just((2, 4, 6))

    def test_sequence(self, empty_trace):
        """sequence converts list of flows to flow of list."""
        flow = flows.sequence((flows.pure(1), flows.pure(2), flows.pure(3)))
        result = flow.value(None, empty_trace)
        assert result.value == Just((1, 2, 3))
