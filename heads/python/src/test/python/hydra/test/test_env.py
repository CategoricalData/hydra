# Hand-written test environment for Python.
# Provides a real graph with primitives registered, mirroring Haskell's TestEnv.hs.
# Delegates to the test_suite_runner's build_test_graph for the full graph setup.
#
# Note: the functions below are named test_graph / test_context to match the
# import contract used by the patched dist/.../test_graph.py and the demos.
# They are NOT pytest test cases — the __test__ = False markers tell pytest's
# auto-discovery to skip them.

import hydra.context

_cached_graph = None
_cached_context = None

def test_graph(test_types=None):
    global _cached_graph
    if _cached_graph is None:
        from test_suite_runner import build_test_graph
        _cached_graph = build_test_graph()
    return _cached_graph
test_graph.__test__ = False  # type: ignore[attr-defined]

def test_context():
    global _cached_context
    if _cached_context is None:
        _cached_context = hydra.context.Context((), (), {})
    return _cached_context
test_context.__test__ = False  # type: ignore[attr-defined]
