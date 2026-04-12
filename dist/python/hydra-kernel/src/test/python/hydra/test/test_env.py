# Hand-written test environment for Python.
# Provides a real graph with primitives registered, mirroring Haskell's TestEnv.hs.
# Delegates to the test_suite_runner's build_test_graph for the full graph setup.

import hydra.context

_cached_graph = None
_cached_context = None

def test_graph():
    global _cached_graph
    if _cached_graph is None:
        from test_suite_runner import build_test_graph
        _cached_graph = build_test_graph()
    return _cached_graph

def test_context():
    global _cached_context
    if _cached_context is None:
        _cached_context = hydra.context.Context((), (), {})
    return _cached_context
