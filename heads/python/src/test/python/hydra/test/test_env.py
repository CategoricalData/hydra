# Hand-written test environment for Python.
# Provides the test graph and test context referenced by the generated
# hydra.test.test_graph module. Mirrors the role of
# heads/java/.../TestEnv.java and heads/scala/.../testEnv.scala.
#
# The DSL declares:
#   hydra.test.testEnv.testContext :: InferenceContext
#   hydra.test.testEnv.testGraph   :: Map Name Type -> Map Name Term -> Graph
#
# This file MUST expose those two FQNs at the same arity:
#   - test_context: an InferenceContext value (not a function)
#   - test_graph(test_types, test_terms): a function returning a Graph
#
# The Python coder filters hydra.test.testEnv from emitted output (via
# testSkipEmitNamespaces in Hydra.Sources.Test.All); this file is the
# Python runtime counterpart that the generated test_graph.py resolves
# against at import time.

import hydra.typing
import hydra.lib.maps

# Test context: an empty InferenceContext value. Eager — no side effects beyond
# the InferenceContext constructor.
test_context = hydra.typing.InferenceContext(fresh_type_variable_count=0, trace=())

# Test graph: a function (matches DSL signature
# Map Name Type -> Map Name Term -> Graph). Cached on first call so
# repeated accesses are cheap. Imports test_suite_runner inside the
# function body to defer the circular import (test_suite_runner imports
# hydra.test.test_graph, which imports this module). Both arguments are
# accepted for signature parity but ignored — the test_suite_runner
# already maintains the live test graph used by all Python tests.
_cached_graph = None

def test_graph(test_types=None, test_terms=None):
    global _cached_graph
    if _cached_graph is None:
        from test_suite_runner import build_test_graph
        _cached_graph = build_test_graph()
    return _cached_graph
test_graph.__test__ = False  # type: ignore[attr-defined]
