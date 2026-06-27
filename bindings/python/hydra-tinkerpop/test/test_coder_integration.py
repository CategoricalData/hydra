"""Integration tests for the Python TinkerPop bridge — require a running Gremlin Server.

These exercise the full graph round-trip (hydra_to_gremlin / gremlin_to_hydra) and validation
against a live server via gremlinpython. They are skipped unless a server is configured, because
gremlinpython is a remote-only client (there is no embedded TinkerGraph in Python, unlike Java).

To run: start a Gremlin Server on localhost:8182 and set HYDRA_TINKERPOP_INTEGRATION=1, e.g.

    HYDRA_TINKERPOP_INTEGRATION=1 pytest test/test_coder_integration.py

pytest collects these only when the marker is enabled; otherwise they skip.
"""

import os
import pytest

pytestmark = pytest.mark.integration

_ENABLED = os.environ.get("HYDRA_TINKERPOP_INTEGRATION") == "1"
_SKIP_REASON = "set HYDRA_TINKERPOP_INTEGRATION=1 with a running Gremlin Server on localhost:8182"


def _remote_traversal():
    from gremlin_python.process.anonymous_traversal import traversal
    from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
    return traversal().with_remote(DriverRemoteConnection("ws://localhost:8182/gremlin", "g"))


@pytest.mark.skipif(not _ENABLED, reason=_SKIP_REASON)
def test_modern_graph_round_trips_via_server():
    from hydra.tinkerpop.examples.modern import modern_graph
    from hydra.overlay.python.tinkerpop import coder

    g = _remote_traversal()
    try:
        g.V().drop().iterate()
        original = modern_graph()
        coder.hydra_to_gremlin(original, g)
        read_back = coder.gremlin_to_hydra(g)
        assert len(read_back.vertices) == len(original.vertices)
        assert len(read_back.edges) == len(original.edges)
    finally:
        g.close()


@pytest.mark.skipif(not _ENABLED, reason=_SKIP_REASON)
def test_modern_graph_validates_via_server():
    from hydra.tinkerpop.examples.modern import modern_graph, modern_schema
    from hydra.overlay.python.tinkerpop import coder

    g = _remote_traversal()
    try:
        g.V().drop().iterate()
        coder.hydra_to_gremlin(modern_graph(), g)
        result = coder.validate(modern_schema(), g)
        assert result.is_valid, f"Modern graph should validate against its schema, got: {result!r}"
    finally:
        g.close()
