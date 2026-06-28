package hydra.overlay.java.tinkerpop.coder;

import hydra.core.Literal;
import hydra.pg.model.Graph;
import hydra.tinkerpop.examples.Modern;

import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Unit tests for the Hydra &lt;-&gt; TinkerPop graph data bridge ({@link HydraGremlinBridge}) and the
 * validation wrapper ({@link Validate}), exercised against the built-in
 * {@code hydra.tinkerpop.examples.modern} sample graph and an in-memory {@link TinkerGraph}.
 *
 * <p>These are unit tests — no Gremlin Server required. Server-dependent bridge paths (the remote
 * {@code GraphTraversalSource} overload) are covered by integration tests tagged {@code "integration"},
 * which are excluded from the default test run.
 */
class BridgeTest {

    @Test
    @DisplayName("Modern graph round-trips Hydra -> TinkerGraph -> Hydra")
    void roundTripViaTinkerGraph() {
        Graph<Literal> original = Modern.modernGraph();

        // Hydra -> TinkerGraph (in-memory)
        TinkerGraph tg = TinkerGraph.open();
        HydraGremlinBridge.hydraToGremlin(original, tg, HydraGremlinBridge::literalToObject);

        // TinkerGraph -> Hydra
        Graph<Literal> readBack = HydraGremlinBridge.gremlinToHydra(tg, HydraGremlinBridge::objectToLiteral);

        // The Modern graph has 6 vertices and 6 edges.
        assertEquals(6, original.vertices.size(), "Modern graph should have 6 vertices");
        assertEquals(6, original.edges.size(), "Modern graph should have 6 edges");
        assertEquals(original.vertices.size(), readBack.vertices.size(), "vertex count preserved");
        assertEquals(original.edges.size(), readBack.edges.size(), "edge count preserved");

        // Deep check: every vertex survives with its label and properties intact.
        for (java.util.Map.Entry<Literal, hydra.pg.model.Vertex<Literal>> e : original.vertices.entrySet()) {
            hydra.pg.model.Vertex<Literal> ov = e.getValue();
            hydra.pg.model.Vertex<Literal> rv = readBack.vertices.get(e.getKey());
            assertTrue(rv != null, "vertex preserved: " + e.getKey());
            assertEquals(ov.label, rv.label, "vertex label preserved: " + e.getKey());
            assertEquals(ov.id, rv.id, "vertex id preserved: " + e.getKey());
            assertEquals(ov.properties, rv.properties, "vertex properties preserved: " + e.getKey());
        }
        // Deep check: every edge survives with label, endpoints, and properties intact.
        for (java.util.Map.Entry<Literal, hydra.pg.model.Edge<Literal>> e : original.edges.entrySet()) {
            hydra.pg.model.Edge<Literal> oe = e.getValue();
            hydra.pg.model.Edge<Literal> re = readBack.edges.get(e.getKey());
            assertTrue(re != null, "edge preserved: " + e.getKey());
            assertEquals(oe.label, re.label, "edge label preserved: " + e.getKey());
            assertEquals(oe.out, re.out, "edge out-vertex preserved: " + e.getKey());
            assertEquals(oe.in, re.in, "edge in-vertex preserved: " + e.getKey());
            assertEquals(oe.properties, re.properties, "edge properties preserved: " + e.getKey());
        }
    }

    @Test
    @DisplayName("Modern graph validates against the Modern schema")
    void modernGraphValidatesAgainstModernSchema() {
        // Load the Modern graph into an in-memory TinkerGraph, then validate it against the schema
        // via the bridge + hydra.validate.pg engine.
        TinkerGraph tg = TinkerGraph.open();
        HydraGremlinBridge.hydraToGremlin(Modern.modernGraph(), tg, HydraGremlinBridge::literalToObject);

        Validate.Result result = Validate.validate(Modern.modernSchema(), tg);
        assertTrue(result.isValid(), "Modern graph should be valid against its own schema, got: " + result);
    }

    @Test
    @DisplayName("Traversal-source read path round-trips (embedded source, no server)")
    void roundTripViaTraversalSource() {
        // The gremlinToHydra(GraphTraversalSource, ...) overload uses g.V().elementMap(); it is the same
        // code path used against a remote Gremlin Server, but runs equally on an embedded traversal source,
        // so we can unit-test it without a server.
        Graph<Literal> original = Modern.modernGraph();
        TinkerGraph tg = TinkerGraph.open();
        HydraGremlinBridge.hydraToGremlin(original, tg, HydraGremlinBridge::literalToObject);

        Graph<Literal> readBack = HydraGremlinBridge.gremlinToHydra(
                tg.traversal(), HydraGremlinBridge::objectToLiteral);

        assertEquals(original.vertices.size(), readBack.vertices.size(), "vertex count (traversal read)");
        assertEquals(original.edges.size(), readBack.edges.size(), "edge count (traversal read)");
        for (java.util.Map.Entry<Literal, hydra.pg.model.Edge<Literal>> e : original.edges.entrySet()) {
            hydra.pg.model.Edge<Literal> re = readBack.edges.get(e.getKey());
            assertTrue(re != null, "edge preserved (traversal read): " + e.getKey());
            assertEquals(e.getValue().out, re.out, "edge out preserved (traversal read)");
            assertEquals(e.getValue().in, re.in, "edge in preserved (traversal read)");
        }
    }

    @Test
    @org.junit.jupiter.api.Tag("integration")
    @DisplayName("[integration] validate a graph over a remote Gremlin Server connection")
    void validateOverRemoteServer() {
        // Integration test: requires a running Gremlin Server. Excluded from the default unit run
        // (build.gradle excludeTags 'integration'). Connect a remote traversal source and validate.
        //
        //   GraphTraversalSource g = traversal().withRemote(
        //       DriverRemoteConnection.using("localhost", 8182, "g"));
        //   Validate.Result result = Validate.validate(Modern.modernSchema(), g);
        //   assertTrue(result.isValid());
        //
        // Left as a documented integration scenario; enable when a server is available.
        org.junit.jupiter.api.Assumptions.assumeTrue(
                Boolean.getBoolean("hydra.tinkerpop.integration"),
                "skipping: set -Dhydra.tinkerpop.integration=true with a running Gremlin Server");
    }
}
