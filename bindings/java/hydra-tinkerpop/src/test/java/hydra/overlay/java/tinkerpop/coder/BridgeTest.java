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

        // Spot-check: every original vertex id survives the round-trip.
        for (Literal id : original.vertices.keySet()) {
            assertTrue(readBack.vertices.containsKey(id), "vertex id preserved: " + id);
        }
        for (Literal id : original.edges.keySet()) {
            assertTrue(readBack.edges.containsKey(id), "edge id preserved: " + id);
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
}
