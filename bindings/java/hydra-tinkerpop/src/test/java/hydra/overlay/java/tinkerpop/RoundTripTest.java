package hydra.overlay.java.tinkerpop;

import hydra.tinkerpop.gremlin.RootTraversal;

import org.apache.tinkerpop.gremlin.process.traversal.Bytecode;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Round-trip smoke tests for the Hydra ↔ TinkerPop Gremlin mapping.
 *
 * <p>Each case starts from a Gremlin query string, parses it into the Hydra
 * {@code hydra.tinkerpop.gremlin} model (via TinkerPop's own ANTLR parser →
 * {@link Bytecode} → {@link BytecodeToHydra}), then maps the Hydra model back to a
 * {@link Bytecode} ({@link HydraToBytecode}) and verifies:
 *
 * <ol>
 *   <li><b>Hydra round-trip:</b> Bytecode → Hydra → Bytecode → Hydra is stable (the second Hydra value
 *       equals the first), exercising both directions over the same traversal.</li>
 *   <li><b>Bytecode equality:</b> the Bytecode produced from Hydra equals the Bytecode we started from,
 *       so the forward and reverse mappers are mutually consistent.</li>
 * </ol>
 *
 * <p>The queries span the step categories the mapping covers: element access, adjacency, predicates
 * (P/TextP), filters, projection, labels, aggregation, repeat/until, choose, and value access.
 */
class RoundTripTest {

    private static Bytecode bytecodeOf(String gremlin) {
        Object parsed = org.apache.tinkerpop.gremlin.language.grammar.GremlinQueryParser.parse(gremlin);
        return ((org.apache.tinkerpop.gremlin.process.traversal.Traversal<?, ?>) parsed).asAdmin().getBytecode();
    }

    /** Asserts that gremlin → Hydra → Bytecode equals the original Bytecode, and Hydra is stable. */
    private static void assertRoundTrips(String gremlin) {
        Bytecode original = bytecodeOf(gremlin);

        RootTraversal hydra = BytecodeToHydra.fromBytecode(original);
        assertNotNull(hydra, "parse produced a null Hydra model for: " + gremlin);

        Bytecode remapped = HydraToBytecode.toBytecode(hydra);
        // Exact Bytecode equality: the Gremlin model now tags numeric literals with precision
        // (IntegerLiteral byte/short/int/long/big, FloatLiteral float/double/big), and the binding applies
        // per-step long coercion (limit/range/tail/skip), so forward∘reverse reproduces the parser's
        // exact boxing — Integer(2) for times(2), Long(5) for limit(5), etc.
        assertEquals(original, remapped, "Bytecode mismatch after Hydra round-trip for: " + gremlin);

        // Hydra stability (exact): mapping the remapped Bytecode back to Hydra yields an equal Hydra value.
        RootTraversal hydra2 = BytecodeToHydra.fromBytecode(remapped);
        assertEquals(hydra, hydra2, "Hydra model not stable across round-trip for: " + gremlin);
    }

    @Test @DisplayName("element access + adjacency")
    void elementAndAdjacency() {
        assertRoundTrips("g.V()");
        assertRoundTrips("g.V().out()");
        assertRoundTrips("g.V().out('knows')");
        assertRoundTrips("g.V().out('knows').in('created')");
        assertRoundTrips("g.V().both().bothE().bothV()");
        assertRoundTrips("g.E()");
    }

    @Test @DisplayName("value access + labels")
    void valuesAndLabels() {
        assertRoundTrips("g.V().values('name')");
        assertRoundTrips("g.V().id()");
        assertRoundTrips("g.V().label()");
        assertRoundTrips("g.V().as('a').out().as('b')");
    }

    @Test @DisplayName("has + predicates (P / TextP)")
    void hasAndPredicates() {
        assertRoundTrips("g.V().has('name','marko')");
        assertRoundTrips("g.V().has('age', P.gt(30))");
        assertRoundTrips("g.V().has('age', P.between(20, 40))");
        assertRoundTrips("g.V().has('name', TextP.startingWith('mar'))");
        assertRoundTrips("g.V().hasLabel('person')");
        assertRoundTrips("g.V().hasLabel('person','software')");
        assertRoundTrips("g.V().is(P.gt(10))");
    }

    @Test @DisplayName("filters + boolean composition")
    void filtersAndComposition() {
        assertRoundTrips("g.V().filter(__.out('knows'))");
        assertRoundTrips("g.V().not(__.out('created'))");
        assertRoundTrips("g.V().where(__.out('knows'))");
        assertRoundTrips("g.V().has('age', P.gt(20).and(P.lt(40)))");
    }

    @Test @DisplayName("projection + aggregation + branching")
    void projectionAggregationBranching() {
        assertRoundTrips("g.V().count()");
        assertRoundTrips("g.V().limit(5)");
        assertRoundTrips("g.V().order().by('name')");
        assertRoundTrips("g.V().select('a','b')");
        assertRoundTrips("g.V().union(__.out(), __.in())");
        assertRoundTrips("g.V().coalesce(__.out('knows'), __.out('created'))");
        assertRoundTrips("g.V().repeat(__.out()).times(2)");
        assertRoundTrips("g.V().choose(P.gt(2), __.out(), __.in())");
        assertRoundTrips("g.V().dedup()");
        assertRoundTrips("g.V().fold()");
    }

    @Test @DisplayName("mutation steps")
    void mutation() {
        assertRoundTrips("g.addV('person')");
        assertRoundTrips("g.V().addE('knows')");
        assertRoundTrips("g.V().property('name','marko')");
    }
}
