package hydra.overlay.java.tinkerpop;

import org.apache.tinkerpop.gremlin.process.traversal.DT;

/** Maps Hydra {@code TraversalDT} (date-part unit) to TinkerPop's {@link DT} enum, used by {@code dateAdd}. */
final class Dts {

    private Dts() {
    }

    static DT toGremlin(hydra.tinkerpop.gremlin.TraversalDT d) {
        return d.accept(new hydra.tinkerpop.gremlin.TraversalDT.Visitor<DT>() {
            @Override public DT visit(hydra.tinkerpop.gremlin.TraversalDT.Second x) { return DT.second; }
            @Override public DT visit(hydra.tinkerpop.gremlin.TraversalDT.Minute x) { return DT.minute; }
            @Override public DT visit(hydra.tinkerpop.gremlin.TraversalDT.Hour x)   { return DT.hour; }
            @Override public DT visit(hydra.tinkerpop.gremlin.TraversalDT.Day x)    { return DT.day; }
        });
    }
}
