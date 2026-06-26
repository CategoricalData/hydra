package hydra.tinkerpop;

import org.apache.tinkerpop.gremlin.structure.Direction;

/**
 * Maps Hydra {@code TraversalDirection} to TinkerPop's {@link Direction}. The grammar's {@code from}/
 * {@code to} are aliases for {@code out}/{@code in} respectively (3.8.x).
 */
final class Directions {

    private Directions() {
    }

    static Direction toGremlin(hydra.tinkerpop.gremlin.TraversalDirection d) {
        return d.accept(new hydra.tinkerpop.gremlin.TraversalDirection.Visitor<Direction>() {
            @Override public Direction visit(hydra.tinkerpop.gremlin.TraversalDirection.Out x)  { return Direction.OUT; }
            @Override public Direction visit(hydra.tinkerpop.gremlin.TraversalDirection.In x)   { return Direction.IN; }
            @Override public Direction visit(hydra.tinkerpop.gremlin.TraversalDirection.Both x) { return Direction.BOTH; }
            @Override public Direction visit(hydra.tinkerpop.gremlin.TraversalDirection.From x) { return Direction.OUT; }
            @Override public Direction visit(hydra.tinkerpop.gremlin.TraversalDirection.To x)   { return Direction.IN; }
        });
    }

    /** Reverse: TinkerPop {@link Direction} → Hydra {@code TraversalDirection} (canonical out/in/both). */
    static hydra.tinkerpop.gremlin.TraversalDirection fromGremlin(Direction d) {
        switch (d) {
            case OUT:  return new hydra.tinkerpop.gremlin.TraversalDirection.Out();
            case IN:   return new hydra.tinkerpop.gremlin.TraversalDirection.In();
            case BOTH: return new hydra.tinkerpop.gremlin.TraversalDirection.Both();
            default:   throw new IllegalArgumentException("Unknown Direction: " + d);
        }
    }
}
