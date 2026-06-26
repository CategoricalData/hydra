package hydra.tinkerpop;

/** Maps Hydra {@code TraversalOperator} to TinkerPop's {@code Operator} enum (used by sack/fold bi-functions). */
final class Operators {

    private Operators() {
    }

    /** Reverse: TinkerPop {@code Operator} → Hydra {@code TraversalOperator}. */
    static hydra.tinkerpop.gremlin.TraversalOperator fromGremlin(org.apache.tinkerpop.gremlin.process.traversal.Operator o) {
        switch (o) {
            case addAll:  return new hydra.tinkerpop.gremlin.TraversalOperator.AddAll();
            case and:     return new hydra.tinkerpop.gremlin.TraversalOperator.And();
            case assign:  return new hydra.tinkerpop.gremlin.TraversalOperator.Assign();
            case div:     return new hydra.tinkerpop.gremlin.TraversalOperator.Div();
            case max:     return new hydra.tinkerpop.gremlin.TraversalOperator.Max();
            case min:     return new hydra.tinkerpop.gremlin.TraversalOperator.Min();
            case minus:   return new hydra.tinkerpop.gremlin.TraversalOperator.Minus();
            case mult:    return new hydra.tinkerpop.gremlin.TraversalOperator.Mult();
            case or:      return new hydra.tinkerpop.gremlin.TraversalOperator.Or();
            case sum:     return new hydra.tinkerpop.gremlin.TraversalOperator.Sum();
            case sumLong: return new hydra.tinkerpop.gremlin.TraversalOperator.SumLong();
            default: throw new IllegalArgumentException("Unknown Operator: " + o);
        }
    }

    static org.apache.tinkerpop.gremlin.process.traversal.Operator toGremlin(hydra.tinkerpop.gremlin.TraversalOperator o) {
        return o.accept(new hydra.tinkerpop.gremlin.TraversalOperator.Visitor<org.apache.tinkerpop.gremlin.process.traversal.Operator>() {
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Operator visit(hydra.tinkerpop.gremlin.TraversalOperator.AddAll x) { return org.apache.tinkerpop.gremlin.process.traversal.Operator.addAll; }
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Operator visit(hydra.tinkerpop.gremlin.TraversalOperator.And x) { return org.apache.tinkerpop.gremlin.process.traversal.Operator.and; }
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Operator visit(hydra.tinkerpop.gremlin.TraversalOperator.Assign x) { return org.apache.tinkerpop.gremlin.process.traversal.Operator.assign; }
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Operator visit(hydra.tinkerpop.gremlin.TraversalOperator.Div x) { return org.apache.tinkerpop.gremlin.process.traversal.Operator.div; }
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Operator visit(hydra.tinkerpop.gremlin.TraversalOperator.Max x) { return org.apache.tinkerpop.gremlin.process.traversal.Operator.max; }
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Operator visit(hydra.tinkerpop.gremlin.TraversalOperator.Min x) { return org.apache.tinkerpop.gremlin.process.traversal.Operator.min; }
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Operator visit(hydra.tinkerpop.gremlin.TraversalOperator.Minus x) { return org.apache.tinkerpop.gremlin.process.traversal.Operator.minus; }
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Operator visit(hydra.tinkerpop.gremlin.TraversalOperator.Mult x) { return org.apache.tinkerpop.gremlin.process.traversal.Operator.mult; }
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Operator visit(hydra.tinkerpop.gremlin.TraversalOperator.Or x) { return org.apache.tinkerpop.gremlin.process.traversal.Operator.or; }
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Operator visit(hydra.tinkerpop.gremlin.TraversalOperator.Sum x) { return org.apache.tinkerpop.gremlin.process.traversal.Operator.sum; }
            @Override public org.apache.tinkerpop.gremlin.process.traversal.Operator visit(hydra.tinkerpop.gremlin.TraversalOperator.SumLong x) { return org.apache.tinkerpop.gremlin.process.traversal.Operator.sumLong; }
        });
    }
}
