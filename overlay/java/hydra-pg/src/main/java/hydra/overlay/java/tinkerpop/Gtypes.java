package hydra.overlay.java.tinkerpop;

/**
 * Maps Hydra {@code TraversalGType} (the Gremlin grammar's type tokens, used by {@code asNumber} and the
 * {@code typeOf} predicate) to TinkerPop's {@code org.apache.tinkerpop.gremlin.process.traversal.GType}
 * enum.
 *
 * <p>Note: the Gremlin grammar (and thus the Hydra model) carries unsigned type tokens ({@code …U}) and
 * {@code uuidL}, but TinkerPop's runtime {@code GType} enum has only the 27 base types. The unsigned /
 * extended variants therefore have no {@code GType} counterpart and throw {@link UnsupportedOperationException}.
 */
final class Gtypes {

    private Gtypes() {
    }

    static Object toGremlin(hydra.tinkerpop.gremlin.TraversalGType g) {
        return g.accept(new hydra.tinkerpop.gremlin.TraversalGType.Visitor<Object>() {
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.BigDecimal x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.BIGDECIMAL; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.BigDecimalU x) { throw unsupported("BigDecimalU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.BigInt x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.BIGINT; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.BigIntU x) { throw unsupported("BigIntU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Binary x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.BINARY; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.BinaryU x) { throw unsupported("BinaryU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Boolean_ x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.BOOLEAN; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.BooleanU x) { throw unsupported("BooleanU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Byte_ x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.BYTE; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.ByteU x) { throw unsupported("ByteU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Char x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.CHAR; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.CharU x) { throw unsupported("CharU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.DateTime x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.DATETIME; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.DateTimeU x) { throw unsupported("DateTimeU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Double_ x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.DOUBLE; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.DoubleU x) { throw unsupported("DoubleU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Duration x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.DURATION; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.DurationU x) { throw unsupported("DurationU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Edge x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.EDGE; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.EdgeU x) { throw unsupported("EdgeU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Float_ x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.FLOAT; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.FloatU x) { throw unsupported("FloatU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Graph x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.GRAPH; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.GraphU x) { throw unsupported("GraphU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Int x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.INT; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.IntU x) { throw unsupported("IntU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.List x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.LIST; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.ListU x) { throw unsupported("ListU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Long_ x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.LONG; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.LongU x) { throw unsupported("LongU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Map x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.MAP; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.MapU x) { throw unsupported("MapU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Null x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.NULL; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.NullU x) { throw unsupported("NullU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Number_ x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.NUMBER; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.NumberU x) { throw unsupported("NumberU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Path x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.PATH; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.PathU x) { throw unsupported("PathU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Property x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.PROPERTY; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.PropertyU x) { throw unsupported("PropertyU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Set x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.SET; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.SetU x) { throw unsupported("SetU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Short_ x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.SHORT; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.ShortU x) { throw unsupported("ShortU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.String_ x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.STRING; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.StringU x) { throw unsupported("StringU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Tree x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.TREE; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.TreeU x) { throw unsupported("TreeU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Uuid x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.UUID; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.UuidL x) { throw unsupported("UuidL"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Vertex x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.VERTEX; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.VertexU x) { throw unsupported("VertexU"); }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.Vproperty x) { return org.apache.tinkerpop.gremlin.process.traversal.GType.VPROPERTY; }
            @Override
            public Object visit(hydra.tinkerpop.gremlin.TraversalGType.VpropertyU x) { throw unsupported("VpropertyU"); }
        });
    }


    /** Reverse: TinkerPop {@code GType} → Hydra {@code TraversalGType} (base 27 types). */
    static hydra.tinkerpop.gremlin.TraversalGType fromGremlin(org.apache.tinkerpop.gremlin.process.traversal.GType g) {
        switch (g) {
            case BIGDECIMAL: return new hydra.tinkerpop.gremlin.TraversalGType.BigDecimal();
            case BIGINT: return new hydra.tinkerpop.gremlin.TraversalGType.BigInt();
            case BINARY: return new hydra.tinkerpop.gremlin.TraversalGType.Binary();
            case BOOLEAN: return new hydra.tinkerpop.gremlin.TraversalGType.Boolean_();
            case BYTE: return new hydra.tinkerpop.gremlin.TraversalGType.Byte_();
            case CHAR: return new hydra.tinkerpop.gremlin.TraversalGType.Char();
            case DATETIME: return new hydra.tinkerpop.gremlin.TraversalGType.DateTime();
            case DOUBLE: return new hydra.tinkerpop.gremlin.TraversalGType.Double_();
            case DURATION: return new hydra.tinkerpop.gremlin.TraversalGType.Duration();
            case EDGE: return new hydra.tinkerpop.gremlin.TraversalGType.Edge();
            case FLOAT: return new hydra.tinkerpop.gremlin.TraversalGType.Float_();
            case GRAPH: return new hydra.tinkerpop.gremlin.TraversalGType.Graph();
            case INT: return new hydra.tinkerpop.gremlin.TraversalGType.Int();
            case LIST: return new hydra.tinkerpop.gremlin.TraversalGType.List();
            case LONG: return new hydra.tinkerpop.gremlin.TraversalGType.Long_();
            case MAP: return new hydra.tinkerpop.gremlin.TraversalGType.Map();
            case NULL: return new hydra.tinkerpop.gremlin.TraversalGType.Null();
            case NUMBER: return new hydra.tinkerpop.gremlin.TraversalGType.Number_();
            case PATH: return new hydra.tinkerpop.gremlin.TraversalGType.Path();
            case PROPERTY: return new hydra.tinkerpop.gremlin.TraversalGType.Property();
            case SET: return new hydra.tinkerpop.gremlin.TraversalGType.Set();
            case SHORT: return new hydra.tinkerpop.gremlin.TraversalGType.Short_();
            case STRING: return new hydra.tinkerpop.gremlin.TraversalGType.String_();
            case TREE: return new hydra.tinkerpop.gremlin.TraversalGType.Tree();
            case UUID: return new hydra.tinkerpop.gremlin.TraversalGType.Uuid();
            case VERTEX: return new hydra.tinkerpop.gremlin.TraversalGType.Vertex();
            case VPROPERTY: return new hydra.tinkerpop.gremlin.TraversalGType.Vproperty();
            default: throw new UnsupportedOperationException("Unknown GType: " + g);
        }
    }

    private static UnsupportedOperationException unsupported(String variant) {
        return new UnsupportedOperationException(
                "TraversalGType." + variant + " has no TinkerPop GType counterpart (unsigned/extended type token)");
    }
}
