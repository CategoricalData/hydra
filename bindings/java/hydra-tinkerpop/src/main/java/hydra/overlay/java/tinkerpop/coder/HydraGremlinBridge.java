package hydra.overlay.java.tinkerpop.coder;

import hydra.Reflect;
import hydra.core.Literal;
import hydra.core.LiteralType;
import hydra.error.pg.InvalidValueError;
import hydra.overlay.java.dsl.LiteralTypes;
import hydra.overlay.java.dsl.Literals;
import hydra.overlay.java.util.Optional;
import hydra.pg.model.Edge;
import hydra.pg.model.EdgeLabel;
import hydra.pg.model.Graph;
import hydra.pg.model.PropertyKey;
import hydra.pg.model.Vertex;
import hydra.pg.model.VertexLabel;

import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.GraphTraversalSource;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.T;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

/**
 * Bidirectional bridge between Hydra property graphs and TinkerPop (Gremlin) graphs.
 */
public class HydraGremlinBridge {

    private HydraGremlinBridge() {
    }

    /**
     * Populates a TinkerPop graph from a Hydra property graph.
     */
    public static <V> void hydraToGremlin(
            Graph<V> hydraGraph,
            org.apache.tinkerpop.gremlin.structure.Graph gremlinGraph,
            Function<V, Object> valueToObject) {

        for (Map.Entry<V, Vertex<V>> entry : hydraGraph.vertices.entrySet()) {
            Vertex<V> hv = entry.getValue();
            Object id = valueToObject.apply(hv.id);
            String label = hv.label.value;

            Object[] keyValues = new Object[4 + hv.properties.size() * 2];
            keyValues[0] = T.id;
            keyValues[1] = id;
            keyValues[2] = T.label;
            keyValues[3] = label;
            int i = 4;
            for (Map.Entry<PropertyKey, V> prop : hv.properties.entrySet()) {
                keyValues[i++] = prop.getKey().value;
                keyValues[i++] = valueToObject.apply(prop.getValue());
            }

            gremlinGraph.addVertex(keyValues);
        }

        for (Map.Entry<V, Edge<V>> entry : hydraGraph.edges.entrySet()) {
            Edge<V> he = entry.getValue();
            Object id = valueToObject.apply(he.id);
            Object outId = valueToObject.apply(he.out);
            Object inId = valueToObject.apply(he.in);
            String label = he.label.value;

            org.apache.tinkerpop.gremlin.structure.Vertex outVertex = findVertex(gremlinGraph, outId);
            org.apache.tinkerpop.gremlin.structure.Vertex inVertex = findVertex(gremlinGraph, inId);

            Object[] keyValues = new Object[2 + he.properties.size() * 2];
            keyValues[0] = T.id;
            keyValues[1] = id;
            int i = 2;
            for (Map.Entry<PropertyKey, V> prop : he.properties.entrySet()) {
                keyValues[i++] = prop.getKey().value;
                keyValues[i++] = valueToObject.apply(prop.getValue());
            }

            outVertex.addEdge(label, inVertex, keyValues);
        }
    }

    /**
     * Reads a TinkerPop graph into a Hydra property graph.
     */
    public static <V> Graph<V> gremlinToHydra(
            org.apache.tinkerpop.gremlin.structure.Graph gremlinGraph,
            Function<Object, V> objectToValue) {

        Map<V, Vertex<V>> vertices = new HashMap<>();
        Iterator<org.apache.tinkerpop.gremlin.structure.Vertex> vertexIter = gremlinGraph.vertices();
        while (vertexIter.hasNext()) {
            org.apache.tinkerpop.gremlin.structure.Vertex gv = vertexIter.next();
            V id = objectToValue.apply(gv.id());
            VertexLabel label = new VertexLabel(gv.label());

            Map<PropertyKey, V> properties = new HashMap<>();
            Iterator<org.apache.tinkerpop.gremlin.structure.VertexProperty<Object>> propIter = gv.properties();
            while (propIter.hasNext()) {
                org.apache.tinkerpop.gremlin.structure.VertexProperty<Object> prop = propIter.next();
                properties.put(new PropertyKey(prop.key()), objectToValue.apply(prop.value()));
            }

            vertices.put(id, new Vertex<>(label, id, properties));
        }

        Map<V, Edge<V>> edges = new HashMap<>();
        Iterator<org.apache.tinkerpop.gremlin.structure.Edge> edgeIter = gremlinGraph.edges();
        while (edgeIter.hasNext()) {
            org.apache.tinkerpop.gremlin.structure.Edge ge = edgeIter.next();
            V id = objectToValue.apply(ge.id());
            EdgeLabel label = new EdgeLabel(ge.label());
            V outId = objectToValue.apply(ge.outVertex().id());
            V inId = objectToValue.apply(ge.inVertex().id());

            Map<PropertyKey, V> properties = new HashMap<>();
            Iterator<org.apache.tinkerpop.gremlin.structure.Property<Object>> propIter = ge.properties();
            while (propIter.hasNext()) {
                org.apache.tinkerpop.gremlin.structure.Property<Object> prop = propIter.next();
                properties.put(new PropertyKey(prop.key()), objectToValue.apply(prop.value()));
            }

            edges.put(id, new Edge<>(label, id, outId, inId, properties));
        }

        return new Graph<>(vertices, edges);
    }

    /**
     * Reads a TinkerPop graph into a Hydra property graph using traversals.
     * Unlike {@link #gremlinToHydra(org.apache.tinkerpop.gremlin.structure.Graph, Function)},
     * this method works with remote graph connections via a GraphTraversalSource.
     */
    @SuppressWarnings("unchecked")
    public static <V> Graph<V> gremlinToHydra(
            GraphTraversalSource g,
            Function<Object, V> objectToValue) {

        Map<V, Vertex<V>> vertices = new HashMap<>();
        List<Map<Object, Object>> vertexMaps = g.V().elementMap().toList();
        for (Map<Object, Object> vm : vertexMaps) {
            V id = objectToValue.apply(vm.get(T.id));
            VertexLabel label = new VertexLabel((String) vm.get(T.label));

            Map<PropertyKey, V> properties = new HashMap<>();
            for (Map.Entry<Object, Object> entry : vm.entrySet()) {
                Object key = entry.getKey();
                if (key.equals(T.id) || key.equals(T.label)) continue;
                properties.put(new PropertyKey((String) key), objectToValue.apply(entry.getValue()));
            }

            vertices.put(id, new Vertex<>(label, id, properties));
        }

        Map<V, Edge<V>> edges = new HashMap<>();
        List<Map<Object, Object>> edgeMaps = g.E().elementMap().toList();
        for (Map<Object, Object> em : edgeMaps) {
            V id = objectToValue.apply(em.get(T.id));
            EdgeLabel label = new EdgeLabel((String) em.get(T.label));

            Map<String, Object> outVertex = (Map<String, Object>) em.get(Direction.OUT);
            Map<String, Object> inVertex = (Map<String, Object>) em.get(Direction.IN);
            V outId = objectToValue.apply(outVertex.get(T.id));
            V inId = objectToValue.apply(inVertex.get(T.id));

            Map<PropertyKey, V> properties = new HashMap<>();
            for (Map.Entry<Object, Object> entry : em.entrySet()) {
                Object key = entry.getKey();
                if (key.equals(T.id) || key.equals(T.label)
                        || key.equals(Direction.OUT) || key.equals(Direction.IN)) continue;
                properties.put(new PropertyKey((String) key), objectToValue.apply(entry.getValue()));
            }

            edges.put(id, new Edge<>(label, id, outId, inId, properties));
        }

        return new Graph<>(vertices, edges);
    }

    /**
     * Converts a plain Java object from TinkerPop to a Hydra Literal.
     * Supports String, Integer, Long, Float, and Double.
     */
    public static Literal objectToLiteral(Object obj) {
        if (obj instanceof String) {
            return Literals.string((String) obj);
        } else if (obj instanceof Integer) {
            return Literals.int32((Integer) obj);
        } else if (obj instanceof Long) {
            return Literals.int32(((Long) obj).intValue());
        } else if (obj instanceof Float) {
            return Literals.float32((Float) obj);
        } else if (obj instanceof Double) {
            return Literals.float64((Double) obj);
        }
        throw new UnsupportedOperationException("Unsupported object type: " + obj.getClass());
    }

    /**
     * Converts a Hydra Literal back to a plain Java object suitable for TinkerPop.
     * Inverse of {@link #objectToLiteral(Object)}: unwraps the literal (and, for integers/floats, the
     * inner {@code IntegerValue}/{@code FloatValue}) to its boxed Java value.
     */
    public static Object literalToObject(Literal lit) {
        return lit.accept(new Literal.Visitor<Object>() {
            public Object visit(Literal.String_ l) { return l.value; }
            public Object visit(Literal.Boolean_ l) { return l.value; }
            public Object visit(Literal.Binary l) { return l.value; }
            public Object visit(Literal.Integer_ l) {
                return l.value.accept(new hydra.core.IntegerValue.Visitor<Object>() {
                    public Object visit(hydra.core.IntegerValue.Bigint x) { return x.value; }
                    public Object visit(hydra.core.IntegerValue.Int8 x) { return x.value; }
                    public Object visit(hydra.core.IntegerValue.Int16 x) { return x.value; }
                    public Object visit(hydra.core.IntegerValue.Int32 x) { return x.value; }
                    public Object visit(hydra.core.IntegerValue.Int64 x) { return x.value; }
                    public Object visit(hydra.core.IntegerValue.Uint8 x) { return x.value; }
                    public Object visit(hydra.core.IntegerValue.Uint16 x) { return x.value; }
                    public Object visit(hydra.core.IntegerValue.Uint32 x) { return x.value; }
                    public Object visit(hydra.core.IntegerValue.Uint64 x) { return x.value; }
                });
            }
            public Object visit(Literal.Float_ l) {
                return l.value.accept(new hydra.core.FloatValue.Visitor<Object>() {
                    public Object visit(hydra.core.FloatValue.Float32 x) { return x.value; }
                    public Object visit(hydra.core.FloatValue.Float64 x) { return x.value; }
                });
            }
            public Object visit(Literal.Decimal l) { return l.value; }
        });
    }

    /**
     * Checks whether a Literal value matches a LiteralType.
     * Returns a function that, given a Literal, returns Optional.none() if the type matches
     * or Optional.given(InvalidValueError) if it does not.
     *
     * <p>This is the {@code checkValue} callback for {@link hydra.validate.Pg#validateGraph}
     * when property values are Literals.
     */
    public static Function<Literal, Optional<InvalidValueError>> checkLiteral(LiteralType type) {
        return value -> {
            LiteralType actual = Reflect.literalType(value);
            if (type.equals(actual)) {
                return Optional.none();
            }
            return Optional.given(new InvalidValueError(
                    LiteralTypes.showLiteralType(type),
                    Literals.showLiteral(value)));
        };
    }

    /**
     * Displays a Literal value as a human-readable string.
     */
    public static String showLiteral(Literal lit) {
        return Literals.showLiteral(lit);
    }

    private static org.apache.tinkerpop.gremlin.structure.Vertex findVertex(
            org.apache.tinkerpop.gremlin.structure.Graph graph, Object id) {
        Iterator<org.apache.tinkerpop.gremlin.structure.Vertex> iter = graph.vertices(id);
        if (!iter.hasNext()) {
            throw new IllegalArgumentException("Vertex not found: " + id);
        }
        return iter.next();
    }
}
