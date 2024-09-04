package hydra.pg.dsl;

import hydra.pg.model.EdgeLabel;
import hydra.pg.model.EdgeType;
import hydra.pg.model.VertexLabel;
import hydra.pg.model.VertexType;

/**
 * DSL for constructing property graph types (vertex and edge types, property types)
 * and values (vertices, edges, and properties).
 */
public interface Graphs {
    static <T, V> EdgeBuilder<V> edge(EdgeType<T> type, V id, V outId, V inId) {
        return new EdgeBuilder<V>(type.label, id, outId, inId);
    }

    static <V> EdgeBuilder<V> edge(String label, V id, V outId, V inId) {
        return new EdgeBuilder<V>(new EdgeLabel(label), id, outId, inId);
    }

    static <T> EdgeTypeBuilder<T> edgeType(String label, T idType, String outLabel, String inLabel) {
        return new EdgeTypeBuilder<T>(
                new EdgeLabel(label), idType, new VertexLabel(outLabel), new VertexLabel(inLabel));
    }

    static <T, V> VertexBuilder<V> vertex(VertexType<T> type, V id) {
        return new VertexBuilder<V>(type.label, id);
    }

    static <V> VertexBuilder<V> vertex(String label, V id) {
        return new VertexBuilder<V>(new VertexLabel(label), id);
    }

    static <T> VertexTypeBuilder<T> vertexType(String label, T idType) {
        return new VertexTypeBuilder<T>(new VertexLabel(label), idType);
    }
}
