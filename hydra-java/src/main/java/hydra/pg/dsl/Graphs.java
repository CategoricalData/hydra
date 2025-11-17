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
    /**
     * Creates an edge builder from an edge type and ids.
     *
     * @param <T> the type representation used in the edge type
     * @param <V> the value type for the edge id and vertex ids
     * @param type the edge type
     * @param id the edge id
     * @param outId the id of the outgoing vertex
     * @param inId the id of the incoming vertex
     * @return an edge builder
     */
    static <T, V> EdgeBuilder<V> edge(EdgeType<T> type, V id, V outId, V inId) {
        return new EdgeBuilder<V>(type.label, id, outId, inId);
    }

    /**
     * Creates an edge builder from a label string and ids.
     *
     * @param <V> the value type for the edge id and vertex ids
     * @param label the edge label
     * @param id the edge id
     * @param outId the id of the outgoing vertex
     * @param inId the id of the incoming vertex
     * @return an edge builder
     */
    static <V> EdgeBuilder<V> edge(String label, V id, V outId, V inId) {
        return new EdgeBuilder<V>(new EdgeLabel(label), id, outId, inId);
    }

    /**
     * Creates an edge type builder.
     *
     * @param <T> the type representation used for the id type
     * @param label the edge label
     * @param idType the type of the edge id
     * @param outLabel the label of the outgoing vertex type
     * @param inLabel the label of the incoming vertex type
     * @return an edge type builder
     */
    static <T> EdgeTypeBuilder<T> edgeType(String label, T idType, String outLabel, String inLabel) {
        return new EdgeTypeBuilder<T>(
                new EdgeLabel(label), idType, new VertexLabel(outLabel), new VertexLabel(inLabel));
    }

    /**
     * Creates a vertex builder from a vertex type and id.
     *
     * @param <T> the type representation used in the vertex type
     * @param <V> the value type for the vertex id
     * @param type the vertex type
     * @param id the vertex id
     * @return a vertex builder
     */
    static <T, V> VertexBuilder<V> vertex(VertexType<T> type, V id) {
        return new VertexBuilder<V>(type.label, id);
    }

    /**
     * Creates a vertex builder from a label string and id.
     *
     * @param <V> the value type for the vertex id
     * @param label the vertex label
     * @param id the vertex id
     * @return a vertex builder
     */
    static <V> VertexBuilder<V> vertex(String label, V id) {
        return new VertexBuilder<V>(new VertexLabel(label), id);
    }

    /**
     * Creates a vertex type builder.
     *
     * @param <T> the type representation used for the id type
     * @param label the vertex label
     * @param idType the type of the vertex id
     * @return a vertex type builder
     */
    static <T> VertexTypeBuilder<T> vertexType(String label, T idType) {
        return new VertexTypeBuilder<T>(new VertexLabel(label), idType);
    }
}
