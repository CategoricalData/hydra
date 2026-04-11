package hydra.pg.dsl;

import hydra.pg.model.Vertex;
import hydra.pg.model.VertexLabel;

/**
 * A builder object for property graph vertices.
 * @param <V> the vertex identifier type
 */
public class VertexBuilder<V> extends ElementBuilder<V, Vertex<V>, VertexBuilder<V>> {
    private final VertexLabel label;
    private final V id;

    /**
     * Construct the builder object.
     * @param label the vertex label
     * @param id the vertex identifier
     */
    public VertexBuilder(VertexLabel label, V id) {
        this.label = label;
        this.id = id;
    }

    @Override
    protected VertexBuilder<V> getThis() {
        return this;
    }

    @Override
    public Vertex<V> build() {
        return new Vertex<V>(label, id, properties);
    }
}
