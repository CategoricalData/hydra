package hydra.pg.dsl;

import hydra.pg.model.Edge;
import hydra.pg.model.EdgeLabel;
import hydra.pg.model.EdgeType;

/**
 * A builder object for property graph edges.
 * @param <V> the vertex identifier type
 */
public class EdgeBuilder<V> extends ElementBuilder<V, Edge<V>, EdgeBuilder<V>> {
    private final EdgeLabel label;
    private final V id;
    private final V outId;
    private final V inId;

    /**
     * Construct the builder object.
     * @param label the edge label
     * @param id the edge identifier
     * @param outId the outgoing vertex identifier
     * @param inId the incoming vertex identifier
     */
    public EdgeBuilder(EdgeLabel label, V id, V outId, V inId) {
        this.label = label;
        this.id = id;
        this.outId = outId;
        this.inId = inId;
    }

    @Override
    protected EdgeBuilder<V> getThis() {
        return this;
    }

    @Override
    public Edge<V> build() {
        return new Edge<V>(label, id, outId, inId, properties);
    }
}
