package hydra.ext.org.apache.tinkerpop.dsl;

import hydra.ext.org.apache.tinkerpop.propertyGraph.Edge;
import hydra.ext.org.apache.tinkerpop.propertyGraph.EdgeLabel;
import hydra.ext.org.apache.tinkerpop.propertyGraph.EdgeType;

/**
 * A builder object for property graph edges.
 */
public class EdgeBuilder<V> extends ElementBuilder<V, Edge<V>, EdgeBuilder<V>> {
    private final EdgeLabel label;
    private final V id;
    private final V outId;
    private final V inId;

    /**
     * Construct the builder object.
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
