package hydra.langs.tinkerpop.dsl;

import hydra.langs.tinkerpop.propertyGraph.Edge;
import hydra.langs.tinkerpop.propertyGraph.EdgeLabel;
import hydra.langs.tinkerpop.propertyGraph.EdgeType;

public class EdgeBuilder<V> extends ElementBuilder<V, Edge<V>, EdgeBuilder<V>> {
    private final EdgeLabel label;
    private final V id;
    private final V outId;
    private final V inId;

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
