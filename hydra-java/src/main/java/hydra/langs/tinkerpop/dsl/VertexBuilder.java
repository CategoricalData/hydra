package hydra.langs.tinkerpop.dsl;

import hydra.langs.tinkerpop.propertyGraph.Vertex;
import hydra.langs.tinkerpop.propertyGraph.VertexLabel;

public class VertexBuilder<V> extends ElementBuilder<V, Vertex<V>, VertexBuilder<V>> {
    private final VertexLabel label;
    private final V id;

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
