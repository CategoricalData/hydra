package hydra.langs.tinkerpop.dsl;

import hydra.langs.tinkerpop.propertyGraph.EdgeLabel;
import hydra.langs.tinkerpop.propertyGraph.EdgeType;
import hydra.langs.tinkerpop.propertyGraph.VertexLabel;

public class EdgeTypeBuilder<T> extends ElementTypeBuilder<T, EdgeType<T>, EdgeTypeBuilder<T>> {
    private final EdgeLabel label;
    private final T id;
    private final VertexLabel outLabel;
    private final VertexLabel inLabel;

    public EdgeTypeBuilder(EdgeLabel label, T id, VertexLabel outLabel, VertexLabel inLabel) {
        this.label = label;
        this.id = id;
        this.outLabel = outLabel;
        this.inLabel = inLabel;
    }

    @Override
    protected EdgeTypeBuilder<T> getThis() {
        return this;
    }

    @Override
    public EdgeType<T> build() {
        return new EdgeType<T>(label, id, outLabel, inLabel, properties);
    }
}
