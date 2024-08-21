package hydra.ext.org.apache.tinkerpop.dsl;

import hydra.pg.model.EdgeLabel;
import hydra.pg.model.EdgeType;
import hydra.pg.model.VertexLabel;

/**
 * A builder object for property graph edge types.
 */
public class EdgeTypeBuilder<T> extends ElementTypeBuilder<T, EdgeType<T>, EdgeTypeBuilder<T>> {
    private final EdgeLabel label;
    private final T id;
    private final VertexLabel outLabel;
    private final VertexLabel inLabel;

    /**
     * Construct the builder object.
     */
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
