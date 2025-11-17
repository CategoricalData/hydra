package hydra.pg.dsl;

import hydra.pg.model.EdgeLabel;
import hydra.pg.model.EdgeType;
import hydra.pg.model.VertexLabel;

/**
 * A builder object for property graph edge types.
 * @param <T> the type identifier type
 */
public class EdgeTypeBuilder<T> extends ElementTypeBuilder<T, EdgeType<T>, EdgeTypeBuilder<T>> {
    private final EdgeLabel label;
    private final T id;
    private final VertexLabel outLabel;
    private final VertexLabel inLabel;

    /**
     * Construct the builder object.
     * @param label the edge label
     * @param id the type identifier
     * @param outLabel the outgoing vertex label
     * @param inLabel the incoming vertex label
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
