package hydra.langs.tinkerpop.dsl;

import hydra.langs.tinkerpop.propertyGraph.VertexLabel;
import hydra.langs.tinkerpop.propertyGraph.VertexType;

/**
 * A builder object for property graph vertex types.
 */
public class VertexTypeBuilder<T> extends ElementTypeBuilder<T, VertexType<T>, VertexTypeBuilder<T>> {
    private final VertexLabel label;
    private final T id;

    /**
     * Construct the builder object.
     */
    public VertexTypeBuilder(VertexLabel label, T id) {
        this.label = label;
        this.id = id;
    }

    @Override
    protected VertexTypeBuilder<T> getThis() {
        return this;
    }

    @Override
    public VertexType<T> build() {
        return new VertexType<T>(label, id, properties);
    }
}
