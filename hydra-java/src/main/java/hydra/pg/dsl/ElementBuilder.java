package hydra.pg.dsl;

import hydra.pg.model.PropertyKey;
import hydra.util.PersistentMap;

/**
 * Abstract base class for building property graph elements (vertices or edges) with properties.
 *
 * @param <V> the value type for properties
 * @param <S> the type of the element being built
 * @param <B> the concrete builder type (for fluent API)
 */
public abstract class ElementBuilder<V, S, B extends ElementBuilder<V, S, B>> {
    protected PersistentMap<PropertyKey, V> properties = PersistentMap.empty();

    /**
     * Returns the concrete builder instance for fluent method chaining.
     *
     * @return this builder instance
     */
    protected abstract B getThis();

    /**
     * Builds and returns the element with all configured properties.
     *
     * @return the constructed element
     */
    public abstract S build();

    /**
     * Adds a property to the element being built.
     *
     * @param key the property key
     * @param value the property value
     * @return this builder instance for fluent chaining
     */
    public B property(String key, V value) {
        properties = properties.insert(new PropertyKey(key), value);
        return getThis();
    }
}
