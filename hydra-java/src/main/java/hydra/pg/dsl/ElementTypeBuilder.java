package hydra.pg.dsl;

import hydra.pg.model.PropertyKey;
import hydra.pg.model.PropertyType;

import java.util.ArrayList;
import java.util.List;

/**
 * Abstract base class for building property graph element types (vertex types or edge types) with property types.
 *
 * @param <T> the type representation used for property types
 * @param <S> the type of the element type being built
 * @param <B> the concrete builder type (for fluent API)
 */
public abstract class ElementTypeBuilder<T, S, B extends ElementTypeBuilder<T, S, B>> {
    protected final List<PropertyType<T>> properties = new ArrayList<>();

    /**
     * Returns the concrete builder instance for fluent method chaining.
     *
     * @return this builder instance
     */
    protected abstract B getThis();

    /**
     * Builds and returns the element type with all configured property types.
     *
     * @return the constructed element type
     */
    public abstract S build();

    /**
     * Adds a property type to the element type being built.
     *
     * @param key the property key
     * @param type the type of the property
     * @param required whether the property is required
     * @return this builder instance for fluent chaining
     */
    public B property(String key, T type, boolean required) {
        properties.add(new PropertyType<T>(new PropertyKey(key), type, required));
        return getThis();
    }
}
