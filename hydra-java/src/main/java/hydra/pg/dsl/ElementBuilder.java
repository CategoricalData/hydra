package hydra.pg.dsl;

import hydra.pg.model.PropertyKey;

import java.util.HashMap;
import java.util.Map;

public abstract class ElementBuilder<V, S, B extends ElementBuilder<V, S, B>> {
    protected final Map<PropertyKey, V> properties = new HashMap<PropertyKey, V>();

    protected abstract B getThis();

    public abstract S build();

    public B property(String key, V value) {
        properties.put(new PropertyKey(key), value);
        return getThis();
    }
}
