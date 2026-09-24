package hydra.core.overlay.java.lib.maps;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.map;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;
import hydra.core.overlay.java.util.PersistentMap;


/**
 * Creates a singleton list.
 */
public class Singleton extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return hydra.lib.Maps.singleton().name;
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE,
                function(Types.var("k"), Types.var("v"), map("k", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.right(Terms.map(PersistentMap.singleton(args.get(0), args.get(1))));
    }

    /**
     * Creates a map with one element.
     * @param <K> the key type
     * @param <V> the value type
     * @param key the key
     * @return a function that takes a value and returns the singleton map
     */
    public static <K, V> Function<V, Map<K, V>> apply(K key) {
        return value -> apply(key, value);
    }

    /**
     * Creates a map with one element.
     * @param <K> the key type
     * @param <V> the value type
     * @param key the key
     * @param value the value
     * @return the singleton map
     */
    public static <K, V> Map<K, V> apply(K key, V value) {
        return PersistentMap.singleton(key, value);
    }
}
