package hydra.overlay.java.lib.maps;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.map;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;
import hydra.overlay.java.util.PersistentMap;

/**
 * Insert a key-value pair into a map.
 */
public class Insert extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return hydra.lib.Maps.insert().name;
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE,
                function(Types.var("k"), Types.var("v"), map("k", "v"), map("k", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            Term key = args.get(0);
            Term value = args.get(1);
            return hydra.overlay.java.lib.eithers.Map.apply(before -> Terms.map(apply(key, value, before)), hydra.extract.Core.map(t -> Either.right(t), t -> Either.right(t), graph, args.get(2)));
        };
    }

    /**
     * Adds an element.
     * @param <K> the key type
     * @param <V> the value type
     * @param k the key to insert
     * @return a curried function that takes a value, a map, and returns the updated map
     */
    public static <K, V> Function<V, Function<Map<K, V>, Map<K, V>>> apply(K k) {
        return v -> apply(k, v);
    }

    /**
     * Adds an element.
     * @param <K> the key type
     * @param <V> the value type
     * @param k the key to insert
     * @param v the value to insert
     * @return a function that takes a map and returns the updated map
     */
    public static <K, V> Function<Map<K, V>, Map<K, V>> apply(K k, V v) {
        return before -> apply(k, v, before);
    }

    /**
     * Apply the function to all three arguments.
     * @param <K> the key type
     * @param <V> the value type
     * @param k the key to insert
     * @param v the value to insert
     * @param before the map to insert into
     * @return the updated map
     */
    public static <K, V> Map<K, V> apply(K k, V v, Map<K, V> before) {
        return PersistentMap.<K, V>coerce(before).insert(k, v);
    }
}
