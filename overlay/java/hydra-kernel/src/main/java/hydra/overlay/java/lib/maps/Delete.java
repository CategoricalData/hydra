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
 * Removes a key from the map.
 */
public class Delete extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return hydra.lib.Maps.delete().name;
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE,
                function(Types.var("k"), map("k", "v"), map("k", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            Term key = args.get(0);
            return hydra.overlay.java.lib.eithers.Map.apply(before -> Terms.map(apply(key, before)), hydra.extract.Core.map(t -> Either.right(t), t -> Either.right(t), graph, args.get(1)));
        };
    }

    /**
     * Removes the key and its value.
     * @param <K> the key type
     * @param <V> the value type
     * @param k the key to remove
     * @return a function that takes a map and returns the updated map
     */
    public static <K, V> Function<Map<K, V>, Map<K, V>> apply(K k) {
        return before -> apply(k, before);
    }

    /**
     * Apply the function to both arguments.
     * @param <K> the key type
     * @param <V> the value type
     * @param k the key to remove
     * @param before the map to remove from
     * @return the updated map
     */
    public static <K, V> Map<K, V> apply(K k, Map<K, V> before) {
        return PersistentMap.<K, V>coerce(before).delete(k);
    }
}
