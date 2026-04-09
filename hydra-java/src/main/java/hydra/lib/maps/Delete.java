package hydra.lib.maps;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.TreeMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Removes a key from the map.
 */
public class Delete extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.delete");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> {
            Term key = args.get(0);
            return hydra.lib.eithers.Map.apply(before -> Terms.map(apply(key, before)), hydra.extract.Core.map(t -> Either.right(t), t -> Either.right(t), graph, args.get(1)));
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
        Map<K, V> result = new TreeMap<>(before);
        result.remove(k);
        return result;
    }
}
