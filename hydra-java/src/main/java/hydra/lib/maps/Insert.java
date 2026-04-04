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
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Adds an element to a set.
 */
public class Insert extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.insert");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> {
            Term key = args.get(0);
            Term value = args.get(1);
            return hydra.lib.eithers.Map.apply(before -> Terms.map(apply(key, value, before)), hydra.extract.Core.map(cx, t -> Either.right(t), t -> Either.right(t), graph, args.get(2)));
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
        Map<K, V> result = new TreeMap<>(before);
        result.put(k, v);
        return result;
    }
}
