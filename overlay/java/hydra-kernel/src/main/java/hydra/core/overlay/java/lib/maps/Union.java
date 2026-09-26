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
 * Computes the union of two sets.
 */
public class Union extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return hydra.core.lib.Maps.union().name;
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE,
                function(map("k", "v"), map("k", "v"), map("k", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Model.map(t -> Either.right(t), t -> Either.right(t), graph, args.get(0)), mp1 ->
            hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Model.map(t -> Either.right(t), t -> Either.right(t), graph, args.get(1)), mp2 ->
                Either.right(Terms.map(apply(mp1, mp2)))));
    }

    /**
     * Combines two maps.
     * @param <K> the key type
     * @param <V> the value type
     * @param mp1 the first map
     * @return a function that takes a second map and returns the union
     */
    public static <K, V> Function<Map<K, V>, Map<K, V>> apply(Map<K, V> mp1) {
        return mp2 -> apply(mp1, mp2);
    }

    /**
     * Combines two maps.
     * @param <K> the key type
     * @param <V> the value type
     * @param mp1 the first map (takes precedence)
     * @param mp2 the second map
     * @return the union of the two maps
     */
    public static <K, V> Map<K, V> apply(Map<K, V> mp1, Map<K, V> mp2) {
        return PersistentMap.<K, V>coerce(mp1).union(PersistentMap.<K, V>coerce(mp2));
    }
}
