package hydra.core.overlay.java.lib.maps;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.Optional;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.map;
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Looks up a value by key.
 */
public class Lookup extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return hydra.core.lib.Maps.lookup().name;
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE,
                function(Types.var("k"), map("k", "v"), optional("v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<Map<Term, Term>, Term>) mp -> Terms.optional(apply(args.get(0), mp)), hydra.core.extract.Model.map(t -> Either.right(t), t -> Either.right(t), graph, args.get(1)));
    }

    /**
     * Retrieves the value for the key.
     * @param <K> the key type
     * @param <V> the value type
     * @param k the key to look up
     * @return a function that takes a map and returns an optional value
     */
    public static <K, V> Function<Map<K, V>, Optional<V>> apply(K k) {
        return mp -> apply(k, mp);
    }

    /**
     * Retrieves the value for the key.
     * @param <K> the key type
     * @param <V> the value type
     * @param k the key to look up
     * @param mp the map to search
     * @return an optional containing the value if found, or empty if not found
     */
    public static <K, V> Optional<V> apply(K k, Map<K, V> mp) {
        return mp.containsKey(k) ? Optional.given(mp.get(k)) : Optional.none();
    }
}
