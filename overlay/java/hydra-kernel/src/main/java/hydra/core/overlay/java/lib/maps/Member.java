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

import static hydra.core.overlay.java.dsl.Types.boolean_;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Checks if an element is in a set.
 */
public class Member extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return hydra.core.lib.Maps.member().name;
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE,
                function(Types.var("k"), hydra.core.overlay.java.dsl.Types.map("k", "v"), boolean_()));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(
                (Function<Map<Term, Term>, Term>) mp -> Terms.boolean_(mp.containsKey(args.get(0))),
                hydra.core.extract.Model.map(t -> Either.right(t), t -> Either.right(t), graph, args.get(1)));
    }

    /**
     * Checks if the element is present.
     * @param <K> the key type
     * @param <V> the value type
     * @param key the key to check
     * @return a function that takes a map and returns true if the key is present
     */
    public static <K, V> Function<Map<K, V>, Boolean> apply(K key) {
        return mp -> apply(key, mp);
    }

    /**
     * Checks if the element is present.
     * @param <K> the key type
     * @param <V> the value type
     * @param key the key to check
     * @param mp the map to search
     * @return true if present, false otherwise
     */
    public static <K, V> Boolean apply(K key, Map<K, V> mp) {
        return mp.containsKey(key);
    }
}
