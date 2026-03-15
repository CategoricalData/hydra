package hydra.lib.maps;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import hydra.util.PersistentMap;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


/**
 * Checks if an element is in a set.
 */
public class Member extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.member");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE,
                function(Types.var("k"), hydra.dsl.Types.map("k", "v"), boolean_()));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(
                (Function<PersistentMap<Term, Term>, Term>) mp -> Terms.boolean_(mp.containsKey(args.get(0))),
                hydra.extract.core.Core.map(cx, t -> Either.right(t), t -> Either.right(t), graph, args.get(1)));
    }

    /**
     * Checks if the element is present.
     * @param <K> the key type
     * @param <V> the value type
     * @param key the key to check
     * @return a function that takes a map and returns true if the key is present
     */
    public static <K, V> Function<PersistentMap<K, V>, Boolean> apply(K key) {
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
    public static <K, V> Boolean apply(K key, PersistentMap<K, V> mp) {
        return mp.containsKey(key);
    }
}
