package hydra.core.overlay.java.lib.maps;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.Optional;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Looks up with a default value.
 */
public class FindWithDefault extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return hydra.core.lib.Maps.findWithDefault().name;
    }

    @Override
    protected List<Integer> lazyParams() {
        return List.of(0);
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("v", Types.NONE, "k", Types.ORD,
                function("v", function("k", function(hydra.core.overlay.java.dsl.Types.map("k", "v"), "v"))));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(
                (Function<Map<Term, Term>, Term>) mp -> {
                    Optional<Term> result = Lookup.apply(args.get(1), mp);
                    return result.orElse(args.get(0));
                },
                hydra.core.extract.Model.map(t -> Either.right(t), t -> Either.right(t), graph, args.get(2)));
    }

    /**
     * @deprecated Use {@link #applyLazy(Supplier, Object, Map)} instead. Eager evaluation of the default wastes memory.
     */
    @Deprecated
    public static <K, V> Function<K, Function<Map<K, V>, V>> apply(V defaultValue) {
        return key -> mp -> apply(defaultValue, key, mp);
    }

    /**
     * @deprecated Use {@link #applyLazy(Supplier, Object, Map)} instead. Eager evaluation of the default wastes memory.
     */
    @Deprecated
    public static <K, V> V apply(V defaultValue, K key, Map<K, V> mp) {
        return mp.containsKey(key) ? mp.get(key) : defaultValue;
    }

    /**
     * Lazily looks up a key in a map, returning a default if not found.
     * The default is only evaluated if the key is absent.
     */
    public static <K, V> Function<K, Function<Map<K, V>, V>> applyLazy(Supplier<V> defaultValue) {
        return key -> mp -> applyLazy(defaultValue, key, mp);
    }

    /**
     * Lazily looks up a key in a map, returning a default if not found.
     * The default is only evaluated if the key is absent.
     */
    public static <K, V> V applyLazy(Supplier<V> defaultValue, K key, Map<K, V> mp) {
        return mp.containsKey(key) ? mp.get(key) : defaultValue.get();
    }
}
