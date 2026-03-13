package hydra.lib.maps;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


/**
 * Looks up with a default value.
 */
public class FindWithDefault extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.findWithDefault");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("v", Types.NONE, "k", Types.ORD,
                function("v", function("k", function(hydra.dsl.Types.map("k", "v"), "v"))));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(
                (Function<Map<Term, Term>, Term>) mp -> {
                    Maybe<Term> result = Lookup.apply(args.get(1), mp);
                    return result.orElse(args.get(0));
                },
                hydra.extract.core.Core.map(cx, t -> Either.right(t), t -> Either.right(t), graph, args.get(2)));
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
        return mp.getOrDefault(key, defaultValue);
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
