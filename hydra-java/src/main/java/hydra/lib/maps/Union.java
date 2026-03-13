package hydra.lib.maps;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


/**
 * Computes the union of two sets.
 */
public class Union extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.union");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.map(cx, t -> Either.right(t), t -> Either.right(t), graph, args.get(0)), mp1 ->
            hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.map(cx, t -> Either.right(t), t -> Either.right(t), graph, args.get(1)), mp2 -> {
                // Left-biased union: mp1 values take precedence (matching Haskell's Data.Map.union)
                Map<Term, Term> result = new HashMap<>(mp2);
                result.putAll(mp1);
                return Either.right(Terms.map(result));
            }));
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
     * @param mp1 the first map
     * @param mp2 the second map
     * @return the union of the two maps
     */
    public static <K, V> Map<K, V> apply(Map<K, V> mp1, Map<K, V> mp2) {
        // Left-biased union: mp1 values take precedence (matching Haskell's Data.Map.union)
        Map<K, V> result = FromList.orderedMap(mp2);
        result.putAll(mp1);
        return result;
    }
}
