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
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


/**
 * Maps a flow function over map keys.
 */
public class MapKeys extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.mapKeys");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained3("k1", Types.ORD, "k2", Types.ORD, "v", Types.NONE,
            function(function("k1", "k2"), map("k1", "v"), map("k2", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph ->
            hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.map(cx, t -> Either.right(t), t -> Either.right(t), graph, args.get(1)), mp -> {
                java.util.LinkedHashMap<Term, Term> result = new java.util.LinkedHashMap<>();
                for (java.util.Map.Entry<Term, Term> e : mp.entrySet()) {
                    Either<InContext<OtherError>, Term> r = hydra.reduction.Reduction.reduceTerm(
                        new hydra.context.Context(java.util.List.of(), java.util.List.of(), java.util.Map.of()), graph, true, Terms.apply(args.get(0), e.getKey()));
                    if (r.isLeft()) return (Either) r;
                    result.put(((Either.Right<InContext<OtherError>, Term>) r).value, e.getValue());
                }
                return Either.right(Terms.map(FromList.orderedMap(result)));
            });
    }

    /**
     * Transforms map keys with flow.
     * @param <K1> the input key type
     * @param <K2> the output key type
     * @param <V> the value type
     * @param mapping the function to apply to each key
     * @return a function that takes a map and returns the map with transformed keys
     */
    public static <K1, K2, V> Function<java.util.Map<K1, V>, java.util.Map<K2, V>> apply(Function<K1, K2> mapping) {
        return (arg) -> apply(mapping, arg);
    }

    /**
     * Apply the function to both arguments.
     * @param <K1> the input key type
     * @param <K2> the output key type
     * @param <V> the value type
     * @param mapping the function to apply to each key
     * @param arg the input map
     * @return the map with transformed keys
     */
    public static <K1, K2, V> java.util.Map<K2, V> apply(Function<K1, K2> mapping, java.util.Map<K1, V> arg) {
        // Build into a LinkedHashMap first since we don't know the output key type's ordering
        java.util.Map<K2, V> result = new java.util.LinkedHashMap<>();
        for (java.util.Map.Entry<K1, V> e : arg.entrySet()) {
            result.put(mapping.apply(e.getKey()), e.getValue());
        }
        return FromList.orderedMap(result);
    }
}
