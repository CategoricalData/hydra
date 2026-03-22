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
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph ->
            hydra.lib.eithers.Bind.apply(hydra.extract.Core.map(cx, t -> Either.right(t), t -> Either.right(t), graph, args.get(1)), mp -> {
                java.util.LinkedHashMap<Term, Term> result = new java.util.LinkedHashMap<>();
                for (java.util.Map.Entry<Term, Term> e : mp.entrySet()) {
                    Either<InContext<Error_>, Term> r = hydra.Reduction.reduceTerm(
                        hydra.Lexical.emptyContext(), graph, true, Terms.apply(args.get(0), e.getKey()));
                    if (r.isLeft()) return (Either) r;
                    result.put(((Either.Right<InContext<Error_>, Term>) r).value, e.getValue());
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
    public static <K1, K2, V> Function<PersistentMap<K1, V>, PersistentMap<K2, V>> apply(Function<K1, K2> mapping) {
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
    @SuppressWarnings("unchecked")
    public static <K1, K2, V> PersistentMap<K2, V> apply(Function<K1, K2> mapping, PersistentMap<K1, V> arg) {
        PersistentMap result = PersistentMap.empty();
        for (java.util.Map.Entry<K1, V> e : arg.entrySet()) {
            result = result.insert((Comparable) mapping.apply(e.getKey()), e.getValue());
        }
        return result;
    }
}
