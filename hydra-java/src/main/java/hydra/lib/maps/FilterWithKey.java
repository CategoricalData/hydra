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

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Filters map entries by key and value.
 */
public class FilterWithKey extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.filterWithKey");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE,
                function(function(Types.var("k"), Types.var("v"), boolean_()), map("k", "v"), map("k", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.map(cx, t -> Either.right(t), t -> Either.right(t), graph, args.get(1)), mp -> {
                Term pred = args.get(0);
                Map<Term, Term> result = FromList.emptyLike(mp);
                for (Map.Entry<Term, Term> entry : mp.entrySet()) {
                    Either<InContext<Error_>, Term> r = hydra.Reduction.reduceTerm(
                        hydra.Lexical.emptyContext(), graph, true, Terms.apply(Terms.apply(pred, entry.getKey()), entry.getValue()));
                    if (r.isLeft()) return (Either) r;
                    Either<InContext<Error_>, Boolean> b = hydra.extract.Core.boolean_(cx, graph,
                        ((Either.Right<InContext<Error_>, Term>) r).value);
                    if (b.isLeft()) return (Either) b;
                    if (((Either.Right<InContext<Error_>, Boolean>) b).value) {
                        result.put(entry.getKey(), entry.getValue());
                    }
                }
                return Either.right(Terms.map(result));
            });
    }

    /**
     * Filters entries based on a curried key-value predicate (used by generated code).
     * @param <K> the key type
     * @param <V> the value type
     * @param pred the curried predicate: {@code key -> value -> Boolean}
     * @return a function that takes a map and returns the filtered map
     */
    public static <K, V> Function<Map<K, V>, Map<K, V>> apply(Function<K, Function<V, Boolean>> pred) {
        return mp -> apply(pred, mp);
    }

    /**
     * Filters entries based on a curried key-value predicate (used by generated code).
     * @param <K> the key type
     * @param <V> the value type
     * @param pred the curried predicate: {@code key -> value -> Boolean}
     * @param mp the map to filter
     * @return the filtered map
     */
    public static <K, V> Map<K, V> apply(Function<K, Function<V, Boolean>> pred, Map<K, V> mp) {
        Map<K, V> result = new TreeMap<>();
        for (Map.Entry<K, V> e : mp.entrySet()) {
            if (pred.apply(e.getKey()).apply(e.getValue())) {
                result.put(e.getKey(), e.getValue());
            }
        }
        return result;
    }
}
