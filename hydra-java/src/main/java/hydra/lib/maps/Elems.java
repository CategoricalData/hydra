package hydra.lib.maps;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;

/**
 * Returns all values.
 */
public class Elems extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.elems");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE,
                function(
                        map("k", "v"),
                        list("v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> {
            Either<InContext<Error_>, Map<Term, Term>> r = hydra.extract.core.Core.map(cx, t -> Either.right(t), t -> Either.right(t), graph, args.get(0));
            return hydra.lib.eithers.Map.apply(map -> {
                java.util.List<Map.Entry<Term, Term>> entries = new java.util.ArrayList<>(map.entrySet());
                entries.sort((a, b) -> hydra.lib.equality.Compare.compareTerms(a.getKey(), b.getKey()));
                java.util.List<Term> values = new java.util.ArrayList<>();
                for (Map.Entry<Term, Term> e : entries) {
                    values.add(e.getValue());
                }
                return Terms.list(values);
            }, r);
        };
    }

    /**
     * Returns the list of values.
     * @param <K> the key type
     * @param <V> the value type
     * @param map the map
     * @return the values
     */
    @SuppressWarnings("unchecked")
    public static <K, V> List<V> apply(Map<K, V> map) {
        // Sort by key to match Haskell's Data.Map.elems which returns values in key order
        if (!map.isEmpty()) {
            K firstKey = map.keySet().iterator().next();
            if (firstKey instanceof Comparable || firstKey instanceof Term) {
                List<Map.Entry<K, V>> entries = new ArrayList<>(map.entrySet());
                if (firstKey instanceof Comparable) {
                    entries.sort((a, b) -> ((Comparable<K>) a.getKey()).compareTo(b.getKey()));
                } else {
                    entries.sort((a, b) -> hydra.lib.equality.Compare.compareTerms((Term) a.getKey(), (Term) b.getKey()));
                }
                List<V> result = new ArrayList<>();
                for (Map.Entry<K, V> e : entries) {
                    result.add(e.getValue());
                }
                return result;
            }
        }
        return new ArrayList<V>(map.values());
    }
}
