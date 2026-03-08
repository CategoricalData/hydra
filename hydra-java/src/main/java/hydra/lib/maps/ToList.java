package hydra.lib.maps;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Pair;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.variable;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


/**
 * Converts a map to a list of pairs.
 */
public class ToList extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.toList");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE, function(
                map("k", "v"),
                list(pair(variable("k"), variable("v")))));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> {
            Either<InContext<OtherError>, Map<Term, Term>> r = hydra.extract.core.Core.map(cx, t -> Either.right(t), t -> Either.right(t), graph, args.get(0));
            return hydra.lib.eithers.Map.apply(map -> {
                java.util.List<Map.Entry<Term, Term>> entries = new java.util.ArrayList<>(map.entrySet());
                entries.sort((a, b) -> hydra.lib.equality.Compare.compareTerms(a.getKey(), b.getKey()));
                return Terms.list(entries.stream().map(
                    e -> Terms.pair(e.getKey(), e.getValue())).collect(Collectors.toList()));
            }, r);
        };
    }

    /**
     * Apply the function to its single argument.
     * @param <K> the key type
     * @param <V> the value type
     * @param map the map to convert
     * @return a list of key-value pairs
     */
    @SuppressWarnings("unchecked")
    public static <K, V> List<Pair<K, V>> apply(Map<K, V> map) {
        List<Pair<K, V>> pairs = new java.util.ArrayList<>(map.size());
        for (Map.Entry<K, V> entry : map.entrySet()) {
            pairs.add(new Pair<>(entry.getKey(), entry.getValue()));
        }
        if (!pairs.isEmpty()) {
            if (pairs.get(0).first instanceof Comparable) {
                pairs.sort((a, b) -> ((Comparable<K>) a.first).compareTo(b.first));
            } else if (pairs.get(0).first instanceof Term) {
                pairs.sort((a, b) -> hydra.lib.equality.Compare.compareTerms((Term) a.first, (Term) b.first));
            }
        }
        return pairs;
    }
}
