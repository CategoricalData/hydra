package hydra.lib.maps;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Pair;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.variable;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;
import hydra.util.PersistentMap;

/**
 * Creates a map from a list of pairs.
 */
public class FromList extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.fromList");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE,
                function(list(pair(variable("k"), variable("v"))), map("k", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<List<Pair<Term, Term>>, Term>) pairs -> new Term.Map(apply(pairs)), hydra.extract.Core.listOf(term -> hydra.extract.Core.pair(t -> Either.right(t), t -> Either.right(t), graph, term), graph, args.get(0)));
    }

    /**
     * Apply the function to its single argument.
     * @param <K> the key type
     * @param <V> the value type
     * @param pairs the list of key-value pairs
     * @return a map constructed from the pairs
     */
    public static <K, V> Map<K, V> apply(List<Pair<K, V>> pairs) {
        @SuppressWarnings({"rawtypes", "unchecked"})
        PersistentMap<K, V> result = PersistentMap.<K, V>empty();
        for (Pair<K, V> p : pairs) {
            result = result.insert(p.first, p.second);
        }
        return result;
    }

    /**
     * Returns a PersistentMap with the same entries as the source. If the source is
     * already a PersistentMap, returns it unchanged; otherwise builds one entry by
     * entry. Replaces an earlier TreeMap-based helper.
     */
    static <K, V> Map<K, V> orderedMap(Map<K, V> source) {
        return PersistentMap.coerce(source);
    }

    /**
     * Creates an empty PersistentMap. The signature is retained for callers that
     * previously consulted the source map's ordering strategy; PersistentMap is
     * always Comparable-ordered so the source is unused.
     */
    static <K, V1, V2> Map<K, V2> emptyLike(Map<K, V1> source) {
        return PersistentMap.empty();
    }
}
