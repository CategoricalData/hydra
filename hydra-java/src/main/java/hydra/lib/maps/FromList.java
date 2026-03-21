package hydra.lib.maps;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Pair;

import hydra.util.ConsList;
import hydra.util.PersistentMap;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.variable;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;

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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<ConsList<Pair<Term, Term>>, Term>) pairs -> new Term.Map(apply(pairs)), hydra.extract.core.Core.listOf(cx, term -> hydra.extract.core.Core.pair(cx, t -> Either.right(t), t -> Either.right(t), graph, term), graph, args.get(0)));
    }

    /**
     * Apply the function to its single argument.
     * @param <K> the key type
     * @param <V> the value type
     * @param pairs the list of key-value pairs
     * @return a map constructed from the pairs
     */
    @SuppressWarnings("unchecked")
    public static <K, V> PersistentMap<K, V> apply(ConsList<Pair<K, V>> pairs) {
        return PersistentMap.fromPairList((List) pairs);
    }

    @SuppressWarnings("unchecked")
    public static <K, V> PersistentMap<K, V> apply(List<Pair<K, V>> pairs) {
        return PersistentMap.fromPairList((List) pairs);
    }

    /**
     * Creates an ordered map from an existing map.
     * Uses TreeMap when keys are Comparable (matching Haskell's Data.Map behavior).
     * If the source is already a TreeMap, returns a shallow copy using the same comparator.
     */
    @SuppressWarnings("unchecked")
    static <K, V> Map<K, V> orderedMap(Map<K, V> source) {
        if (source instanceof PersistentMap) {
            // Convert to mutable TreeMap since callers expect a mutable map
            TreeMap<K, V> result = new TreeMap<>((a, b) -> ((Comparable<K>) a).compareTo(b));
            result.putAll(source);
            return result;
        }
        if (source instanceof TreeMap) {
            // Use the SortedMap copy constructor which iterates linearly,
            // rather than putAll which uses recursive buildFromSorted and
            // can cause StackOverflowError on large maps.
            return new TreeMap<>((java.util.SortedMap<K, V>) source);
        }
        if (source.isEmpty()) {
            return new LinkedHashMap<>();
        }
        for (K key : source.keySet()) {
            if (key != null) {
                if (key instanceof Comparable) {
                    try {
                        TreeMap<K, V> result = new TreeMap<>((a, b) -> ((Comparable<K>) a).compareTo(b));
                        result.putAll(source);
                        return result;
                    } catch (ClassCastException e) {
                        return new LinkedHashMap<>(source);
                    }
                } else {
                    return new LinkedHashMap<>(source);
                }
            }
        }
        return new LinkedHashMap<>(source);
    }

    /**
     * Creates an empty ordered map with the same ordering strategy as the source map.
     * If the source is a TreeMap, creates a new empty TreeMap with the same comparator.
     * If the source has Comparable keys, creates a TreeMap.
     * Otherwise creates a LinkedHashMap.
     */
    @SuppressWarnings("unchecked")
    static <K, V1, V2> Map<K, V2> emptyLike(Map<K, V1> source) {
        if (source instanceof PersistentMap) {
            // Return a mutable TreeMap with natural ordering for keys
            return new TreeMap<>((a, b) -> ((Comparable<K>) a).compareTo(b));
        }
        if (source instanceof TreeMap) {
            return new TreeMap<>(((TreeMap<K, V1>) source).comparator());
        }
        if (!source.isEmpty()) {
            for (K key : source.keySet()) {
                if (key != null) {
                    if (key instanceof Comparable) {
                        try {
                            return new TreeMap<>((a, b) -> ((Comparable<K>) a).compareTo(b));
                        } catch (ClassCastException e) {
                            return new LinkedHashMap<>();
                        }
                    }
                    break;
                }
            }
        }
        return new LinkedHashMap<>();
    }
}
