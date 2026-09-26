package hydra.core.overlay.java.lib.maps;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.Pair;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.map;
import static hydra.core.overlay.java.dsl.Types.pair;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.variable;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;
import hydra.core.overlay.java.util.PersistentMap;

/**
 * Creates a map from a list of pairs.
 */
public class FromList extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return hydra.core.lib.Maps.fromList().name;
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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<List<Pair<Term, Term>>, Term>) pairs -> new Term.Map(apply(pairs)), hydra.core.extract.Model.listOf(term -> hydra.core.extract.Model.pair(t -> Either.right(t), t -> Either.right(t), graph, term), graph, args.get(0)));
    }

    /**
     * Apply the function to its single argument.
     * @param <K> the key type
     * @param <V> the value type
     * @param pairs the list of key-value pairs
     * @return a map constructed from the pairs
     */
    public static <K, V> Map<K, V> apply(List<Pair<K, V>> pairs) {
        PersistentMap<K, V> result = PersistentMap.<K, V>empty();
        for (Pair<K, V> p : pairs) {
            result = result.insert(p.first, p.second);
        }
        return result;
    }
}
