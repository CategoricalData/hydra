package hydra.core.overlay.java.lib.maps;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.map;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;
import hydra.core.overlay.java.util.PersistentMap;


/**
 * Computes the left-biased union of a list of maps.
 */
public class Unions extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return hydra.core.lib.Maps.unions().name;
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE,
                function(list(map("k", "v")), map("k", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(maps -> Terms.map(apply(maps)),
            hydra.core.extract.Model.listOf(t -> hydra.core.extract.Model.map(t2 -> Either.right(t2), t2 -> Either.right(t2), graph, t), graph, args.get(0)));
    }

    /**
     * Computes the left-biased union of a list of maps.
     * @param <K> the key type
     * @param <V> the value type
     * @param maps the list of maps to combine
     * @return a new map containing every binding of every map, earliest map winning on collision
     */
    public static <K, V> Map<K, V> apply(List<Map<K, V>> maps) {
        PersistentMap<K, V> result = PersistentMap.<K, V>empty();
        for (Map<K, V> mp : maps) {
            result = result.union(PersistentMap.<K, V>coerce(mp));
        }
        return result;
    }
}
