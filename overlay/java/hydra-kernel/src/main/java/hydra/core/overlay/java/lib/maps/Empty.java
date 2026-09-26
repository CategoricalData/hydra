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

import static hydra.core.overlay.java.dsl.Types.map;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;
import hydra.core.overlay.java.util.PersistentMap;

/**
 * Creates an empty map.
 */
public class Empty extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return hydra.core.lib.Maps.empty().name;
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE, map("k", "v"));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return ignored -> graph -> Either.right(Terms.map(apply()));
    }

    /**
     * Creates an empty map.
     * @param <K> the key type
     * @param <V> the value type
     * @return the empty map
     */
    public static <K, V> Map<K, V> apply() {
        return PersistentMap.empty();
    }
}
