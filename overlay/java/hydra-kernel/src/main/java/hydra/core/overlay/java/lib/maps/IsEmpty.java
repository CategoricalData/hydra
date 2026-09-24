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

import static hydra.core.overlay.java.dsl.Types.boolean_;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.map;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Checks if a map is empty.
 */
public class IsEmpty extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return hydra.lib.Maps.isEmpty().name;
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE, function(
                map("k", "v"),
                boolean_()));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            Either<Error_, Map<Term, Term>> r = hydra.core.extract.Core.map(t -> Either.right(t), t -> Either.right(t), graph, args.get(0));
            return hydra.core.overlay.java.lib.eithers.Map.apply(map -> Terms.boolean_(map.isEmpty()), r);
        };
    }

    /**
     * Checks if the map is empty.
     * @param <K> the key type
     * @param <V> the value type
     * @param map the map to check
     * @return true if the map is empty, false otherwise
     */
    public static <K, V> Boolean apply(Map<K, V> map) {
        return map.isEmpty();
    }
}
