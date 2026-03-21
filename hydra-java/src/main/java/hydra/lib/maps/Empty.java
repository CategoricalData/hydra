package hydra.lib.maps;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import hydra.util.PersistentMap;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Creates an empty map.
 */
public class Empty extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.empty");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return ignored -> cx -> graph -> Either.right(Terms.map(apply()));
    }

    /**
     * Creates an empty map.
     * @param <K> the key type
     * @param <V> the value type
     * @return the empty map
     */
    public static <K, V> PersistentMap<K, V> apply() {
        return PersistentMap.empty();
    }
}
