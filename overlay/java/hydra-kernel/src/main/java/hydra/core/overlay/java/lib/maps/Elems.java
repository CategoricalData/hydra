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
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Either;

/**
 * Returns all values.
 */
public class Elems extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return hydra.core.lib.Maps.elems().name;
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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            Either<Error_, Map<Term, Term>> r = hydra.core.extract.Model.map(t -> Either.right(t), t -> Either.right(t), graph, args.get(0));
            return hydra.core.overlay.java.lib.eithers.Map.apply(m -> Terms.list(apply(m)), r);
        };
    }

    /**
     * Returns the list of values.
     * @param <K> the key type
     * @param <V> the value type
     * @param map the map
     * @return the values
     */
    public static <K, V> List<V> apply(Map<K, V> map) {
        ConsList<V> reversed = ConsList.empty();
        for (V v : map.values()) {
            reversed = ConsList.cons(v, reversed);
        }
        return reversed.reverse();
    }
}
