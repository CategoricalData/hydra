package hydra.lib.maps;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Flows.map;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;


/**
 * Returns the number of entries.
 */
public class Size extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.size");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("k", "v",
                function(hydra.dsl.Types.map("k", "v"), int32()));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map(Expect.map(Flows::pure, Flows::pure, args.get(0)),
                (Function<Map<Term, Term>, Term>) mp -> Terms.int32(mp.size()));
    }

    /**
     * Returns the size of the map.
     * @param <K> the key type
     * @param <V> the value type
     * @param mp the map
     * @return the size
     */
    public static <K, V> Integer apply(Map<K, V> mp) {
        return mp.size();
    }
}
