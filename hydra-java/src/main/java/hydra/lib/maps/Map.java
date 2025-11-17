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

import java.util.HashMap;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.forall;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;


/**
 * Maps a function over a flow.
 */
public class Map extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.map");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("k", forall("v1", forall("v2",
            function(function("v1", "v2"), map("k", "v1"), map("k", "v2")))));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(
            Expect.map(Flows::pure, v -> pure(Terms.apply(args.get(0), v)), args.get(1)), Terms::map);
    }

    /**
     * Transforms a flow value.
     * @param <K> the key type
     * @param <V1> the input value type
     * @param <V2> the output value type
     * @param mapping the function to apply to each value
     * @return a function that takes a map and returns the transformed map
     */
    public static <K, V1, V2> Function<java.util.Map<K, V1>, java.util.Map<K, V2>> apply(Function<V1, V2> mapping) {
        return (arg) -> apply(mapping, arg);
    }

    /**
     * Apply the function to both arguments.
     * @param <K> the key type
     * @param <V1> the input value type
     * @param <V2> the output value type
     * @param mapping the function to apply to each value
     * @param arg the input map
     * @return the transformed map
     */
    public static <K, V1, V2> java.util.Map<K, V2> apply(Function<V1, V2> mapping, java.util.Map<K, V1> arg) {
        java.util.Map<K, V2> result = new HashMap<>();
        for (java.util.Map.Entry<K, V1> e : arg.entrySet()) {
            result.put(e.getKey(), mapping.apply(e.getValue()));
        }
        return result;
    }
}
