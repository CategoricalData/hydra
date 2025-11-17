package hydra.lib.math;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static hydra.dsl.Flows.map2;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


/**
 * Generates a range of integers.
 */
public class Range extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return new Name("hydra.lib.math.range");
    }

    /**
     * Gets the type scheme for this function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme(function(int32(), int32(), list(int32())));
    }

    /**
     * Provides the implementation of this function.
     * @return a function that maps terms to a flow of terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map2(Expect.int32(args.get(0)), Expect.int32(args.get(1)),
            (arg0, arg1) -> Terms.list(apply(arg0, arg1).stream()
                .map(Terms::int32)
                .collect(Collectors.toList())));
    }

    /**
     * Generates a range from start to end.
     * @param start the start
     * @return the list of integers
     */
    public static Function<Integer, List<Integer>> apply(Integer start) {
        return (end) -> apply(start, end);
    }

    /**
     * Generates a range from start to end.
     * @param start the start
     * @param end the end
     * @return the list of integers
     */
    public static List<Integer> apply(Integer start, Integer end) {
        if (start > end) {
            return new ArrayList<>();
        }
        return IntStream.rangeClosed(start, end)
            .boxed()
            .collect(Collectors.toList());
    }
}
