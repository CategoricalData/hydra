package hydra.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import hydra.util.ConsList;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.int32(cx, graph, args.get(0)), arg0 -> hydra.lib.eithers.Map.apply(arg1 -> Terms.list(apply(arg0, arg1).stream()
                .map(Terms::int32)
                .collect(Collectors.toList())), hydra.extract.core.Core.int32(cx, graph, args.get(1))));
    }

    /**
     * Generates a range from start to end.
     * @param start the start
     * @return the list of integers
     */
    public static Function<Integer, ConsList<Integer>> apply(Integer start) {
        return (end) -> apply(start, end);
    }

    /**
     * Generates a range from start to end.
     * @param start the start
     * @param end the end
     * @return the list of integers
     */
    public static ConsList<Integer> apply(Integer start, Integer end) {
        if (start > end) {
            return ConsList.empty();
        }
        return ConsList.fromList(IntStream.rangeClosed(start, end)
            .boxed()
            .collect(Collectors.toList()));
    }
}
