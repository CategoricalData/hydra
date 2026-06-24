package hydra.overlay.java.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.int32;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Either;


/**
 * Generates a range of integers.
 */
public class Range extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return hydra.lib.Math_.range().name;
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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.int32(graph, args.get(0)), arg0 -> hydra.overlay.java.lib.eithers.Map.apply(arg1 -> {
                ConsList<Term> result = ConsList.empty();
                for (int i = arg1; i >= arg0; i--) {
                    result = ConsList.cons(Terms.int32(i), result);
                }
                return Terms.list(result);
            }, hydra.extract.Core.int32(graph, args.get(1))));
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
            return ConsList.empty();
        }
        ConsList<Integer> result = ConsList.empty();
        for (int i = end; i >= start; i--) {
            result = ConsList.cons(i, result);
        }
        return result;
    }
}
