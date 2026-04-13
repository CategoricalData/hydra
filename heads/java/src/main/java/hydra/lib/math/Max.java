package hydra.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Returns the maximum of two numbers.
 */
public class Max extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return new Name("hydra.lib.math.max");
    }

    /**
     * Gets the type scheme for this function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme(function(int32(), int32(), int32()));
    }

    /**
     * Provides the implementation of this function.
     * @return a function that maps terms to a flow of terms
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.int32(graph, args.get(0)), arg0 -> hydra.lib.eithers.Map.apply(arg1 -> Terms.int32(apply(arg0, arg1)), hydra.extract.Core.int32(graph, args.get(1))));
    }

    /**
     * Returns the maximum of two numbers.
     * @param x the first number
     * @return a function that takes the second number and returns the maximum
     */
    public static Function<Integer, Integer> apply(Integer x) {
        return (y) -> apply(x, y);
    }

    /**
     * Returns the maximum of two numbers.
     * @param x the first number
     * @param y the second number
     * @return the maximum of x and y
     */
    public static Integer apply(Integer x, Integer y) {
        return java.lang.Math.max(x, y);
    }
}
