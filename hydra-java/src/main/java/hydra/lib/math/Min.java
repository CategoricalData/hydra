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
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


/**
 * Returns the minimum of two numbers.
 */
public class Min extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return new Name("hydra.lib.math.min");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.int32(cx, graph, args.get(0)), arg0 -> hydra.lib.eithers.Map.apply(arg1 -> Terms.int32(apply(arg0, arg1)), hydra.extract.core.Core.int32(cx, graph, args.get(1))));
    }

    /**
     * Returns the minimum of two numbers.
     * @param x the first number
     * @return a function that takes the second number and returns the minimum
     */
    public static Function<Integer, Integer> apply(Integer x) {
        return (y) -> apply(x, y);
    }

    /**
     * Returns the minimum of two numbers.
     * @param x the first number
     * @param y the second number
     * @return the minimum of x and y
     */
    public static Integer apply(Integer x, Integer y) {
        return java.lang.Math.min(x, y);
    }
}
