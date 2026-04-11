package hydra.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.float64;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Performs multiplication on two float64 numbers.
 */
public class MulFloat64 extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return new Name("hydra.lib.math.mulFloat64");
    }

    /**
     * Gets the type scheme for this function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme(function(float64(), float64(), float64()));
    }

    /**
     * Provides the implementation of this function.
     * @return a function that maps terms to a flow of terms
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.float64(graph, args.get(0)), arg0 -> hydra.lib.eithers.Map.apply(arg1 -> Terms.float64(apply(arg0, arg1)), hydra.extract.Core.float64(graph, args.get(1))));
    }

    /**
     * Multiplies two float64 numbers.
     * @param multiplier the multiplicand
     * @return the product
     */
    public static Function<Double, Double> apply(Double multiplier) {
        return (multiplicand) -> apply(multiplier, multiplicand);
    }

    /**
     * Multiplies two float64 numbers.
     * @param multiplier the multiplicand
     * @param multiplicand the multiplier
     * @return the product
     */
    public static Double apply(Double multiplier, Double multiplicand) {
        return (multiplier * multiplicand);
    }
}
