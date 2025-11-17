package hydra.lib.equality;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;


/**
 * Returns the minimum of two values.
 */
public class Min extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.equality.min");
    }

    @Override
    public TypeScheme type() {
        return scheme("x", function(Types.var("x"), Types.var("x"), Types.var("x")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            // Simple comparison based on term structure
            int cmp = args.get(0).toString().compareTo(args.get(1).toString());
            return Flows.pure(cmp <= 0 ? args.get(0) : args.get(1));
        };
    }

    /**
     * Returns the minimum of two values.
     * @param <A> the comparable type
     * @param left the first value
     * @return a function that takes the second value and returns the minimum
     */
    public static <A extends Comparable<A>> Function<A, A> apply(A left) {
        return right -> apply(left, right);
    }

    /**
     * Returns the minimum of two values.
     * @param <A> the comparable type
     * @param left the first value
     * @param right the second value
     * @return the minimum of the two values
     */
    public static <A extends Comparable<A>> A apply(A left, A right) {
        return left.compareTo(right) <= 0 ? left : right;
    }
}
