package hydra.lib.equality;

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
import static hydra.dsl.Types.schemeOrd;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Returns the maximum of two values.
 */
public class Max extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.equality.max");
    }

    @Override
    public TypeScheme type() {
        return schemeOrd("x", function(Types.var("x"), Types.var("x"), Types.var("x")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> {
            // Simple comparison based on term structure
            int cmp = args.get(0).toString().compareTo(args.get(1).toString());
            return Either.right(cmp >= 0 ? args.get(0) : args.get(1));
        };
    }

    /**
     * Returns the maximum of two values.
     * @param <A> the comparable type
     * @param left the first value
     * @return a function that takes the second value and returns the maximum
     */
    @SuppressWarnings("unchecked")
    public static <A> Function<A, A> apply(A left) {
        return right -> apply(left, right);
    }

    /**
     * Returns the maximum of two values.
     * @param <A> the comparable type
     * @param left the first value
     * @param right the second value
     * @return the maximum of the two values
     */
    @SuppressWarnings("unchecked")
    public static <A> A apply(A left, A right) {
        return ((Comparable) left).compareTo(right) >= 0 ? left : right;
    }
}
