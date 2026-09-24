package hydra.core.overlay.java.lib.ordering;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.schemeOrd;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Returns the maximum of two values.
 */
public class Max extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Ordering.max().name;
    }

    @Override
    public TypeScheme type() {
        return schemeOrd("x", function(Types.var("x"), Types.var("x"), Types.var("x")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            int cmp = Compare.compareTerms(args.get(0), args.get(1));
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
