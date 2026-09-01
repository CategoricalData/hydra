package hydra.overlay.java.lib.ordering;

import hydra.core.*;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;
import hydra.util.Comparison;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.schemeOrd;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Compares two values and returns a Comparison (LessThan, EqualTo, or GreaterThan).
 */
public class Compare extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Ordering.compare().name;
    }

    @Override
    public TypeScheme type() {
        return schemeOrd("x", function(Types.var("x"), Types.var("x"), Types.apply(Types.var("hydra.util.Comparison"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            int cmp = compareTerms(args.get(0), args.get(1));
            if (cmp < 0) {
                return Either.right(Terms.injectUnit(Comparison.TYPE_, Comparison.LESS_THAN));
            } else if (cmp > 0) {
                return Either.right(Terms.injectUnit(Comparison.TYPE_, Comparison.GREATER_THAN));
            } else {
                return Either.right(Terms.injectUnit(Comparison.TYPE_, Comparison.EQUAL_TO));
            }
        };
    }

    /**
     * Compares two terms structurally, per docs/specification/ordering-and-equality.md.
     * Delegates to the generated {@link Term#compareTo}, which already implements the
     * spec's declared-variant-order-then-payload rule via {@code hydraOrdinal()} and
     * {@link hydra.overlay.java.util.Comparing}.
     */
    public static int compareTerms(Term t1, Term t2) {
        return t1.compareTo(t2);
    }

    /**
     * Compares two comparable values.
     * @param <A> the comparable type
     * @param left the first value
     * @return a function that takes the second value and returns the comparison result
     */
    @SuppressWarnings("unchecked")
    public static <A> Function<A, Comparison> apply(A left) {
        return right -> apply(left, right);
    }

    /**
     * Compares two comparable values.
     * @param <A> the comparable type
     * @param left the first value
     * @param right the second value
     * @return the comparison result
     */
    public static <A> Comparison apply(A left, A right) {
        int cmp = hydra.overlay.java.util.Comparing.compare(left, right);
        if (cmp < 0) {
            return new Comparison.LessThan();
        } else if (cmp > 0) {
            return new Comparison.GreaterThan();
        } else {
            return new Comparison.EqualTo();
        }
    }
}
