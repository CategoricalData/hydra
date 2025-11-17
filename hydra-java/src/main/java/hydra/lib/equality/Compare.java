package hydra.lib.equality;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Comparison;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;


/**
 * Compares two values and returns a Comparison (LessThan, EqualTo, or GreaterThan).
 */
public class Compare extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.equality.compare");
    }

    @Override
    public TypeScheme type() {
        return scheme("x", function(Types.var("x"), Types.var("x"), Types.apply(Types.var("hydra.util.Comparison"))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            // For Term comparison, we use a simple comparison based on the term structure
            // This is a simplified implementation; a full implementation would need proper Ord instance
            int cmp = compareTerms(args.get(0), args.get(1));
            if (cmp < 0) {
                return Flows.pure(Terms.injectUnit(Comparison.TYPE_NAME, Comparison.FIELD_NAME_LESS_THAN));
            } else if (cmp > 0) {
                return Flows.pure(Terms.injectUnit(Comparison.TYPE_NAME, Comparison.FIELD_NAME_GREATER_THAN));
            } else {
                return Flows.pure(Terms.injectUnit(Comparison.TYPE_NAME, Comparison.FIELD_NAME_EQUAL_TO));
            }
        };
    }

    private int compareTerms(Term t1, Term t2) {
        // Simple structural comparison
        // In a full implementation, this would use proper Ord typeclass
        return t1.toString().compareTo(t2.toString());
    }

    /**
     * Compares two comparable values.
     * @param <A> the comparable type
     * @param left the first value
     * @return a function that takes the second value and returns the comparison result
     */
    public static <A extends Comparable<A>> Function<A, Comparison> apply(A left) {
        return right -> apply(left, right);
    }

    /**
     * Compares two comparable values.
     * @param <A> the comparable type
     * @param left the first value
     * @param right the second value
     * @return the comparison result
     */
    public static <A extends Comparable<A>> Comparison apply(A left, A right) {
        int cmp = left.compareTo(right);
        if (cmp < 0) {
            return new Comparison.LessThan(true);
        } else if (cmp > 0) {
            return new Comparison.GreaterThan(true);
        } else {
            return new Comparison.EqualTo(true);
        }
    }
}
