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

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;


/**
 * Tests if the first value is less than or equal to the second.
 */
public class Lte extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.equality.lte");
    }

    @Override
    public TypeScheme type() {
        return scheme("x", function(Types.var("x"), Types.var("x"), boolean_()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            int cmp = Compare.compareTerms(args.get(0), args.get(1));
            return Flows.pure(Terms.boolean_(cmp <= 0));
        };
    }

    /**
     * Tests if the first value is less than or equal to the second.
     * @param <A> the comparable type
     * @param left the first value
     * @return a function that takes the second value and returns true if left &lt;= right
     */
    @SuppressWarnings("unchecked")
    public static <A> Function<A, Boolean> apply(A left) {
        return right -> apply(left, right);
    }

    /**
     * Tests if the first value is less than or equal to the second.
     * @param <A> the comparable type
     * @param left the first value
     * @param right the second value
     * @return true if left &lt;= right
     */
    @SuppressWarnings("unchecked")
    public static <A> Boolean apply(A left, A right) {
        return ((Comparable) left).compareTo(right) <= 0;
    }
}
