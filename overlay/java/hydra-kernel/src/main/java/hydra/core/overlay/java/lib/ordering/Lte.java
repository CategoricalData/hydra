package hydra.core.overlay.java.lib.ordering;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.boolean_;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.schemeOrd;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Tests if the first value is less than or equal to the second.
 */
public class Lte extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Ordering.lte().name;
    }

    @Override
    public TypeScheme type() {
        return schemeOrd("x", function(Types.var("x"), Types.var("x"), boolean_()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            int cmp = Compare.compareTerms(args.get(0), args.get(1));
            return Either.right(Terms.boolean_(cmp <= 0));
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
