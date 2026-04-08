package hydra.lib.equality;

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
import static hydra.dsl.Types.schemeOrd;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Tests if the first value is greater than the second.
 */
public class Gt extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.equality.gt");
    }

    @Override
    public TypeScheme type() {
        return schemeOrd("x", function(Types.var("x"), Types.var("x"), boolean_()));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> {
            int cmp = Compare.compareTerms(args.get(0), args.get(1));
            return Either.right(Terms.boolean_(cmp > 0));
        };
    }

    /**
     * Tests if the first value is greater than the second.
     * @param <A> the comparable type
     * @param left the first value
     * @return a function that takes the second value and returns true if left &gt; right
     */
    @SuppressWarnings("unchecked")
    public static <A> Function<A, Boolean> apply(A left) {
        return right -> apply(left, right);
    }

    /**
     * Tests if the first value is greater than the second.
     * @param <A> the comparable type
     * @param left the first value
     * @param right the second value
     * @return true if left &gt; right
     */
    @SuppressWarnings("unchecked")
    public static <A> Boolean apply(A left, A right) {
        return ((Comparable) left).compareTo(right) > 0;
    }
}
