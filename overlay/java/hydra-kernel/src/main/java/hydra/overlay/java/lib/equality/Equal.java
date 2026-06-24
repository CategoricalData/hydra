package hydra.overlay.java.lib.equality;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.boolean_;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.schemeEq;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Tests equality.
 */
public class Equal extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Equality.equal().name;
    }

    @Override
    public TypeScheme type() {
        return schemeEq("x", function(Types.var("x"), Types.var("x"), boolean_()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        // Term equality is sufficient here, as we assume that type inference has already succeeded.
        return args -> graph -> Either.right(Terms.boolean_(args.get(0).equals(args.get(1))));
    }

    /**
     * Checks if two values are equal.
     * @param <A> the type
     * @param left the value1
     * @return true if equal, false otherwise
     */
    public static <A> Function<A, Boolean> apply(A left) {
        return right -> apply(left, right);
    }

    /**
     * Checks if two values are equal.
     * @param <A> the type
     * @param left the value1
     * @param right the value2
     * @return true if equal, false otherwise
     */
    public static <A> boolean apply(A left, A right) {
        // Special handling for BigDecimal: use compareTo for scale-insensitive comparison
        // (e.g., 42.0 and 42 should be equal)
        if (left instanceof java.math.BigDecimal && right instanceof java.math.BigDecimal) {
            return ((java.math.BigDecimal) left).compareTo((java.math.BigDecimal) right) == 0;
        }
        return left.equals(right);
    }
}
