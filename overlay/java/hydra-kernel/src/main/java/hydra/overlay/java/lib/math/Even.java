package hydra.overlay.java.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.boolean_;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.schemeIntegral;
import static hydra.overlay.java.dsl.Types.var;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Determines whether an integer is even.
 *
 * <p>Constraint-polymorphic ('integral') parity test: the type scheme is {@code integral x => x
 * -> boolean} and the implementation dispatches on the operand's runtime integer variant via
 * {@link IntegralDispatch}. No typeclass is consulted at runtime.
 */
public class Even extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return hydra.lib.Math_.even().name;
    }

    /**
     * Gets the type scheme for this function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return schemeIntegral("x", function(var("x"), boolean_()));
    }

    /**
     * Provides the implementation of this function.
     * @return a function that maps terms to a flow of terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.right(Terms.boolean_(IntegralDispatch.even(args.get(0))));
    }

    /**
     * Checks if the number is even. This is the statically-typed entry point emitted by
     * generated code, generic and erased over the {@code integral} type variable (see
     * {@link IntegralDispatch#applyNativeEven}).
     * @param num the number
     * @return true if even, false otherwise
     */
    public static <A> Boolean apply(A num) {
        return IntegralDispatch.applyNativeEven(num);
    }
}
