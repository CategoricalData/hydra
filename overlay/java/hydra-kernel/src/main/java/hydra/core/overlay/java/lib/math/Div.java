package hydra.core.overlay.java.lib.math;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.schemeIntegral;
import static hydra.core.overlay.java.dsl.Types.var;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Divides two integers, returning Nothing if the divisor is zero.
 *
 * <p>Constraint-polymorphic ('integral') floor division: the type scheme is {@code integral x =>
 * x -> x -> optional x} and the implementation dispatches on the operands' runtime integer variant
 * via {@link IntegralDispatch}. No typeclass is consulted at runtime.
 */
public class Div extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Math_.div().name;
    }

    @Override
    public TypeScheme type() {
        return schemeIntegral("x", function(var("x"), var("x"), optional(var("x"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.right(IntegralDispatch.div(args.get(0), args.get(1)));
    }

    /**
     * Divides the first number by the second. This is the statically-typed entry point emitted
     * by generated code, generic and erased over the {@code integral} type variable (see
     * {@link IntegralDispatch#applyNativeDiv}).
     * @param dividend the dividend
     * @return a function that takes a divisor and returns a Optional containing the quotient
     */
    public static <A> Function<A, Optional<A>> apply(A dividend) {
        return (divisor) -> apply(dividend, divisor);
    }

    /**
     * Divides the first number by the second.
     * @param dividend the dividend
     * @param divisor the divisor
     * @return a Optional containing the quotient, or empty if divisor is zero
     */
    public static <A> Optional<A> apply(A dividend, A divisor) {
        return IntegralDispatch.applyNativeDiv(dividend, divisor);
    }
}
