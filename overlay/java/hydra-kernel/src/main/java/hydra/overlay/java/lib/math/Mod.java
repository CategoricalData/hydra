package hydra.overlay.java.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;
import hydra.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.optional;
import static hydra.overlay.java.dsl.Types.schemeIntegral;
import static hydra.overlay.java.dsl.Types.var;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Compute the modulo of two integers, returning Nothing if the divisor is zero.
 *
 * <p>Constraint-polymorphic ('integral') floor modulus: the type scheme is {@code integral x =>
 * x -> x -> optional x} and the implementation dispatches on the operands' runtime integer variant
 * via {@link IntegralDispatch}. No typeclass is consulted at runtime.
 */
public class Mod extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Math_.mod().name;
    }

    @Override
    public TypeScheme type() {
        return schemeIntegral("x", function(var("x"), var("x"), optional(var("x"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.right(IntegralDispatch.mod(args.get(0), args.get(1)));
    }

    /**
     * Computes the modulo. This is the statically-typed entry point emitted by generated code,
     * generic and erased over the {@code integral} type variable (see
     * {@link IntegralDispatch#applyNativeMod}).
     * @param dividend the dividend
     * @return a function that takes a divisor and returns a Optional containing the modulo
     */
    public static <A> Function<A, Optional<A>> apply(A dividend) {
        return (divisor) -> apply(dividend, divisor);
    }

    /**
     * Computes the modulo.
     * @param dividend the dividend
     * @param divisor the divisor
     * @return a Optional containing the modulo, or empty if divisor is zero
     */
    public static <A> Optional<A> apply(A dividend, A divisor) {
        return IntegralDispatch.applyNativeMod(dividend, divisor);
    }
}
