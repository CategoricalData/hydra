package hydra.overlay.java.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.schemeFrac;
import static hydra.overlay.java.dsl.Types.var;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Performs IEEE 754 division on two floating-point numbers.
 *
 * <p>Constraint-polymorphic ('fractional') division: the type scheme is {@code fractional x => x
 * -> x -> x} and the implementation dispatches on the operands' runtime float variant via
 * {@link FractionalDispatch}. No typeclass is consulted at runtime.
 */
public class Divide extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return hydra.lib.Math_.divide().name;
    }

    /**
     * Gets the type scheme for this function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return schemeFrac("x", function(var("x"), var("x"), var("x")));
    }

    /**
     * Provides the implementation of this function.
     * @return a function that maps terms to a flow of terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.right(FractionalDispatch.binary("divide", DIVIDE, args.get(0), args.get(1)));
    }

    private static final FractionalDispatch.BinaryOp DIVIDE = new FractionalDispatch.BinaryOp() {
        public double applyFloat(double a, double b) {
            return a / b;
        }

        public float applyFloat32(float a, float b) {
            return a / b;
        }
    };

    /**
     * Divides two numbers. This is the statically-typed entry point emitted by generated code. It
     * is generic and erased so that code polymorphic over a {@code fractional} type variable can
     * reference it as a {@code <A> A apply(A, A)} function value; the runtime float type is
     * recovered by dispatching on the operands' boxed class (see
     * {@link FractionalDispatch#applyNativeBinary}).
     * @param dividend the dividend
     * @return a function taking the divisor and returning the quotient
     */
    public static <A> Function<A, A> apply(A dividend) {
        return (divisor) -> apply(dividend, divisor);
    }

    /**
     * Divides two numbers.
     * @param dividend the dividend
     * @param divisor the divisor
     * @return the quotient
     */
    public static <A> A apply(A dividend, A divisor) {
        return FractionalDispatch.applyNativeBinary("divide", DIVIDE, dividend, divisor);
    }
}
