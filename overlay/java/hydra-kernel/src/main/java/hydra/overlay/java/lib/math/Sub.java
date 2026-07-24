package hydra.overlay.java.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.schemeNum;
import static hydra.overlay.java.dsl.Types.var;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Performs subtraction on two numbers.
 *
 * <p>Constraint-polymorphic ('numeric') arithmetic: the type scheme is {@code numeric x => x -> x
 * -> x} and the implementation dispatches on the operands' runtime numeric variant via
 * {@link NumericDispatch}. No typeclass is consulted at runtime.
 */
public class Sub extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return hydra.lib.Math_.sub().name;
    }

    /**
     * Gets the type scheme for this function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return schemeNum("x", function(var("x"), var("x"), var("x")));
    }

    /**
     * Provides the implementation of this function.
     * @return a function that maps terms to a flow of terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.right(NumericDispatch.binary("sub", SUB, args.get(0), args.get(1)));
    }

    private static final NumericDispatch.BinaryOp SUB = new NumericDispatch.BinaryOp() {
        public java.math.BigInteger applyInteger(java.math.BigInteger a, java.math.BigInteger b) {
            return a.subtract(b);
        }

        public double applyFloat(double a, double b) {
            return a - b;
        }

        public float applyFloat32(float a, float b) {
            return a - b;
        }
    };

    /**
     * Subtracts the second number from the first. This is the statically-typed entry point emitted
     * by generated code. It is generic and erased so that code polymorphic over a {@code numeric}
     * type variable can reference it as a {@code <A> A apply(A, A)} function value; the runtime
     * numeric type is recovered by dispatching on the operands' boxed class (see
     * {@link NumericDispatch#applyNativeBinary}).
     * @param minuend the minuend
     * @return a function taking the subtrahend and returning the difference
     */
    public static <A> Function<A, A> apply(A minuend) {
        return (subtrahend) -> apply(minuend, subtrahend);
    }

    /**
     * Subtracts the second number from the first.
     * @param minuend the minuend
     * @param subtrahend the subtrahend
     * @return the difference
     */
    public static <A> A apply(A minuend, A subtrahend) {
        return NumericDispatch.applyNativeBinary("sub", SUB, minuend, subtrahend);
    }
}
