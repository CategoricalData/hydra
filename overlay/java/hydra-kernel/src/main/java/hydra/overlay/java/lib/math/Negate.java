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
 * Negates a number.
 *
 * <p>Constraint-polymorphic ('numeric') arithmetic: the type scheme is {@code numeric x => x -> x}
 * and the implementation dispatches on the operand's runtime numeric variant via
 * {@link NumericDispatch}. No typeclass is consulted at runtime.
 */
public class Negate extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return hydra.lib.Math_.negate().name;
    }

    /**
     * Gets the type scheme for this function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return schemeNum("x", function(var("x"), var("x")));
    }

    /**
     * Provides the implementation of this function.
     * @return a function that maps terms to a flow of terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.right(NumericDispatch.unary("negate", NEGATE, args.get(0)));
    }

    private static final NumericDispatch.UnaryOp NEGATE = new NumericDispatch.UnaryOp() {
        public java.math.BigInteger applyInteger(java.math.BigInteger a) {
            return a.negate();
        }

        public double applyFloat(double a) {
            return -a;
        }

        public float applyFloat32(float a) {
            return -a;
        }
    };

    /**
     * Negates the given number. This is the statically-typed entry point emitted by generated code.
     * It is generic and erased so that code polymorphic over a {@code numeric} type variable can
     * reference it as a {@code <A> A apply(A)} function value; the runtime numeric type is recovered
     * by dispatching on the operand's boxed class (see {@link NumericDispatch#applyNativeUnary}).
     * @param num the number
     * @return the negated value
     */
    public static <A> A apply(A num) {
        return NumericDispatch.applyNativeUnary("negate", NEGATE, num);
    }
}
