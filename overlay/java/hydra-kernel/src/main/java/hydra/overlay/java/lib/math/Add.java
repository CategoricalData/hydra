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
 * Performs addition on two numbers.
 *
 * <p>Constraint-polymorphic ('numeric') arithmetic: the type scheme is {@code numeric x => x -> x
 * -> x} and the implementation dispatches on the operands' runtime numeric variant via
 * {@link NumericDispatch}. No typeclass is consulted at runtime.
 */
public class Add extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return hydra.lib.Math_.add().name;
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
        return args -> graph -> Either.right(NumericDispatch.binary("add", ADD, args.get(0), args.get(1)));
    }

    private static final NumericDispatch.BinaryOp ADD = new NumericDispatch.BinaryOp() {
        public java.math.BigInteger applyInteger(java.math.BigInteger a, java.math.BigInteger b) {
            return a.add(b);
        }

        public double applyFloat(double a, double b) {
            return a + b;
        }

        public float applyFloat32(float a, float b) {
            return a + b;
        }
    };

    /**
     * Adds two numbers. This is the statically-typed entry point emitted by generated code. It is
     * generic and erased so that code polymorphic over a {@code numeric} type variable (e.g. a user
     * definition inferred as {@code numeric a => [a] -> a}) can reference it as a {@code <A> A
     * apply(A, A)} function value; the runtime numeric type is recovered by dispatching on the
     * operands' boxed class (see {@link NumericDispatch#applyNativeBinary}).
     * @param augend the augend
     * @return a function taking the addend and returning the sum
     */
    public static <A> Function<A, A> apply(A augend) {
        return (addend) -> apply(augend, addend);
    }

    /**
     * Adds two numbers.
     * @param augend the augend
     * @param addend the addend
     * @return the sum
     */
    public static <A> A apply(A augend, A addend) {
        return NumericDispatch.applyNativeBinary("add", ADD, augend, addend);
    }
}
