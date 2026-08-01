package hydra.overlay.java.lib.math;

import hydra.core.FloatValue;
import hydra.core.Literal;
import hydra.core.Term;
import hydra.overlay.java.dsl.Terms;

/**
 * Runtime dispatch for the constraint-polymorphic ('fractional') division primitive (divide).
 *
 * <p>This primitive is registered with a {@code fractional x => x -> x -> x} type scheme (the
 * constraint is carried in the type for inference only) and identity/pass-through behavior at the
 * marshalling boundary, so the runtime float type is discovered by dispatching on the argument's
 * literal variant. This mirrors the Haskell host's {@code divideTerm}/{@code floatDivide}
 * (see {@code Hydra.Overlay.Haskell.Lib.Math}) and {@link NumericDispatch}'s structural-dispatch
 * pattern. No typeclass mechanism is consulted at runtime — the host has none.
 *
 * <p>Both arms delegate to Java's native IEEE 754 double/float division, which already produces
 * the IEEE sentinels (±Infinity, NaN) for free (JLS §15.17.2: floating-point division never
 * raises).
 *
 * <p>Type inference guarantees both operands of divide share one {@code fractional} type, so the
 * dispatch keys on the first operand and requires the second to match; a mismatch or a
 * non-fractional operand is an internal invariant violation and fails loudly.
 */
public final class FractionalDispatch {
    private FractionalDispatch() {
    }

    /** A binary operation defined uniformly over the float representation domain. */
    public interface BinaryOp {
        double applyFloat(double a, double b);

        float applyFloat32(float a, float b);
    }

    /**
     * Apply a fractional binary operation to two fractional terms, dispatching on the runtime
     * variant.
     */
    public static Term binary(String opName, BinaryOp op, Term x, Term y) {
        Literal lx = fractionalLiteral(opName, x);
        Literal ly = fractionalLiteral(opName, y);
        if (lx instanceof Literal.Float_ && ly instanceof Literal.Float_) {
            return Terms.float_(floatBinary(opName, op, ((Literal.Float_) lx).value, ((Literal.Float_) ly).value));
        }
        throw new IllegalStateException("hydra.lib.math." + opName + ": operands are not the same fractional kind");
    }

    private static Literal fractionalLiteral(String opName, Term term) {
        if (term instanceof Term.Literal) {
            return ((Term.Literal) term).value;
        }
        throw new IllegalStateException("hydra.lib.math." + opName + ": expected a literal term");
    }

    private static FloatValue floatBinary(String opName, BinaryOp op, FloatValue fx, FloatValue fy) {
        if (fx instanceof FloatValue.Float32 && fy instanceof FloatValue.Float32) {
            return new FloatValue.Float32(op.applyFloat32(((FloatValue.Float32) fx).value, ((FloatValue.Float32) fy).value));
        }
        if (fx instanceof FloatValue.Float64 && fy instanceof FloatValue.Float64) {
            return new FloatValue.Float64(op.applyFloat(((FloatValue.Float64) fx).value, ((FloatValue.Float64) fy).value));
        }
        throw new IllegalStateException("hydra.lib.math." + opName + ": float operands differ in precision");
    }

    // ===== Native-value dispatch (the statically-typed generated-code entry point) =====

    /**
     * Apply a fractional binary operation to two boxed native operands, dispatching on their
     * runtime class. This is the body of the generic {@code <A> A apply(A, A)} static entry
     * points.
     */
    @SuppressWarnings("unchecked")
    public static <A> A applyNativeBinary(String opName, BinaryOp op, A a, A b) {
        if (a instanceof Float && b instanceof Float) {
            return (A) (Float) op.applyFloat32((Float) a, (Float) b);
        }
        if (a instanceof Double && b instanceof Double) {
            return (A) (Double) op.applyFloat((Double) a, (Double) b);
        }
        throw new IllegalStateException("hydra.lib.math." + opName + ": operand is not fractional: " + a);
    }
}
