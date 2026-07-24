package hydra.overlay.java.lib.math;

import hydra.core.FloatValue;
import hydra.core.IntegerValue;
import hydra.core.Literal;
import hydra.core.Term;
import hydra.overlay.java.dsl.Terms;

import java.math.BigInteger;

/**
 * Runtime dispatch for the constraint-polymorphic ('numeric') arithmetic primitives
 * (add/sub/mul/negate).
 *
 * <p>These primitives are registered with a {@code numeric x => x -> x -> x} type scheme (the
 * constraint is carried in the type for inference only) and identity/pass-through behavior at the
 * marshalling boundary, so the runtime numeric type is discovered by dispatching on the argument's
 * literal variant. This mirrors the Haskell host's {@code numericBinary}/{@code numericUnary}
 * (see {@code Hydra.Overlay.Haskell.Lib.Math}) and the structural-dispatch pattern already used by
 * {@code equality.compare}. No typeclass mechanism is consulted at runtime — the host has none.
 *
 * <p>Integer arithmetic is performed in {@link BigInteger} and narrowed back to the source
 * variant's width, giving two's-complement wraparound for the fixed-width types and arbitrary
 * precision for bigint. Float arithmetic is performed in IEEE 754 double/float per variant.
 *
 * <p>Type inference guarantees both operands of a binary op share one {@code numeric} type, so the
 * dispatch keys on the first operand and requires the second to match; a mismatch or a non-numeric
 * operand is an internal invariant violation and fails loudly.
 */
public final class NumericDispatch {
    private NumericDispatch() {
    }

    /** A binary operation defined uniformly over the integer and float representation domains. */
    public interface BinaryOp {
        BigInteger applyInteger(BigInteger a, BigInteger b);

        double applyFloat(double a, double b);

        float applyFloat32(float a, float b);
    }

    /** A unary operation defined uniformly over the integer and float representation domains. */
    public interface UnaryOp {
        BigInteger applyInteger(BigInteger a);

        double applyFloat(double a);

        float applyFloat32(float a);
    }

    /**
     * Apply a numeric binary operation to two numeric terms, dispatching on the runtime variant.
     */
    public static Term binary(String opName, BinaryOp op, Term x, Term y) {
        Literal lx = numericLiteral(opName, x);
        Literal ly = numericLiteral(opName, y);
        if (lx instanceof Literal.Integer_ && ly instanceof Literal.Integer_) {
            return Terms.integer(integerBinary(opName, op, ((Literal.Integer_) lx).value, ((Literal.Integer_) ly).value));
        }
        if (lx instanceof Literal.Float_ && ly instanceof Literal.Float_) {
            return Terms.float_(floatBinary(opName, op, ((Literal.Float_) lx).value, ((Literal.Float_) ly).value));
        }
        throw new IllegalStateException("hydra.lib.math." + opName + ": operands are not the same numeric kind");
    }

    /**
     * Apply a numeric unary operation to a numeric term, dispatching on the runtime variant.
     */
    public static Term unary(String opName, UnaryOp op, Term x) {
        Literal lx = numericLiteral(opName, x);
        if (lx instanceof Literal.Integer_) {
            return Terms.integer(integerUnary(op, ((Literal.Integer_) lx).value));
        }
        if (lx instanceof Literal.Float_) {
            return Terms.float_(floatUnary(op, ((Literal.Float_) lx).value));
        }
        throw new IllegalStateException("hydra.lib.math." + opName + ": operand is not numeric");
    }

    private static Literal numericLiteral(String opName, Term term) {
        if (term instanceof Term.Literal) {
            return ((Term.Literal) term).value;
        }
        throw new IllegalStateException("hydra.lib.math." + opName + ": expected a literal term");
    }

    private static IntegerValue integerBinary(String opName, BinaryOp op, IntegerValue ix, IntegerValue iy) {
        BigInteger r = op.applyInteger(toBigInteger(ix), toBigInteger(iy));
        return rewrapInteger(opName, ix, iy, r);
    }

    private static IntegerValue integerUnary(UnaryOp op, IntegerValue iv) {
        BigInteger r = op.applyInteger(toBigInteger(iv));
        return rewrapInteger(iv, r);
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

    private static FloatValue floatUnary(UnaryOp op, FloatValue fv) {
        if (fv instanceof FloatValue.Float32) {
            return new FloatValue.Float32(op.applyFloat32(((FloatValue.Float32) fv).value));
        }
        return new FloatValue.Float64(op.applyFloat(((FloatValue.Float64) fv).value));
    }

    private static BigInteger toBigInteger(IntegerValue v) {
        if (v instanceof IntegerValue.Int8) return BigInteger.valueOf(((IntegerValue.Int8) v).value);
        if (v instanceof IntegerValue.Int16) return BigInteger.valueOf(((IntegerValue.Int16) v).value);
        if (v instanceof IntegerValue.Int32) return BigInteger.valueOf(((IntegerValue.Int32) v).value);
        if (v instanceof IntegerValue.Int64) return BigInteger.valueOf(((IntegerValue.Int64) v).value);
        if (v instanceof IntegerValue.Uint8) return BigInteger.valueOf(((IntegerValue.Uint8) v).value);
        if (v instanceof IntegerValue.Uint16) return BigInteger.valueOf(((IntegerValue.Uint16) v).value);
        if (v instanceof IntegerValue.Uint32) return BigInteger.valueOf(((IntegerValue.Uint32) v).value);
        if (v instanceof IntegerValue.Uint64) return ((IntegerValue.Uint64) v).value;
        if (v instanceof IntegerValue.Bigint) return ((IntegerValue.Bigint) v).value;
        throw new IllegalStateException("unexpected IntegerValue variant: " + v);
    }

    private static IntegerValue rewrapInteger(String opName, IntegerValue ix, IntegerValue iy, BigInteger r) {
        if (ix.getClass() != iy.getClass()) {
            throw new IllegalStateException("hydra.lib.math." + opName + ": integer operands differ in precision");
        }
        return rewrapInteger(ix, r);
    }

    // Narrow the arbitrary-precision result back to the source variant's width, giving
    // two's-complement wraparound for the fixed-width types and full precision for bigint.
    private static IntegerValue rewrapInteger(IntegerValue like, BigInteger r) {
        if (like instanceof IntegerValue.Int8) return new IntegerValue.Int8((byte) r.intValue());
        if (like instanceof IntegerValue.Int16) return new IntegerValue.Int16((short) r.intValue());
        if (like instanceof IntegerValue.Int32) return new IntegerValue.Int32(r.intValue());
        if (like instanceof IntegerValue.Int64) return new IntegerValue.Int64(r.longValue());
        if (like instanceof IntegerValue.Uint8) return new IntegerValue.Uint8((short) (r.intValue() & 0xFF));
        if (like instanceof IntegerValue.Uint16) return new IntegerValue.Uint16((char) r.intValue());
        if (like instanceof IntegerValue.Uint32) return new IntegerValue.Uint32(r.longValue() & 0xFFFFFFFFL);
        if (like instanceof IntegerValue.Uint64) return new IntegerValue.Uint64(r);
        if (like instanceof IntegerValue.Bigint) return new IntegerValue.Bigint(r);
        throw new IllegalStateException("unexpected IntegerValue variant: " + like);
    }

    // ===== Native-value dispatch (the statically-typed generated-code entry point) =====
    //
    // Generated code that is polymorphic over a 'numeric' type variable (e.g. a user definition
    // like  addAll := Lists.foldl Math.add ...  inferred as  numeric a => [a] -> a) compiles to a
    // generic Java method that references the primitive's static apply as a  <A> A apply(A, A)
    // function value. There is no concrete type for javac to resolve a monomorphic overload against,
    // so apply must be generic and erased; it dispatches on the operands' runtime boxed class here.
    // This mirrors equality.Compare.apply's  <A> Comparison apply(A, A)  (which recovers Comparable
    // at runtime); numeric has no java.lang.Addable analogue, so an instanceof ladder over the boxed
    // Number subtypes selects the per-type arithmetic. Native fixed-width results are NOT re-narrowed
    // (the boxed type is the width); bigint uses BigInteger for full precision.

    /**
     * Apply a numeric binary operation to two boxed native operands, dispatching on their runtime
     * class. This is the body of the generic {@code <A> A apply(A, A)} static entry points.
     */
    @SuppressWarnings("unchecked")
    public static <A> A applyNativeBinary(String opName, BinaryOp op, A a, A b) {
        if (a instanceof Double || a instanceof Float) {
            if (a instanceof Float && b instanceof Float) {
                return (A) (Float) op.applyFloat32((Float) a, (Float) b);
            }
            return (A) (Double) op.applyFloat(((Number) a).doubleValue(), ((Number) b).doubleValue());
        }
        if (a instanceof BigInteger) {
            return (A) op.applyInteger((BigInteger) a, (BigInteger) b);
        }
        if (a instanceof Number) {
            BigInteger r = op.applyInteger(
                BigInteger.valueOf(((Number) a).longValue()),
                BigInteger.valueOf(((Number) b).longValue()));
            return (A) narrowToBoxed(opName, a, r);
        }
        throw new IllegalStateException("hydra.lib.math." + opName + ": operand is not numeric: " + a);
    }

    /**
     * Apply a numeric unary operation to a boxed native operand, dispatching on its runtime class.
     */
    @SuppressWarnings("unchecked")
    public static <A> A applyNativeUnary(String opName, UnaryOp op, A a) {
        if (a instanceof Double) {
            return (A) (Double) op.applyFloat((Double) a);
        }
        if (a instanceof Float) {
            return (A) (Float) op.applyFloat32((Float) a);
        }
        if (a instanceof BigInteger) {
            return (A) op.applyInteger((BigInteger) a);
        }
        if (a instanceof Number) {
            BigInteger r = op.applyInteger(BigInteger.valueOf(((Number) a).longValue()));
            return (A) narrowToBoxed(opName, a, r);
        }
        throw new IllegalStateException("hydra.lib.math." + opName + ": operand is not numeric: " + a);
    }

    // Narrow a BigInteger result back to the boxed native type of the operand, preserving width
    // (two's-complement wraparound for the fixed-width integer types).
    private static Number narrowToBoxed(String opName, Object like, BigInteger r) {
        if (like instanceof Byte) return (byte) r.intValue();
        if (like instanceof Short) return (short) r.intValue();
        if (like instanceof Integer) return r.intValue();
        if (like instanceof Long) return r.longValue();
        throw new IllegalStateException("hydra.lib.math." + opName + ": unexpected integer box: " + like.getClass());
    }
}
