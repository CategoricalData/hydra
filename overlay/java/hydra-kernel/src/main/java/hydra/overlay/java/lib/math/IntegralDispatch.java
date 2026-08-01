package hydra.overlay.java.lib.math;

import hydra.core.IntegerValue;
import hydra.core.Literal;
import hydra.core.Term;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.util.Optional;

import java.math.BigInteger;

/**
 * Runtime dispatch for the constraint-polymorphic ('integral') division/modulus/remainder/parity
 * primitives (div/mod/rem/even/odd).
 *
 * <p>These primitives are registered with an {@code integral x => x -> x -> optional x} (or
 * {@code integral x => x -> boolean}) type scheme (the constraint is carried in the type for
 * inference only) and identity/pass-through behavior at the marshalling boundary, so the runtime
 * integer type is discovered by dispatching on the argument's literal variant. This mirrors the
 * Haskell host's {@code divTerm}/{@code modTerm}/{@code remTerm}/{@code evenTerm}/{@code oddTerm}
 * (see {@code Hydra.Overlay.Haskell.Lib.Math}) and {@link NumericDispatch}'s structural-dispatch
 * pattern. No typeclass mechanism is consulted at runtime — the host has none.
 *
 * <p>div/mod are floor-based (sign follows the divisor); rem is truncated (sign follows the
 * dividend) — computed in {@link BigInteger} (giving correct floor/truncated semantics for any
 * width) and narrowed back to the source variant's width. Both families guard the zero-divisor
 * case (returning {@code Optional.none()}) before computing.
 *
 * <p>The (minBound, -1) boundary needs no explicit guard here: this dispatch computes div/mod/rem
 * in unbounded {@link BigInteger} (to share one implementation across all widths), so the exact
 * quotient at that boundary is {@code 2^(width-1)} — one past the representable maximum for a
 * signed width-n type — and {@link #rewrapInteger} narrows it back via {@code BigInteger.intValue}
 * / {@code longValue}, which truncates to the low bits. That truncation is exactly two's-complement
 * wraparound, so {@code 2^(width-1)} narrows to {@code -2^(width-1)} (minBound) automatically,
 * matching the design contract (unlike Haskell's GHC.Int div/quot, which throw there and need an
 * explicit guard — see {@code Hydra.Overlay.Haskell.Lib.Math.maybeDivBounded}).
 */
public final class IntegralDispatch {
    private IntegralDispatch() {
    }

    /**
     * Apply floor division to two integer terms, dispatching on the runtime integer variant.
     */
    public static Term div(Term x, Term y) {
        IntegerValue ix = integralLiteral("div", x);
        IntegerValue iy = integralLiteral("div", y);
        return Terms.optional(divModRem(ix, iy, DivKind.DIV));
    }

    /**
     * Apply floor modulus to two integer terms, dispatching on the runtime integer variant.
     */
    public static Term mod(Term x, Term y) {
        IntegerValue ix = integralLiteral("mod", x);
        IntegerValue iy = integralLiteral("mod", y);
        return Terms.optional(divModRem(ix, iy, DivKind.MOD));
    }

    /**
     * Apply truncated remainder to two integer terms, dispatching on the runtime integer variant.
     */
    public static Term rem(Term x, Term y) {
        IntegerValue ix = integralLiteral("rem", x);
        IntegerValue iy = integralLiteral("rem", y);
        return Terms.optional(divModRem(ix, iy, DivKind.REM));
    }

    /**
     * Apply a parity test to an integer term, dispatching on the runtime integer variant.
     */
    public static Boolean even(Term x) {
        return !toBigInteger(integralLiteral("even", x)).testBit(0);
    }

    /**
     * Apply a parity test (odd) to an integer term, dispatching on the runtime integer variant.
     */
    public static Boolean odd(Term x) {
        return toBigInteger(integralLiteral("odd", x)).testBit(0);
    }

    private enum DivKind { DIV, MOD, REM }

    private static Optional<Term> divModRem(IntegerValue ix, IntegerValue iy, DivKind kind) {
        if (ix.getClass() != iy.getClass()) {
            throw new IllegalStateException("hydra.lib.math." + kind + ": integer operands differ in precision");
        }
        BigInteger x = toBigInteger(ix);
        BigInteger y = toBigInteger(iy);
        Optional<BigInteger> result = divModRemBig(x, y, kind);
        return result.map(r -> Terms.integer(rewrapInteger(ix, r)));
    }

    // Pure BigInteger-level div/mod/rem, shared by the Term-path (divModRem) and the native-value
    // path (applyNative*) so the floor/truncate arithmetic is written once.
    private static Optional<BigInteger> divModRemBig(BigInteger x, BigInteger y, DivKind kind) {
        if (y.signum() == 0) {
            return Optional.none();
        }
        switch (kind) {
            case DIV: {
                BigInteger[] qr = x.divideAndRemainder(y);
                BigInteger q = qr[0];
                // BigInteger divideAndRemainder truncates; floor-adjust when signs differ and
                // there's a nonzero remainder.
                if (qr[1].signum() != 0 && (qr[1].signum() != y.signum())) {
                    q = q.subtract(BigInteger.ONE);
                }
                return Optional.given(q);
            }
            case MOD: {
                BigInteger r = x.remainder(y);
                if (r.signum() != 0 && (r.signum() != y.signum())) {
                    r = r.add(y);
                }
                return Optional.given(r);
            }
            case REM:
                return Optional.given(x.remainder(y));
            default:
                throw new IllegalStateException("unreachable");
        }
    }

    private static IntegerValue integralLiteral(String opName, Term term) {
        if (term instanceof Term.Literal && ((Term.Literal) term).value instanceof Literal.Integer_) {
            return ((Literal.Integer_) ((Term.Literal) term).value).value;
        }
        throw new IllegalStateException("hydra.lib.math." + opName + ": expected an integer literal term");
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

    /**
     * Apply floor division to two boxed native integer operands, dispatching on their runtime
     * class. This is the body of the generic {@code <A> Optional<A> apply(A, A)} static entry
     * points.
     */
    public static <A> Optional<A> applyNativeDiv(A x, A y) {
        return applyNative("div", x, y, DivKind.DIV);
    }

    /**
     * Apply floor modulus to two boxed native integer operands, dispatching on their runtime
     * class.
     */
    public static <A> Optional<A> applyNativeMod(A x, A y) {
        return applyNative("mod", x, y, DivKind.MOD);
    }

    /**
     * Apply truncated remainder to two boxed native integer operands, dispatching on their
     * runtime class.
     */
    public static <A> Optional<A> applyNativeRem(A x, A y) {
        return applyNative("rem", x, y, DivKind.REM);
    }

    @SuppressWarnings("unchecked")
    private static <A> Optional<A> applyNative(String opName, A x, A y, DivKind kind) {
        if (x instanceof BigInteger) {
            return (Optional<A>) divModRemBig((BigInteger) x, (BigInteger) y, kind);
        }
        if (x instanceof Number) {
            BigInteger bx = BigInteger.valueOf(((Number) x).longValue());
            BigInteger by = BigInteger.valueOf(((Number) y).longValue());
            return divModRemBig(bx, by, kind).map(r -> (A) narrowToBoxed(opName, x, r));
        }
        throw new IllegalStateException("hydra.lib.math." + opName + ": operand is not integral: " + x);
    }

    /**
     * Apply a parity test to a boxed native integer operand, dispatching on its runtime class.
     */
    public static Boolean applyNativeEven(Object x) {
        return !toBigIntegerNative(x).testBit(0);
    }

    /**
     * Apply a parity test (odd) to a boxed native integer operand, dispatching on its runtime
     * class.
     */
    public static Boolean applyNativeOdd(Object x) {
        return toBigIntegerNative(x).testBit(0);
    }

    private static BigInteger toBigIntegerNative(Object x) {
        if (x instanceof BigInteger) return (BigInteger) x;
        if (x instanceof Number) return BigInteger.valueOf(((Number) x).longValue());
        throw new IllegalStateException("hydra.lib.math.even/odd: operand is not integral: " + x);
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
