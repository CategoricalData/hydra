package hydra.core.overlay.java.lib.literals;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

/**
 * Shortest round-trip decimal rendering of floats and doubles, independent of the JDK version.
 * <p>
 * {@link Float#toString(float)} and {@link Double#toString(double)} only guarantee the shortest
 * round-trip digits from JDK 19 on (JDK-4511638); earlier JDKs sometimes emit an extra digit
 * (e.g. JDK 11 and 17 render {@code 2.8147494e14f} as {@code 2.81474943E14}). The output of a
 * Hydra host must not depend on the JDK it runs on, so the literal primitives use this instead.
 * The strings have exactly the {@code Float.toString}/{@code Double.toString} format; only the
 * digits differ, and only on the JDKs that got them wrong.
 */
class ShortestDecimal {
    private ShortestDecimal() {
    }

    /**
     * Render a finite float in {@code Float.toString} format with the shortest round-trip digits.
     */
    static String toJavaString(float value) {
        if (Float.isNaN(value) || Float.isInfinite(value) || value == 0.0f) {
            return Float.toString(value);
        }
        BigDecimal exact = new BigDecimal(value);
        for (int precision = 1; precision < 9; precision++) {
            BigDecimal best = null;
            for (RoundingMode mode : MODES) {
                BigDecimal candidate = exact.round(new MathContext(precision, mode));
                if (candidate.floatValue() == value && closer(candidate, best, exact)) {
                    best = candidate;
                }
            }
            if (best != null) {
                return format(best, Math.abs(value) >= 1e-3f && Math.abs(value) < 1e7f);
            }
        }
        return format(exact.round(new MathContext(9, RoundingMode.HALF_EVEN)),
            Math.abs(value) >= 1e-3f && Math.abs(value) < 1e7f);
    }

    /**
     * Render a finite double in {@code Double.toString} format with the shortest round-trip digits.
     */
    static String toJavaString(double value) {
        if (Double.isNaN(value) || Double.isInfinite(value) || value == 0.0) {
            return Double.toString(value);
        }
        BigDecimal exact = new BigDecimal(value);
        for (int precision = 1; precision < 17; precision++) {
            BigDecimal best = null;
            for (RoundingMode mode : MODES) {
                BigDecimal candidate = exact.round(new MathContext(precision, mode));
                if (candidate.doubleValue() == value && closer(candidate, best, exact)) {
                    best = candidate;
                }
            }
            if (best != null) {
                return format(best, Math.abs(value) >= 1e-3 && Math.abs(value) < 1e7);
            }
        }
        return format(exact.round(new MathContext(17, RoundingMode.HALF_EVEN)),
            Math.abs(value) >= 1e-3 && Math.abs(value) < 1e7);
    }

    // HALF_EVEN gives the nearest decimal at a precision; CEILING and FLOOR cover the binade
    // boundaries, where the round-trip interval is asymmetric and the nearest decimal can fall
    // outside it while its neighbor does not.
    private static final RoundingMode[] MODES = {RoundingMode.HALF_EVEN, RoundingMode.CEILING, RoundingMode.FLOOR};

    private static boolean closer(BigDecimal candidate, BigDecimal best, BigDecimal exact) {
        return best == null
            || candidate.subtract(exact).abs().compareTo(best.subtract(exact).abs()) < 0;
    }

    /**
     * Lay out the digits of a nonzero decimal the way Float.toString/Double.toString do:
     * plain notation with at least one fractional digit, or "d.ddddE[-]n".
     */
    private static String format(BigDecimal d, boolean plain) {
        BigDecimal stripped = d.stripTrailingZeros();
        String digits = stripped.unscaledValue().abs().toString();
        int exponent = digits.length() - 1 - stripped.scale();
        StringBuilder sb = new StringBuilder();
        if (stripped.signum() < 0) {
            sb.append('-');
        }
        if (plain) {
            if (exponent >= 0) {
                if (digits.length() <= exponent + 1) {
                    sb.append(digits);
                    for (int i = digits.length(); i <= exponent; i++) {
                        sb.append('0');
                    }
                    sb.append(".0");
                } else {
                    sb.append(digits, 0, exponent + 1).append('.').append(digits, exponent + 1, digits.length());
                }
            } else {
                sb.append("0.");
                for (int i = -1; i > exponent; i--) {
                    sb.append('0');
                }
                sb.append(digits);
            }
        } else {
            sb.append(digits.charAt(0)).append('.');
            sb.append(digits.length() > 1 ? digits.substring(1) : "0");
            sb.append('E').append(exponent);
        }
        return sb.toString();
    }
}
