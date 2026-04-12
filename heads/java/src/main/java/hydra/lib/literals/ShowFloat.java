package hydra.lib.literals;

/**
 * Formats floating-point numbers to match Hydra's standard {@code show} output.
 * <p>
 * Uses decimal notation for values with absolute value in [0.1, 9999999],
 * and scientific notation (with lowercase 'e') otherwise.
 */
class ShowFloat {
    /**
     * Format a float to match Hydra's {@code show} for Float.
     */
    static String showFloat32(float value) {
        if (Float.isNaN(value)) return "NaN";
        if (Float.isInfinite(value)) return value > 0 ? "Infinity" : "-Infinity";
        if (value == 0.0f) {
            return (Float.floatToRawIntBits(value) < 0) ? "-0.0" : "0.0";
        }

        float abs = Math.abs(value);
        if (abs >= 0.1f && abs < 1.0e7f) {
            // Java's Float.toString gives shortest decimal for this range
            return Float.toString(value);
        } else {
            return toScientific(Float.toString(value));
        }
    }

    /**
     * Format a double to match Hydra's {@code show} for Double.
     */
    static String showFloat(double value) {
        if (Double.isNaN(value)) return "NaN";
        if (Double.isInfinite(value)) return value > 0 ? "Infinity" : "-Infinity";
        if (value == 0.0) {
            return (Double.doubleToRawLongBits(value) < 0) ? "-0.0" : "0.0";
        }

        double abs = Math.abs(value);
        if (abs >= 0.1 && abs < 1.0e7) {
            return Double.toString(value);
        } else {
            return toScientific(Double.toString(value));
        }
    }

    /**
     * Convert a Java float/double string representation to scientific notation.
     * Java uses "1.0E-3", Hydra uses "1.0e-3".
     * Java uses "1.0E10", Hydra uses "1.0e10".
     * If the number is in decimal form (e.g., "0.05"), convert to scientific.
     */
    private static String toScientific(String javaStr) {
        // If already in scientific notation, just lowercase the E
        int eIdx = javaStr.indexOf('E');
        if (eIdx >= 0) {
            return javaStr.substring(0, eIdx) + "e" + javaStr.substring(eIdx + 1);
        }

        // Decimal form - need to convert to scientific notation
        boolean negative = javaStr.startsWith("-");
        String abs = negative ? javaStr.substring(1) : javaStr;

        int dot = abs.indexOf('.');
        if (dot < 0) {
            // No decimal point - shouldn't happen for float/double toString
            return javaStr;
        }

        // Remove the dot to get all digits
        String intPart = abs.substring(0, dot);
        String fracPart = abs.substring(dot + 1);
        String allDigits = intPart + fracPart;

        // Find first non-zero digit
        int firstNonZero = 0;
        while (firstNonZero < allDigits.length() && allDigits.charAt(firstNonZero) == '0') {
            firstNonZero++;
        }
        if (firstNonZero >= allDigits.length()) {
            return "0.0";
        }

        // Exponent: position of first non-zero digit relative to original decimal point
        int exponent = dot - firstNonZero - 1;

        // Mantissa: first non-zero digit, dot, remaining digits
        String significant = allDigits.substring(firstNonZero);
        // Remove trailing zeros
        while (significant.length() > 1 && significant.endsWith("0")) {
            significant = significant.substring(0, significant.length() - 1);
        }

        String mantissa;
        if (significant.length() == 1) {
            mantissa = significant + ".0";
        } else {
            mantissa = significant.charAt(0) + "." + significant.substring(1);
        }

        StringBuilder sb = new StringBuilder();
        if (negative) sb.append('-');
        sb.append(mantissa);
        sb.append('e');
        sb.append(exponent);
        return sb.toString();
    }
}
