package hydra.overlay.java.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.math.BigDecimal;
import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.decimal;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Primitive function which converts a decimal (arbitrary-precision exact decimal) to its string representation.
 */
public class PrintDecimal extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.printDecimal"
     */
    public Name name() {
        return hydra.lib.Literals.printDecimal().name;
    }

    /**
     * Returns the type scheme for this function: decimal -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(decimal(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts decimal terms to string terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((Function<BigDecimal, Term>) d -> Terms.string(apply(d)), hydra.extract.Core.decimal(graph, args.get(0)));
    }

    /**
     * Converts a BigDecimal value to its representation-faithful string, per
     * docs/specification/json-format.md (Decimal formatting) and syntax.md (2.6):
     * coefficient digits -- trailing zeros included -- are preserved exactly, and
     * zero prints per its scale ("0", "0.0", "0.00"). Layout follows ECMAScript
     * Number::toString / RFC 8785: positional form when the adjusted exponent a
     * satisfies -6 &lt;= a &lt; 21, exponent form otherwise (one digit before the
     * point, lowercase e, always-signed exponent, coefficient digits preserved).
     * @param value the BigDecimal value to convert
     * @return the string representation of the value
     */
    public static String apply(BigDecimal value) {
        int scale = value.scale();
        String digits = value.unscaledValue().abs().toString();
        int n = digits.length();
        String sign = value.signum() < 0 ? "-" : "";
        // Adjusted exponent: position of the leading significant digit.
        int a = n - 1 - scale;
        if (a >= -6 && a < 21) {
            return sign + positional(digits, scale, n);
        } else {
            String mantissa = n == 1
                ? digits + ".0"
                : digits.charAt(0) + "." + digits.substring(1);
            return sign + mantissa + "e" + (a >= 0 ? "+" : "") + a;
        }
    }

    /**
     * Places the decimal point {@code scale} digits from the right, padding
     * with zeros on either side as needed.
     */
    private static String positional(String digits, int scale, int n) {
        if (scale <= 0) {
            return digits + "0".repeat(-scale);
        } else if (scale < n) {
            return digits.substring(0, n - scale) + "." + digits.substring(n - scale);
        } else {
            return "0." + "0".repeat(scale - n) + digits;
        }
    }
}
