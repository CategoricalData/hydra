package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.math.BigDecimal;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.decimal;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Primitive function which converts a decimal (arbitrary-precision exact decimal) to its string representation.
 */
public class ShowDecimal extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.showDecimal"
     */
    public Name name() {
        return new Name("hydra.lib.literals.showDecimal");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<BigDecimal, Term>) d -> Terms.string(apply(d)), hydra.extract.Core.decimal(graph, args.get(0)));
    }

    /**
     * Converts a BigDecimal value to its string representation, matching Haskell's
     * Data.Scientific show format (e.g. "42.0", "3.14", "1.0e20", "1.0e-10").
     * @param value the BigDecimal value to convert
     * @return the string representation of the value
     */
    public static String apply(BigDecimal value) {
        // Canonicalize to Scientific-style representation.
        // Strip trailing zeros and then format either in decimal or scientific notation
        // depending on the exponent, matching Haskell's Data.Scientific show.
        BigDecimal stripped = value.stripTrailingZeros();
        int scale = stripped.scale();
        int precision = stripped.precision();
        // coefficient-exponent form: stripped = coefficient * 10^(-scale)
        // Haskell Scientific "e" = precision - scale - 1 for the canonical
        // single-digit-before-decimal representation.
        // Haskell's threshold for plain form is -1 <= e <= 6 (values like 0.1, 1.0,
        // 10.0, ..., 1000000.0). Outside that range, use scientific notation.
        int e = precision - scale - 1;
        if (e >= 7 || e < -1) {
            // Scientific notation: "c.dddeE"
            String plain = stripped.unscaledValue().abs().toString();
            String sign = stripped.signum() < 0 ? "-" : "";
            String mantissa;
            if (plain.length() == 1) {
                mantissa = plain + ".0";
            } else {
                mantissa = plain.charAt(0) + "." + plain.substring(1);
            }
            return sign + mantissa + "e" + e;
        } else {
            // Decimal notation: force at least one digit after the decimal point
            String plain = stripped.toPlainString();
            if (!plain.contains(".")) {
                plain = plain + ".0";
            }
            return plain;
        }
    }
}
