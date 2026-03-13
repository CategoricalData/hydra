package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import java.util.List;
import java.util.function.Function;


import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;

/**
 * Primitive function which converts a string to its quoted string literal representation.
 * Escapes special characters and surrounds the string with double quotes.
 */
public class ShowString extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.showString"
     */
    public Name name() {
        return new Name("hydra.lib.literals.showString");
    }

    /**
     * Returns the type scheme for this function: string -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts string terms to quoted string literal terms
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<String, Term>) s -> Terms.string(apply(s)), hydra.extract.core.Core.string(cx, graph, args.get(0)));
    }

    /**
     * Converts a string to its quoted and escaped literal representation.
     * @param value the string value to convert
     * @return the quoted and escaped string literal
     */
    public static String apply(String value) {
        return "\"" + escapeHaskell(value) + "\"";
    }

    // ASCII control character names matching Haskell's show for Char
    private static final String[] ASCII_CONTROL_NAMES = {
        "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "a",
        "b",   "t",   "n",   "v",   "f",   "r",   "SO",  "SI",
        "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
        "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US"
    };

    /**
     * Escapes a string following Haskell's `show` semantics for String.
     * <ul>
     *   <li>Control chars 0-31 use Haskell named escapes (\NUL, \a, \n, etc.)</li>
     *   <li>DEL (127) becomes \DEL</li>
     *   <li>Backslash and double quote are escaped</li>
     *   <li>Characters above 127 are escaped as decimal codes (\233, \955, etc.)</li>
     *   <li>A \&amp; gap is inserted after a numeric escape when the next char is a digit</li>
     * </ul>
     */
    private static String escapeHaskell(String str) {
        StringBuilder sb = new StringBuilder();
        boolean lastWasNumericEscape = false;
        // Iterate over Unicode code points (not UTF-16 code units) so that
        // characters outside the Basic Multilingual Plane (e.g. emoji) are
        // emitted as a single \NNNNNN escape rather than a surrogate pair.
        for (int i = 0; i < str.length(); ) {
            int cp = str.codePointAt(i);
            i += Character.charCount(cp);
            if (lastWasNumericEscape && cp >= '0' && cp <= '9') {
                sb.append("\\&");
            }
            lastWasNumericEscape = false;
            if (cp == '\\') {
                sb.append("\\\\");
            } else if (cp == '"') {
                sb.append("\\\"");
            } else if (cp < 0x20) {
                sb.append('\\');
                sb.append(ASCII_CONTROL_NAMES[cp]);
            } else if (cp == 0x7F) {
                sb.append("\\DEL");
            } else if (cp > 0x7F) {
                sb.append('\\');
                sb.append(cp);
                lastWasNumericEscape = true;
            } else {
                sb.appendCodePoint(cp);
            }
        }
        return sb.toString();
    }
}
