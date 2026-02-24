package hydra.lib.literals;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import java.util.List;
import java.util.function.Function;


import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;

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
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)), (Function<String, Term>) s -> Terms.string(apply(s)));
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
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            if (lastWasNumericEscape && c >= '0' && c <= '9') {
                sb.append("\\&");
            }
            lastWasNumericEscape = false;
            if (c == '\\') {
                sb.append("\\\\");
            } else if (c == '"') {
                sb.append("\\\"");
            } else if (c < 0x20) {
                sb.append('\\');
                sb.append(ASCII_CONTROL_NAMES[c]);
            } else if (c == 0x7F) {
                sb.append("\\DEL");
            } else if (c > 0x7F) {
                sb.append('\\');
                sb.append((int) c);
                lastWasNumericEscape = true;
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }
}
