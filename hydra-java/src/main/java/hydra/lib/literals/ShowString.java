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
        return "\"" + escapeJava(value) + "\"";
    }

    private static String escapeJava(String str) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            switch (c) {
                case '\\': sb.append("\\\\"); break;
                case '"': sb.append("\\\""); break;
                case '\n': sb.append("\\n"); break;
                case '\r': sb.append("\\r"); break;
                case '\t': sb.append("\\t"); break;
                case '\b': sb.append("\\b"); break;
                case '\f': sb.append("\\f"); break;
                default:
                    if (c < 0x20) {
                        sb.append(String.format("\\u%04x", (int) c));
                    } else {
                        sb.append(c);
                    }
            }
        }
        return sb.toString();
    }
}
