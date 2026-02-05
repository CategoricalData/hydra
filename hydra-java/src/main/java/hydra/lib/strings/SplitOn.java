package hydra.lib.strings;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.dsl.Flows.map2;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;

/**
 * Splits a string on a given delimiter.
 */
public class SplitOn extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.splitOn"
     */
    public Name name() {
        return new Name("hydra.lib.strings.splitOn");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that splits a string on a delimiter
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), string(), list(string())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map2(Expect.string(args.get(0)), Expect.string(args.get(1)),
            (BiFunction<String, String, Term>) (s, s2) -> Terms.listOfStrings(apply(s, s2)));
    }

    /**
     * Returns a function that splits strings on the given delimiter.
     * @param delim the delimiter string
     * @return a function that takes a string and returns the split result
     */
    public static Function<String, List<String>> apply(String delim) {
        return (string) -> apply(delim, string);
    }

    /**
     * Splits a string on a delimiter.
     * Note: the delimiter is not interpreted as a regular expression;
     * it is simply a literal string. See Haskell's Data.List.Split.
     * @param delim the delimiter string
     * @param string the string to split
     * @return the list of substrings
     */
    public static List<String> apply(String delim, String string) {
        List<String> parts = new ArrayList<>();

        if (delim.isEmpty()) {
            parts.add("");
            int i = 0;
            while (i < string.length()) {
                int cp = string.codePointAt(i);
                int charCount = Character.charCount(cp);
                parts.add(string.substring(i, i + charCount));
                i += charCount;
            }
        } else {
            int k = 0;
            int delimLen = delim.length();
            while (true) {
                int idx = string.indexOf(delim, k);
                if (idx < 0) {
                    break;
                }
                parts.add(string.substring(k, idx));
                k = idx + delimLen;
            }
            parts.add(string.substring(k));
        }

        return parts;
    }
}
