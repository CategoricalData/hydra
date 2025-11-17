package hydra.lib.strings;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import static hydra.dsl.Flows.map;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;

/**
 * Splits a string into lines by breaking at newline characters.
 */
public class Lines extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.lines"
     */
    public Name name() {
        return new Name("hydra.lib.strings.lines");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that splits a string into lines
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), list(string())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map(Expect.string(args.get(0)),
            s -> Terms.list(apply(s).stream().map(Terms::string).collect(Collectors.toList())));
    }

    /**
     * Splits a string into lines by breaking at newline characters.
     * @param s the string to split
     * @return the list of lines
     */
    public static List<String> apply(String s) {
        if (s.isEmpty()) {
            return Arrays.asList();
        }
        String[] parts = s.split("\\n", -1);
        // Remove trailing empty string if the string ends with newline
        if (parts.length > 0 && parts[parts.length - 1].isEmpty()) {
            return Arrays.asList(Arrays.copyOf(parts, parts.length - 1));
        }
        return Arrays.asList(parts);
    }
}
