package hydra.lib.strings;

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
import java.util.stream.Collectors;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;

/**
 * Joins a list of strings with newlines and appends a final newline.
 */
public class Unlines extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.strings.unlines");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(list(string()), string()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(
                Expect.list(Expect::string, args.get(0)),
                lines -> Terms.string(apply(lines)));
    }

    /**
     * Joins a list of strings with newlines and appends a final newline.
     * @param lines the list of strings to join
     * @return the joined string with newlines
     */
    public static String apply(List<String> lines) {
        return lines.stream().collect(Collectors.joining("\n")) + (lines.isEmpty() ? "" : "\n");
    }
}
