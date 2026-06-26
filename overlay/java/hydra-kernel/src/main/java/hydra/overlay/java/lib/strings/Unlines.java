package hydra.overlay.java.lib.strings;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Joins a list of strings with newlines and appends a final newline.
 */
public class Unlines extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.unlines"
     */
    public Name name() {
        return hydra.lib.Strings.unlines().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that joins strings with newlines
     */
    @Override
    public TypeScheme type() {
        return scheme(function(list(string()), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(lines -> Terms.string(apply(lines)), hydra.extract.Core.listOf(t -> hydra.extract.Core.string(graph, t), graph, args.get(0)));
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
