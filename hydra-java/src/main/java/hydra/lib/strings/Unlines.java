package hydra.lib.strings;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
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
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;

/**
 * Joins a list of strings with newlines and appends a final newline.
 */
public class Unlines extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.unlines"
     */
    public Name name() {
        return new Name("hydra.lib.strings.unlines");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(lines -> Terms.string(apply(lines)), hydra.extract.core.Core.listOf(cx, t -> hydra.extract.core.Core.string(cx, graph, t), graph, args.get(0)));
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
