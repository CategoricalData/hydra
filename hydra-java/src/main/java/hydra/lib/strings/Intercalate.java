package hydra.lib.strings;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;

/**
 * Joins a list of strings with a delimiter string.
 */
public class Intercalate extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.intercalate"
     */
    public Name name() {
        return new Name("hydra.lib.strings.intercalate");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that joins strings with a delimiter
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), list(string()), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.string(cx, graph, args.get(0)), delim -> hydra.lib.eithers.Map.apply(strings -> Terms.string(Intercalate.apply(delim, strings)), hydra.extract.core.Core.listOf(cx, t -> hydra.extract.core.Core.string(cx, graph, t), graph, args.get(1))));
    }

    /**
     * Returns a function that joins strings with the given delimiter.
     * @param delim the delimiter string
     * @return a function that takes a list of strings and returns the joined result
     */
    public static Function<List<String>, String> apply(String delim) {
        return (strings) -> apply(delim, strings);
    }

    /**
     * Joins a list of strings with a delimiter.
     * @param delim the delimiter string
     * @param strings the list of strings to join
     * @return the joined string
     */
    public static String apply(String delim, List<String> strings) {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (String s : strings) {
            if (first) {
                first = false;
            } else {
                sb.append(delim);
            }
            sb.append(s);
        }
        return sb.toString();
    }
}
