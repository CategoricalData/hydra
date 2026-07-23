package hydra.overlay.java.lib.strings;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Joins a list of strings with a delimiter string.
 */
public class Join extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.join"
     */
    public Name name() {
        return hydra.lib.Strings.join().name;
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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.string(graph, args.get(0)), delim -> hydra.overlay.java.lib.eithers.Map.apply(strings -> Terms.string(Join.apply(delim, strings)), hydra.extract.Core.listOf(t -> hydra.extract.Core.string(graph, t), graph, args.get(1))));
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
