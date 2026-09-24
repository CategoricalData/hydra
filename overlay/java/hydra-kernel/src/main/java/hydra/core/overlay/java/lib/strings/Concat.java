package hydra.core.overlay.java.lib.strings;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Concatenates a list of strings into a single string.
 */
public class Concat extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.strings.concat"
     */
    public Name name() {
        return hydra.lib.Strings.concat().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that concatenates a list of strings
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
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(strings -> Terms.string(apply(strings)), hydra.core.extract.Core.listOf(t -> hydra.core.extract.Core.string(graph, t), graph, args.get(0)));
    }

    /**
     * Concatenates a list of strings into a single string.
     * @param args the list of strings to concatenate
     * @return the concatenated string
     */
    public static String apply(List<String> args) {
        return String.join("", args);
    }
}
