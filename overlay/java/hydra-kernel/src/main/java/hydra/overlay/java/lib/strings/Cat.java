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
 * Concatenates a list of strings into a single string.
 */
public class Cat extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.cat"
     */
    public Name name() {
        return hydra.lib.Strings.cat().name;
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
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(strings -> Terms.string(apply(strings)), hydra.extract.Core.listOf(t -> hydra.extract.Core.string(graph, t), graph, args.get(0)));
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
