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
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Converts a string to lowercase.
 */
public class ToLower extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.toLower"
     */
    public Name name() {
        return hydra.lib.Strings.toLower().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that converts a string to lowercase
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(s -> Terms.string(apply(s)), hydra.extract.Core.string(graph, args.get(0)));
    }

    /**
     * Converts a string to lowercase.
     * @param upper the string to convert
     * @return the lowercase version of the string
     */
    public static String apply(String upper) {
        // TODO: Java's built-in behavior may not agree with that of Haskell or other host languages
        return upper.toLowerCase();
    }
}
