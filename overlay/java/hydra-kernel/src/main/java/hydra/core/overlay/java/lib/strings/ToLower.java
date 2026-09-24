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
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Converts a string to lowercase.
 */
public class ToLower extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.strings.toLower"
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
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(s -> Terms.string(apply(s)), hydra.core.extract.Core.string(graph, args.get(0)));
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
