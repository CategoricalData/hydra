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
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Converts a string to uppercase.
 */
public class ToUpper extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.toUpper"
     */
    public Name name() {
        return new Name("hydra.lib.strings.toUpper");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that converts a string to uppercase
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(s -> Terms.string(apply(s)), hydra.extract.core.Core.string(cx, graph, args.get(0)));
    }

    /**
     * Converts a string to uppercase.
     * @param lower the string to convert
     * @return the uppercase version of the string
     */
    public static String apply(String lower) {
        // TODO: Java's built-in behavior may not agree with that of Haskell or other host languages
        return lower.toUpperCase();
    }
}
