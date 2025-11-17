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

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;

/**
 * Converts a string to lowercase.
 */
public class ToLower extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.toLower"
     */
    public Name name() {
        return new Name("hydra.lib.strings.toLower");
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
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)), s -> Terms.string(apply(s)));
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
