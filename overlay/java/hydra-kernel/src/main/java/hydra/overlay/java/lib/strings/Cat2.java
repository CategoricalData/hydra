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
 * Concatenates two strings.
 */
public class Cat2 extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.cat2"
     */
    public Name name() {
        return hydra.lib.Strings.cat2().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that concatenates two strings
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), string(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.string(graph, args.get(0)), l -> hydra.overlay.java.lib.eithers.Map.apply(r -> Terms.string(Cat2.apply(l, r)), hydra.extract.Core.string(graph, args.get(1))));
    }

    /**
     * Concatenates two strings.
     * @param left the first string
     * @param right the second string
     * @return the concatenation of left and right
     */
    public static String apply(String left, String right) {
        return left + right;
    }
}
