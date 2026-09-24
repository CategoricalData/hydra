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
 * Concatenates two strings.
 */
public class Concat2 extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.strings.concat2"
     */
    public Name name() {
        return hydra.lib.Strings.concat2().name;
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
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Core.string(graph, args.get(0)), l -> hydra.core.overlay.java.lib.eithers.Map.apply(r -> Terms.string(Concat2.apply(l, r)), hydra.core.extract.Core.string(graph, args.get(1))));
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
