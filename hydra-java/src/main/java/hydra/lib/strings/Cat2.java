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
import hydra.error.OtherError;
import hydra.util.Either;

/**
 * Concatenates two strings.
 */
public class Cat2 extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.cat2"
     */
    public Name name() {
        return new Name("hydra.lib.strings.cat2");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.string(cx, graph, args.get(0)), l -> hydra.lib.eithers.Map.apply(r -> Terms.string(Cat2.apply(l, r)), hydra.extract.core.Core.string(cx, graph, args.get(1))));
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
