package hydra.lib.strings;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;

/**
 * Determines whether a string is empty.
 */
public class IsEmpty extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.isEmpty"
     */
    public Name name() {
        return new Name("hydra.lib.strings.isEmpty");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that checks if a string is empty
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), boolean_()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(s -> Terms.boolean_(apply(s)), hydra.extract.core.Core.string(cx, graph, args.get(0)));
    }

    /**
     * Checks whether a string is empty.
     * @param s the string to test
     * @return true if the string is empty, false otherwise
     */
    public static boolean apply(String s) {
        return s.isEmpty();
    }
}
