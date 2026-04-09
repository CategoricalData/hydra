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
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Determines whether a string is empty (null check).
 */
public class Null extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.null"
     */
    public Name name() {
        return new Name("hydra.lib.strings.null");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that checks if a string is null/empty
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(s -> Terms.boolean_(apply(s)), hydra.extract.Core.string(graph, args.get(0)));
    }

    /**
     * Checks whether a string is empty.
     * @param s the string to test
     * @return true if the string is empty, false otherwise
     */
    public static Boolean apply(String s) {
        return s.isEmpty();
    }
}
