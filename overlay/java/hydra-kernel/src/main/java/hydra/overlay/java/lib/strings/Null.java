package hydra.overlay.java.lib.strings;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.boolean_;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Determines whether a string is empty (null check).
 */
public class Null extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.null"
     */
    public Name name() {
        return hydra.lib.Strings.null_().name;
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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(s -> Terms.boolean_(apply(s)), hydra.extract.Core.string(graph, args.get(0)));
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
