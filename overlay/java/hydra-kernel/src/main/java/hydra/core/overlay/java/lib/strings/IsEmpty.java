package hydra.core.overlay.java.lib.strings;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.boolean_;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Determines whether a string is empty.
 */
public class IsEmpty extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.strings.isEmpty"
     */
    public Name name() {
        return hydra.lib.Strings.isEmpty().name;
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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(s -> Terms.boolean_(apply(s)), hydra.core.extract.Core.string(graph, args.get(0)));
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
