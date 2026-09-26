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
import static hydra.core.overlay.java.dsl.Types.int32;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Returns the length of a string.
 */
public class Length extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.strings.length"
     */
    public Name name() {
        return hydra.core.lib.Strings.length().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that returns the length of a string
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), int32()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(s -> Terms.int32(apply(s)), hydra.core.extract.Model.string(graph, args.get(0)));
    }

    /**
     * Returns the length of a string.
     * @param s the string to measure
     * @return the length of the string
     */
    public static int apply(String s) {
        return s.codePointCount(0, s.length());
    }
}
