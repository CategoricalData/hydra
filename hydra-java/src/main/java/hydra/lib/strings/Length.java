package hydra.lib.strings;

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

import static hydra.dsl.Flows.map;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;

/**
 * Returns the length of a string.
 */
public class Length extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.length"
     */
    public Name name() {
        return new Name("hydra.lib.strings.length");
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
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map(Expect.string(args.get(0)), s -> Terms.int32(apply(s)));
    }

    /**
     * Returns the length of a string.
     * @param s the string to measure
     * @return the length of the string
     */
    public static int apply(String s) {
        return s.length();
    }
}
