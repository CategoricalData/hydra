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
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;

/**
 * Retrieves the character at a given index in a string.
 */
public class CharAt extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.charAt"
     */
    public Name name() {
        return new Name("hydra.lib.strings.charAt");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that retrieves a character at an index
     */
    @Override
    public TypeScheme type() {
        return scheme(function(int32(), string(), int32()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map2(
                Expect.int32(args.get(0)),
                Expect.string(args.get(1)),
                (i, s) -> Terms.int32(apply(i, s)));
    }

    /**
     * Returns a function that retrieves the character at the given index.
     * @param index the index of the character to retrieve
     * @return a function that takes a string and returns the character code point at the index
     */
    public static Function<String, Integer> apply(Integer index) {
        return (s) -> apply(index, s);
    }

    /**
     * Retrieves the character at the given index in the string.
     * @param index the index of the character to retrieve
     * @param s the string to query
     * @return the code point of the character at the index
     */
    public static Integer apply(Integer index, String s) {
        return (int) s.charAt(index);
    }
}
