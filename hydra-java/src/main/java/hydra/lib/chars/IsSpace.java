package hydra.lib.chars;

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

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;

/**
 * Determines whether a character is whitespace.
 */
public class IsSpace extends PrimitiveFunction {
    /**
     * Gets the qualified name of this primitive function.
     * @return the name "hydra.lib.chars.isSpace"
     */
    public Name name() {
        return new Name("hydra.lib.chars.isSpace");
    }

    /**
     * Gets the type scheme for this function.
     * @return the type scheme representing int32 to boolean function
     */
    @Override
    public TypeScheme type() {
        return scheme(function(int32(), boolean_()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that takes a list of terms and returns a flow producing a boolean term
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(
                Expect.int32(args.get(0)),
                c -> Terms.boolean_(apply(c)));
    }

    /**
     * Checks whether the given code point is whitespace.
     * @param codePoint the Unicode code point to test
     * @return true if the code point is whitespace, false otherwise
     */
    public static boolean apply(int codePoint) {
        return Character.isWhitespace(codePoint);
    }
}
