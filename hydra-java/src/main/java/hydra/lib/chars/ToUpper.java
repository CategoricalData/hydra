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

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;

/**
 * Converts a character to uppercase.
 */
public class ToUpper extends PrimitiveFunction {
    /**
     * Gets the qualified name of this primitive function.
     * @return the name "hydra.lib.chars.toUpper"
     */
    public Name name() {
        return new Name("hydra.lib.chars.toUpper");
    }

    /**
     * Gets the type scheme for this function.
     * @return the type scheme representing int32 to int32 function
     */
    @Override
    public TypeScheme type() {
        return scheme(function(int32(), int32()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that takes a list of terms and returns a flow producing an int32 term
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(
                Expect.int32(args.get(0)),
                c -> Terms.int32(apply(c)));
    }

    /**
     * Converts the given code point to uppercase.
     * @param codePoint the Unicode code point to convert
     * @return the uppercase equivalent of the code point
     */
    public static int apply(int codePoint) {
        return Character.toUpperCase(codePoint);
    }
}
