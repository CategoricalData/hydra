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
 * Determines whether a character is uppercase.
 */
public class IsUpper extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.chars.isUpper");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), boolean_()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(
                Expect.int32(args.get(0)),
                c -> Terms.boolean_(apply(c)));
    }

    /**
     * Checks whether the given code point is uppercase.
     * @param codePoint the Unicode code point to test
     * @return true if the code point is uppercase, false otherwise
     */
    public static boolean apply(int codePoint) {
        return Character.isUpperCase(codePoint);
    }
}
