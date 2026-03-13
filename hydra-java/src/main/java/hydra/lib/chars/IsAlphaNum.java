package hydra.lib.chars;

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
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;

/**
 * Determines whether a character is alphanumeric (a letter or digit).
 */
public class IsAlphaNum extends PrimitiveFunction {
    /**
     * Gets the qualified name of this primitive function.
     * @return the name "hydra.lib.chars.isAlphaNum"
     */
    public Name name() {
        return new Name("hydra.lib.chars.isAlphaNum");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(c -> Terms.boolean_(apply(c)), hydra.extract.core.Core.int32(cx, graph, args.get(0)));
    }

    /**
     * Checks whether the given code point is alphanumeric.
     * @param codePoint the Unicode code point to test
     * @return true if the code point represents a letter or digit, false otherwise
     */
    public static boolean apply(int codePoint) {
        return Character.isLetterOrDigit(codePoint);
    }
}
