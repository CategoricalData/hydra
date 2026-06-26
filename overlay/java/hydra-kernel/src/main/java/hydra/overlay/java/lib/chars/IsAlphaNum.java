package hydra.overlay.java.lib.chars;

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
import static hydra.overlay.java.dsl.Types.int32;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Determines whether a character is alphanumeric (a letter or digit).
 */
public class IsAlphaNum extends PrimitiveFunction {
    /**
     * Gets the qualified name of this primitive function.
     * @return the name "hydra.lib.chars.isAlphaNum"
     */
    public Name name() {
        return hydra.lib.Chars.isAlphaNum().name;
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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(c -> Terms.boolean_(apply(c)), hydra.extract.Core.int32(graph, args.get(0)));
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
