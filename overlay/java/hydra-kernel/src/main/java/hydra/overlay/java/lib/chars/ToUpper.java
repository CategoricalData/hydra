package hydra.overlay.java.lib.chars;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.int32;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Converts a character to uppercase.
 */
public class ToUpper extends PrimitiveFunction {
    /**
     * Gets the qualified name of this primitive function.
     * @return the name "hydra.lib.chars.toUpper"
     */
    public Name name() {
        return hydra.lib.Chars.toUpper().name;
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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(c -> Terms.int32(apply(c)), hydra.extract.Core.int32(graph, args.get(0)));
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
