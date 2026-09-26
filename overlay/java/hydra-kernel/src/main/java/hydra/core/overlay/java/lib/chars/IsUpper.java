package hydra.core.overlay.java.lib.chars;

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
import static hydra.core.overlay.java.dsl.Types.int32;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Determines whether a character is uppercase.
 */
public class IsUpper extends PrimitiveFunction {
    /**
     * Gets the qualified name of this primitive function.
     * @return the name "hydra.core.lib.chars.isUpper"
     */
    public Name name() {
        return hydra.core.lib.Chars.isUpper().name;
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
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(c -> Terms.boolean_(apply(c)), hydra.core.extract.Model.int32(graph, args.get(0)));
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
