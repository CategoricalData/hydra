package hydra.core.overlay.java.lib.chars;

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
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Converts a character to lowercase.
 */
public class ToLower extends PrimitiveFunction {
    /**
     * Gets the qualified name of this primitive function.
     * @return the name "hydra.core.lib.chars.toLower"
     */
    public Name name() {
        return hydra.core.lib.Chars.toLower().name;
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
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(c -> Terms.int32(apply(c)), hydra.core.extract.Model.int32(graph, args.get(0)));
    }

    /**
     * Converts the given code point to lowercase.
     * @param codePoint the Unicode code point to convert
     * @return the lowercase equivalent of the code point
     */
    public static int apply(int codePoint) {
        return Character.toLowerCase(codePoint);
    }
}
