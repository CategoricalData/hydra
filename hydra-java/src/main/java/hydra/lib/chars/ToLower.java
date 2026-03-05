package hydra.lib.chars;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;

/**
 * Converts a character to lowercase.
 */
public class ToLower extends PrimitiveFunction {
    /**
     * Gets the qualified name of this primitive function.
     * @return the name "hydra.lib.chars.toLower"
     */
    public Name name() {
        return new Name("hydra.lib.chars.toLower");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(c -> Terms.int32(apply(c)), hydra.extract.core.Core.int32(cx, graph, args.get(0)));
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
