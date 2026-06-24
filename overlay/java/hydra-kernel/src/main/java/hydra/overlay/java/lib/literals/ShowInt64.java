package hydra.overlay.java.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.int64;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Primitive function which converts an int64 (64-bit signed integer) to its string representation.
 */
public class ShowInt64 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.showInt64"
     */
    public Name name() {
        return hydra.lib.Literals.showInt64().name;
    }

    /**
     * Returns the type scheme for this function: int64 -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(int64(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts int64 terms to string terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((Function<Long, Term>) l -> Terms.string(apply(l)), hydra.extract.Core.int64(graph, args.get(0)));
    }

    /**
     * Converts a Long (64-bit signed integer) value to its string representation.
     * @param value the Long value to convert
     * @return the string representation of the value
     */
    public static String apply(Long value) {
        return Long.toString(value);
    }
}
