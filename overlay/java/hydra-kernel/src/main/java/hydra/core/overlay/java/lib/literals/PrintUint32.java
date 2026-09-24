package hydra.core.overlay.java.lib.literals;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import static hydra.core.overlay.java.dsl.Types.uint32;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Primitive function which converts a uint32 (32-bit unsigned integer) to its string representation.
 */
public class PrintUint32 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.core.lib.literals.printUint32"
     */
    public Name name() {
        return hydra.lib.Literals.printUint32().name;
    }

    /**
     * Returns the type scheme for this function: uint32 -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(uint32(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts uint32 terms to string terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<Long, Term>) l -> Terms.string(apply(l)), hydra.core.extract.Core.uint32(graph, args.get(0)));
    }

    /**
     * Converts a Long (used to represent 32-bit unsigned integer) to its string representation.
     * @param value the Long value to convert
     * @return the string representation of the value
     */
    public static String apply(Long value) {
        return Long.toString(value);
    }
}
