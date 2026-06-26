package hydra.overlay.java.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Primitive function which converts a bigint (arbitrary-precision integer) to an int16 (16-bit signed integer).
 * This conversion may result in loss of precision if the value is outside the int16 range.
 */
public class BigintToInt16 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.bigintToInt16"
     */
    public Name name() {
        return hydra.lib.Literals.bigintToInt16().name;
    }

    /**
     * Returns the type scheme for this function: bigint -&gt; int16.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.bigint(), Types.int16()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts bigint terms to int16 terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(s -> Terms.int16(apply(s)), hydra.extract.Core.bigint(graph, args.get(0)));
    }

    /**
     * Converts a BigInteger value to a Short (16-bit signed integer).
     * @param value the BigInteger value to convert
     * @return the Short representation of the value
     */
    public static Short apply(BigInteger value) {
        return value.shortValue();
    }
}
