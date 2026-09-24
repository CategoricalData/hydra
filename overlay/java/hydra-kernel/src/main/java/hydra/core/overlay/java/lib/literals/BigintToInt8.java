package hydra.core.overlay.java.lib.literals;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Primitive function which converts a bigint (arbitrary-precision integer) to an int8 (8-bit signed integer).
 * This conversion may result in loss of precision if the value is outside the int8 range.
 */
public class BigintToInt8 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.core.lib.literals.bigintToInt8"
     */
    public Name name() {
        return hydra.lib.Literals.bigintToInt8().name;
    }

    /**
     * Returns the type scheme for this function: bigint -&gt; int8.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.bigint(), Types.int8()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts bigint terms to int8 terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(s -> Terms.int8(apply(s)), hydra.core.extract.Core.bigint(graph, args.get(0)));
    }

    /**
     * Converts a BigInteger value to a Byte (8-bit signed integer).
     * @param value the BigInteger value to convert
     * @return the Byte representation of the value
     */
    public static Byte apply(BigInteger value) {
        return value.byteValue();
    }
}
