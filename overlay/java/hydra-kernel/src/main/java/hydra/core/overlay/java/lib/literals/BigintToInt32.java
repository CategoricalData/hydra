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
 * Primitive function which converts a bigint (arbitrary-precision integer) to an int32 (32-bit signed integer).
 * This conversion may result in loss of precision if the value is outside the int32 range.
 */
public class BigintToInt32 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.core.lib.literals.bigintToInt32"
     */
    public Name name() {
        return hydra.lib.Literals.bigintToInt32().name;
    }

    /**
     * Returns the type scheme for this function: bigint -&gt; int32.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.bigint(), Types.int32()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts bigint terms to int32 terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(s -> Terms.int32(apply(s)), hydra.core.extract.Core.bigint(graph, args.get(0)));
    }

    /**
     * Converts a BigInteger value to an Integer (32-bit signed integer).
     * @param value the BigInteger value to convert
     * @return the Integer representation of the value
     */
    public static Integer apply(BigInteger value) {
        return value.intValue();
    }
}
