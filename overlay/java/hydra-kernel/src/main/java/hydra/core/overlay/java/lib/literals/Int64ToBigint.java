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
 * Primitive function which converts an int64 (64-bit signed integer) to a bigint (arbitrary-precision integer).
 * This conversion is lossless.
 */
public class Int64ToBigint extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.core.lib.literals.int64ToBigint"
     */
    public Name name() {
        return hydra.lib.Literals.int64ToBigint().name;
    }

    /**
     * Returns the type scheme for this function: int64 -&gt; bigint.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.int64(), Types.bigint()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts int64 terms to bigint terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(s -> Terms.bigint(apply(s)), hydra.core.extract.Core.int64(graph, args.get(0)));
    }

    /**
     * Converts a Long (64-bit signed integer) value to a BigInteger.
     * @param value the Long value to convert
     * @return the BigInteger representation of the value
     */
    public static BigInteger apply(Long value) {
        return BigInteger.valueOf(value);
    }
}
