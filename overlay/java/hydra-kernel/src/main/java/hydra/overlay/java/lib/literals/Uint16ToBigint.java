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
 * Primitive function which converts a uint16 (16-bit unsigned integer) to a bigint (arbitrary-precision integer).
 * This conversion is lossless.
 */
public class Uint16ToBigint extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.uint16ToBigint"
     */
    public Name name() {
        return hydra.lib.Literals.uint16ToBigint().name;
    }

    /**
     * Returns the type scheme for this function: uint16 -&gt; bigint.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.uint16(), Types.bigint()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts uint16 terms to bigint terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(s -> Terms.bigint(apply(s)), hydra.extract.Core.uint16(graph, args.get(0)));
    }

    /**
     * Converts a Character (16-bit unsigned integer) value to a BigInteger.
     * @param value the Character value to convert
     * @return the BigInteger representation of the value
     */
    public static BigInteger apply(Character value) {
        return BigInteger.valueOf(value);
    }
}
