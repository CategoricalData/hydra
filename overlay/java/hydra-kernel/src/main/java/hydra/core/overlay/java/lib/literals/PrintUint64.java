package hydra.core.overlay.java.lib.literals;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import static hydra.core.overlay.java.dsl.Types.uint64;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Primitive function which converts a uint64 (64-bit unsigned integer) to its string representation.
 */
public class PrintUint64 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.core.lib.literals.printUint64"
     */
    public Name name() {
        return hydra.core.lib.Literals.printUint64().name;
    }

    /**
     * Returns the type scheme for this function: uint64 -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(uint64(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts uint64 terms to string terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<BigInteger, Term>) i -> Terms.string(apply(i)), hydra.core.extract.Model.uint64(graph, args.get(0)));
    }

    /**
     * Converts a BigInteger (used to represent 64-bit unsigned integer) to its string representation.
     * @param value the BigInteger value to convert
     * @return the string representation of the value
     */
    public static String apply(BigInteger value) {
        return value.toString();
    }
}
