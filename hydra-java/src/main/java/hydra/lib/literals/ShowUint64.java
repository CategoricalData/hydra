package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import static hydra.dsl.Types.uint64;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


/**
 * Primitive function which converts a uint64 (64-bit unsigned integer) to its string representation.
 */
public class ShowUint64 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.showUint64"
     */
    public Name name() {
        return new Name("hydra.lib.literals.showUint64");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<BigInteger, Term>) i -> Terms.string(apply(i)), hydra.extract.core.Core.uint64(cx, graph, args.get(0)));
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
