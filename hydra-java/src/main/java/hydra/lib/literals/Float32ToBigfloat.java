package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.math.BigDecimal;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;

/**
 * Primitive function which converts a float32 (32-bit floating-point) to a bigfloat (arbitrary-precision decimal).
 * This conversion preserves the value but may introduce representation differences.
 */
public class Float32ToBigfloat extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.float32ToBigfloat"
     */
    public Name name() {
        return new Name("hydra.lib.literals.float32ToBigfloat");
    }

    /**
     * Returns the type scheme for this function: float32 -&gt; bigfloat.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.float32(), Types.bigfloat()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts float32 terms to bigfloat terms
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(s -> Terms.bigfloat(apply(s)), hydra.extract.core.Core.float32(cx, graph, args.get(0)));
    }

    /**
     * Converts a Float (32-bit) value to a BigDecimal.
     * @param value the Float value to convert
     * @return the BigDecimal representation of the value
     */
    public static BigDecimal apply(Float value) {
        return BigDecimal.valueOf(value);
    }
}
