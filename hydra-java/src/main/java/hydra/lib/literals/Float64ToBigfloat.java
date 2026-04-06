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
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Primitive function which converts a float64 (64-bit floating-point) to a bigfloat (arbitrary-precision decimal).
 * This conversion preserves the value but may introduce representation differences.
 */
public class Float64ToBigfloat extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.float64ToBigfloat"
     */
    public Name name() {
        return new Name("hydra.lib.literals.float64ToBigfloat");
    }

    /**
     * Returns the type scheme for this function: float64 -&gt; bigfloat.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.float64(), Types.bigfloat()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts float64 terms to bigfloat terms
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(s -> Terms.bigfloat(apply(s)), hydra.extract.Core.float64(cx, graph, args.get(0)));
    }

    /**
     * Converts a Double (64-bit) value to a BigDecimal.
     * @param value the Double value to convert
     * @return the BigDecimal representation of the value
     */
    public static BigDecimal apply(Double value) {
        // BigDecimal cannot represent NaN or Infinity; use sentinel zero.
        // Callers (coders) should check for NaN/Inf before reaching here.
        if (Double.isNaN(value) || Double.isInfinite(value)) {
            return BigDecimal.ZERO;
        }
        return BigDecimal.valueOf(value);
    }
}
