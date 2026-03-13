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
import hydra.error.Error_;
import hydra.util.Either;

/**
 * Primitive function which converts a bigfloat (arbitrary-precision decimal) to a float32 (32-bit floating-point).
 * This conversion may result in loss of precision.
 */
public class BigfloatToFloat32 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.bigfloatToFloat32"
     */
    public Name name() {
        return new Name("hydra.lib.literals.bigfloatToFloat32");
    }

    /**
     * Returns the type scheme for this function: bigfloat -&gt; float32.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(Types.bigfloat(), Types.float32()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts bigfloat terms to float32 terms
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(s -> Terms.float32(apply(s)), hydra.extract.core.Core.bigfloat(cx, graph, args.get(0)));
    }

    /**
     * Converts a BigDecimal value to a Float (32-bit).
     * @param value the BigDecimal value to convert
     * @return the Float representation of the value
     */
    public static Float apply(BigDecimal value) {
        return value.floatValue();
    }
}
