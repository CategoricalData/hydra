package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Primitive function which converts a float64 (IEEE 754 double) to a float32 (IEEE 754 single).
 * Lossy narrowing conversion: values outside the float32 range round to +/-Infinity, and precision is reduced.
 */
public class Float64ToFloat32 extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.float64ToFloat32");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(Types.float64(), Types.float32()));
    }

    @Override
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(d -> Terms.float32(apply(d)), hydra.extract.Core.float64(graph, args.get(0)));
    }

    public static Float apply(Double value) {
        return (float) value.doubleValue();
    }
}
