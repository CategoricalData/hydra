package hydra.core.overlay.java.lib.literals;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Primitive function which converts a float64 (IEEE 754 double) to a float32 (IEEE 754 single).
 * Lossy narrowing conversion: values outside the float32 range round to +/-Infinity, and precision is reduced.
 */
public class Float64ToFloat32 extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Literals.float64ToFloat32().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(Types.float64(), Types.float32()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(d -> Terms.float32(apply(d)), hydra.core.extract.Model.float64(graph, args.get(0)));
    }

    public static Float apply(Double value) {
        return (float) value.doubleValue();
    }
}
