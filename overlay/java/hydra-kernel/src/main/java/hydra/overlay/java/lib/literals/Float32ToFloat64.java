package hydra.overlay.java.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Primitive function which converts a float32 (IEEE 754 single) to a float64 (IEEE 754 double).
 * Lossless widening conversion.
 */
public class Float32ToFloat64 extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Literals.float32ToFloat64().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(Types.float32(), Types.float64()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(f -> Terms.float64(apply(f)), hydra.extract.Core.float32(graph, args.get(0)));
    }

    public static Double apply(Float value) {
        return (double) value.floatValue();
    }
}
