package hydra.overlay.java.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.math.BigDecimal;
import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Primitive function which converts a float32 (IEEE 754 single) to a decimal (arbitrary-precision exact decimal).
 * Uses Float.toString for the shortest round-trip digits, stripping the cosmetic trailing
 * ".0" Java always appends to whole values -- the decimal's scale must reflect only the
 * significant digits, not that convention (2.0f is scale-0 "2", not scale-1 "2.0").
 */
public class Float32ToDecimal extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Literals.float32ToDecimal().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(Types.float32(), Types.decimal()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(f -> Terms.decimal(apply(f)), hydra.extract.Core.float32(graph, args.get(0)));
    }

    public static BigDecimal apply(Float value) {
        String s = Float.toString(value);
        if (s.endsWith(".0")) {
            s = s.substring(0, s.length() - 2);
        }
        return new BigDecimal(s);
    }
}
