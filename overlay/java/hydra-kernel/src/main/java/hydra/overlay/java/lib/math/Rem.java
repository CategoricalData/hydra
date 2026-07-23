package hydra.overlay.java.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;
import hydra.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.int32;
import static hydra.overlay.java.dsl.Types.optional;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Compute the remainder of integer division, returning Nothing if the divisor is zero.
 */
public class Rem extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Math_.rem().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), int32(), optional(int32())));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.int32(graph, args.get(0)),
            arg0 -> hydra.overlay.java.lib.eithers.Map.apply(arg1 -> Terms.optional(apply(arg0, arg1).map(Terms::int32)),
            hydra.extract.Core.int32(graph, args.get(1))));
    }

    /**
     * Computes the remainder.
     * @param dividend the dividend
     * @return a function that takes a divisor and returns a Optional containing the remainder
     */
    public static Function<Integer, Optional<Integer>> apply(Integer dividend) {
        return (divisor) -> apply(dividend, divisor);
    }

    /**
     * Computes the remainder.
     * @param dividend the dividend
     * @param divisor the divisor
     * @return a Optional containing the remainder, or empty if divisor is zero
     */
    public static Optional<Integer> apply(Integer dividend, Integer divisor) {
        if (divisor == 0) {
            return Optional.none();
        } else {
            return Optional.given(dividend % divisor);
        }
    }
}
