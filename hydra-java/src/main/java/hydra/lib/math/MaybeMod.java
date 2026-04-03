package hydra.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Compute the modulo of two integers, returning Nothing if the divisor is zero.
 */
public class MaybeMod extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.math.maybeMod");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), int32(), optional(int32())));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.int32(cx, graph, args.get(0)),
            arg0 -> hydra.lib.eithers.Map.apply(arg1 -> Terms.optional(apply(arg0, arg1).map(Terms::int32)),
            hydra.extract.Core.int32(cx, graph, args.get(1))));
    }

    /**
     * Computes the modulo.
     * @param dividend the dividend
     * @return a function that takes a divisor and returns a Maybe containing the modulo
     */
    public static Function<Integer, Maybe<Integer>> apply(Integer dividend) {
        return (divisor) -> apply(dividend, divisor);
    }

    /**
     * Computes the modulo.
     * @param dividend the dividend
     * @param divisor the divisor
     * @return a Maybe containing the modulo, or empty if divisor is zero
     */
    public static Maybe<Integer> apply(Integer dividend, Integer divisor) {
        if (divisor == 0) {
            return Maybe.nothing();
        } else {
            return Maybe.just(java.lang.Math.floorMod(dividend, divisor));
        }
    }
}
