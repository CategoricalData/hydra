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
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Divide two integers, returning Nothing if the divisor is zero.
 */
public class MaybeDiv extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.math.maybeDiv");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), int32(), optional(int32())));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.int32(graph, args.get(0)),
            arg0 -> hydra.lib.eithers.Map.apply(arg1 -> Terms.optional(apply(arg0, arg1).map(Terms::int32)),
            hydra.extract.Core.int32(graph, args.get(1))));
    }

    /**
     * Divides the first number by the second.
     * @param dividend the dividend
     * @return a function that takes a divisor and returns a Maybe containing the quotient
     */
    public static Function<Integer, Maybe<Integer>> apply(Integer dividend) {
        return (divisor) -> apply(dividend, divisor);
    }

    /**
     * Divides the first number by the second.
     * @param dividend the dividend
     * @param divisor the divisor
     * @return a Maybe containing the quotient, or empty if divisor is zero
     */
    public static Maybe<Integer> apply(Integer dividend, Integer divisor) {
        if (divisor == 0) {
            return Maybe.nothing();
        } else {
            return Maybe.just(Math.floorDiv(dividend, divisor));
        }
    }
}
