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
 * Return the predecessor of an integer, or Nothing if the value is Integer.MIN_VALUE.
 */
public class MaybePred extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.math.maybePred");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), optional(int32())));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((arg0) -> Terms.optional(apply(arg0).map(Terms::int32)), hydra.extract.Core.int32(graph, args.get(0)));
    }

    /**
     * Returns the predecessor of the value.
     * @param num the value
     * @return a Maybe containing the predecessor, or empty if the value is Integer.MIN_VALUE
     */
    public static Maybe<Integer> apply(Integer num) {
        if (num == Integer.MIN_VALUE) {
            return Maybe.nothing();
        } else {
            return Maybe.just(num - 1);
        }
    }
}
