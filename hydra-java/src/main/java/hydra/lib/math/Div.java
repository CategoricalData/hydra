package hydra.lib.math;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.Flows.bind2;
import static hydra.Flows.fail;
import static hydra.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;

public class Div<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/math.div");
    }

    @Override
    public Type<A> type() {
        return function(int32(), int32(), int32());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> bind2(Expect.int32(args.get(0)), Expect.int32(args.get(1)),
            (BiFunction<Integer, Integer, Flow<Graph<A>, Term<A>>>) (arg0, arg1) -> {
                if (arg1.equals(0)) {
                    return fail("division by zero");
                } else {
                    return pure(Terms.int32(apply(arg0, arg1)));
                }
            });
    }

    public static Function<Integer, Integer> apply(Integer dividend) {
        return (divisor) -> apply(dividend, divisor);
    }

    public static Integer apply(Integer dividend, Integer divisor) {
        return (dividend / divisor);
    }
}
