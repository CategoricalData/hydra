package hydra.lib.literals;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;

public class Float64ToBigfloat<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/literals.float64ToBigfloat");
    }

    @Override
    public Type<A> type() {
        return function(Types.float64(), Types.bigfloat());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map(Expect.float64(args.get(0)), s -> Terms.bigfloat(apply(s)));
    }

    public static Double apply(Double value) {
        return value;
    }
}
