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

public class BigfloatToFloat32<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/literals.bigfloatToFloat32");
    }

    @Override
    public Type<A> type() {
        return function(Types.bigfloat(), Types.float32());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map(Expect.bigfloat(args.get(0)), s -> Terms.float32(apply(s)));
    }

    public static Float apply(Double value) {
        return (float) ((double) value);
    }
}
