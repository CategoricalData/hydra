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

import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;

public class Int64ToBigint<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/literals.int64ToBigint");
    }

    @Override
    public Type<A> type() {
        return function(Types.int64(), Types.bigint());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map(Expect.int64(args.get(0)), s -> Terms.bigint(apply(s)));
    }

    public static BigInteger apply(Long value) {
        return BigInteger.valueOf(value);
    }
}
