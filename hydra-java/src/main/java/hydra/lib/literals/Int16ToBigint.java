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

public class Int16ToBigint<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/literals.int16ToBigint");
    }

    @Override
    public Type<A> type() {
        return function(Types.int16(), Types.bigint());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map(Expect.int16(args.get(0)), s -> Terms.bigint(apply(s)));
    }

    public static BigInteger apply(Short value) {
        return BigInteger.valueOf(value);
    }
}
