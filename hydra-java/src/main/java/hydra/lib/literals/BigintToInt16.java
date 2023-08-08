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

public class BigintToInt16<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/literals.bigintToInt16");
    }

    @Override
    public Type<A> type() {
        return function(Types.bigint(), Types.int16());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map(Expect.bigint(args.get(0)), s -> Terms.int16(apply(s)));
    }

    public static Short apply(BigInteger value) {
        return value.shortValue();
    }
}
