package hydra.lib.math;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.tools.PrimitiveFunction;
import java.util.List;
import java.util.function.Function;

import static hydra.Flows.*;
import static hydra.dsl.Types.*;

public class Neg<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/math.neg");
    }

    @Override
    public Type<A> type() {
        return function(int32(), int32());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Void, Term<A>>> implementation() {
        return args -> map(Expect.int32(args.get(0)),
            (arg0) -> Terms.int32(apply(arg0)));
    }

    public static Integer apply(Integer num) {
        return (-1 * num);
    }
}
