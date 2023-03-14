package hydra.lib.lists;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Terms;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.Collections;
import java.util.function.Function;

import static hydra.dsl.Types.*;

public class Pure<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.pure");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function("x", list("x")));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Void, Term<A>>> implementation() {
        return args -> Flows.pure(Terms.list(apply(args.get(0))));
    }

    public static <X> List<X> apply(X single) {
        return Collections.singletonList(single);
    }
}
