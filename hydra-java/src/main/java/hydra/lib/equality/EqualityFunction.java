package hydra.lib.equality;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import java.util.List;
import java.util.function.Function;

import static hydra.Flows.*;
import static hydra.dsl.Types.*;


public abstract class EqualityFunction<A, T> extends PrimitiveFunction<A> {
    protected abstract String typeName();
    protected abstract Type<A> datatype();
    protected abstract Flow<Graph<A>, T> expect(Term<A> term);
    protected abstract boolean checkEqual(T first, T second);

    public Name name() {
        return new Name("hydra/lib/equality." + typeName());
    }

    @Override
    public Type<A> type() {
        return function(datatype(), datatype(), boolean_());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> map2(expect(args.get(0)), expect(args.get(1)),
            (arg0, arg1) -> Terms.boolean_(checkEqual(arg0, arg1)));
    }
}
