package hydra.tools;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.graph.Primitive;
import java.util.List;
import java.util.function.Function;


public abstract class PrimitiveFunction<A> {
    public abstract Name name();

    public abstract Type<A> type();

    protected abstract Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation();

    public Term<A> term() {
        return Terms.primitive(name());
    }

    public Primitive<A> toNative() {
        return new Primitive<>(name(), type(), implementation());
    }
}
