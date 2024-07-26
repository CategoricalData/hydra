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


public abstract class PrimitiveFunction {
    public abstract Name name();

    public abstract Type type();

    protected abstract Function<List<Term>, Flow<Graph, Term>> implementation();

    public Term term() {
        return Terms.primitive(name());
    }

    public Primitive toNative() {
        return new Primitive(name(), type(), implementation());
    }
}
