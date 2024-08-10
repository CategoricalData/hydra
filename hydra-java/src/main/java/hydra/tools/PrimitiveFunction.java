package hydra.tools;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.graph.Primitive;

import java.util.List;
import java.util.function.Function;


/**
 * Any of Hydra's primitive functions, implemented in Java
 */
public abstract class PrimitiveFunction {
    /**
     * The unique name of the primitive function
     */
    public abstract Name name();

    /**
     * The datatype of the primitive function
     */
    public abstract TypeScheme type();

    /**
     * A dynamic/interpreted implementation of the function
     */
    protected abstract Function<List<Term>, Flow<Graph, Term>> implementation();

    /**
     * The primitive function as a term
     */
    public Term term() {
        return Terms.primitive(name());
    }

    /**
     * The primitive function as a native Hydra Primitive object
     */
    public Primitive toNative() {
        return new Primitive(name(), type(), implementation());
    }
}
