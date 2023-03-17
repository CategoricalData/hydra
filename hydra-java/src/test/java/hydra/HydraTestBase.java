package hydra;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.dsl.Terms;
import hydra.graph.AnnotationClass;
import hydra.graph.Element;
import hydra.graph.Graph;
import hydra.graph.Primitive;
import hydra.lib.Libraries;
import hydra.tools.PrimitiveFunction;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;


public class HydraTestBase {
    protected static <A> Graph<A> emptyGraph() {
        Map<Name, Element<A>> elements = Collections.emptyMap();
        Map<Name, Optional<Term<A>>> environment = Collections.emptyMap();
        Term<A> body = Terms.string("empty graph");

        Map<Name, Primitive<A>> primitives = new HashMap<>();
        for (PrimitiveFunction prim : Libraries.standardPrimitives()) {
            primitives.put(prim.name(), prim.toNative());
        }

        AnnotationClass<A> annotations = null;
        Optional<Graph<A>> schema = Optional.empty();

        return new Graph<>(elements, environment, body, primitives, annotations, schema);
    }

    protected static <A> Flow<Graph<A>, Term<A>> reduce(Term<A> original) {
        return Reduction.reduceEager(original);
    }
}
