package hydra;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.dsl.Terms;
import hydra.graph.AnnotationClass;
import hydra.graph.Element;
import hydra.graph.Graph;
import hydra.graph.Primitive;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;

import static hydra.Flows.*;


public class HydraTestBase {
    protected static <A> Graph<A> emptyGraph() {
        Map<Name, Element<A>> elements = Collections.emptyMap();
        Map<Name, Optional<Term<A>>> environment = Collections.emptyMap();
        Term<A> body = Terms.string("empty graph");
        Map<Name, Primitive<A>> primitives = Collections.emptyMap();
        AnnotationClass<A> annotations = null;
        Optional<Graph<A>> schema = Optional.empty();

        return new Graph<>(elements, environment, body, primitives, annotations, schema);
    }

    protected static <A> Flow<Graph<A>, Term<A>> reduce(Term<A> original) {
        return pure(original);
    }
}
