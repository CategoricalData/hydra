package hydra;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.graph.Graph;
import hydra.graph.Primitive;

import java.util.Optional;

import static hydra.Flows.bind;
import static hydra.Flows.fail;
import static hydra.Flows.getState;


public class Lexical {
    private Lexical() {
    }

    public static <A> Optional<Primitive<A>> lookupPrimitive(Graph<A> g, Name name) {
        return Optional.ofNullable(g.primitives.get(name));
    }

    public static <A> Flow<Graph<A>, Primitive<A>> requirePrimitive(Name name) {
        return bind(getState(), g -> {
            Optional<Primitive<A>> mprim = lookupPrimitive(g, name);
            return mprim.<Flow<Graph<A>, Primitive<A>>>map(Flows::pure)
                .orElseGet(() -> fail("no such primitive function: " + name.value));
        });
    }
}
