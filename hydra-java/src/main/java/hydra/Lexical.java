package hydra;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.graph.Graph;
import hydra.graph.Primitive;

import java.util.Optional;

import static hydra.Flows.bind;
import static hydra.Flows.fail;
import static hydra.Flows.getState;


/**
 * Lexical functions (which deal with named elements and primitives within graphs).
 */
public class Lexical {
    private Lexical() {
    }

    /**
     * Get a primitive from a graph by name; the primitive is not required to exist.
     */
    public static <A> Optional<Primitive<A>> lookupPrimitive(Graph<A> g, Name name) {
        return Optional.ofNullable(g.primitives.get(name));
    }

    /**
     * Get a primitive from the current graph by name; the primitive is required to exist.
     */
    public static <A> Flow<Graph<A>, Primitive<A>> requirePrimitive(Name name) {
        return bind(getState(), g -> {
            Optional<Primitive<A>> mprim = lookupPrimitive(g, name);
            return mprim.<Flow<Graph<A>, Primitive<A>>>map(Flows::pure)
                .orElseGet(() -> fail("no such primitive function: " + name.value));
        });
    }
}
