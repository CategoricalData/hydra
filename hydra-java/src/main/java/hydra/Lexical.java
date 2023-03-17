package hydra;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.graph.Graph;
import hydra.graph.Primitive;
import java.util.Optional;
import java.util.function.Function;

import static hydra.Flows.*;


public class Lexical {
    private Lexical() {
    }

    public static <A> Optional<Primitive<A>> lookupPrimitive(Graph<A> g, Name name) {
        return Optional.ofNullable(g.primitives.get(name));
    }

    public static <A> Flow<Graph<A>, Primitive<A>> requirePrimitive(Name name) {
        return bind(getState(), new Function<Graph<A>, Flow<Graph<A>, Primitive<A>>>() {
            @Override
            public Flow<Graph<A>, Primitive<A>> apply(Graph<A> g) {
                Optional<Primitive<A>> mprim = lookupPrimitive(g, name);
                return mprim.<Flow<Graph<A>, Primitive<A>>>map(Flows::pure)
                    .orElseGet(() -> fail("no such primitive function: " + name.value));
            }
        });
    }
}
