package hydra.lib.maps;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


/**
 * Returns the number of entries.
 */
public class Size extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.size");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE,
                function(hydra.dsl.Types.map("k", "v"), int32()));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(
                (Function<Map<Term, Term>, Term>) mp -> Terms.int32(mp.size()),
                hydra.extract.core.Core.map(cx, t -> Either.right(t), t -> Either.right(t), graph, args.get(0)));
    }

    /**
     * Returns the size of the map.
     * @param <K> the key type
     * @param <V> the value type
     * @param mp the map
     * @return the size
     */
    public static <K, V> Integer apply(Map<K, V> mp) {
        return mp.size();
    }
}
