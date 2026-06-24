package hydra.overlay.java.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;
import hydra.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.int32;
import static hydra.overlay.java.dsl.Types.optional;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Return the successor of an integer, or Nothing if the value is Integer.MAX_VALUE.
 */
public class MaybeSucc extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Math_.maybeSucc().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), optional(int32())));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((arg0) -> Terms.optional(apply(arg0).map(Terms::int32)), hydra.extract.Core.int32(graph, args.get(0)));
    }

    /**
     * Returns the successor of the value.
     * @param num the value
     * @return a Optional containing the successor, or empty if the value is Integer.MAX_VALUE
     */
    public static Optional<Integer> apply(Integer num) {
        if (num == Integer.MAX_VALUE) {
            return Optional.none();
        } else {
            return Optional.given(num + 1);
        }
    }
}
