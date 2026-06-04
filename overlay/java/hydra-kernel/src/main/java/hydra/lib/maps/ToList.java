package hydra.lib.maps;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Pair;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.variable;
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
import hydra.util.ConsList;
import hydra.util.Either;


/**
 * Converts a map to a list of pairs.
 */
public class ToList extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.toList");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE, function(
                map("k", "v"),
                list(pair(variable("k"), variable("v")))));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> {
            Either<Error_, Map<Term, Term>> r = hydra.extract.Core.map(t -> Either.right(t), t -> Either.right(t), graph, args.get(0));
            return hydra.lib.eithers.Map.apply(m -> {
                ConsList<Term> reversed = ConsList.empty();
                for (Map.Entry<Term, Term> e : m.entrySet()) {
                    reversed = ConsList.cons(Terms.pair(e.getKey(), e.getValue()), reversed);
                }
                return Terms.list(reversed.reverse());
            }, r);
        };
    }

    /**
     * Apply the function to its single argument.
     * @param <K> the key type
     * @param <V> the value type
     * @param map the map to convert
     * @return a list of key-value pairs
     */
    public static <K, V> List<Pair<K, V>> apply(Map<K, V> map) {
        ConsList<Pair<K, V>> reversed = ConsList.empty();
        for (Map.Entry<K, V> e : map.entrySet()) {
            reversed = ConsList.cons(new Pair<>(e.getKey(), e.getValue()), reversed);
        }
        return reversed.reverse();
    }
}
