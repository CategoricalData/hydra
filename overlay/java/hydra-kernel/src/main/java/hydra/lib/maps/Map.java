package hydra.lib.maps;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
import hydra.util.Either;
import hydra.util.PersistentMap;


/**
 * Maps a function over a flow.
 */
public class Map extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.map");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained3("v1", Types.NONE, "v2", Types.NONE, "k", Types.ORD,
            function(function("v1", "v2"), map("k", "v1"), map("k", "v2")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph ->
            hydra.lib.eithers.Bind.apply(hydra.extract.Core.map(t -> Either.right(t), t -> Either.right(t), graph, args.get(1)), mp -> {
                PersistentMap<Term, Term> result = PersistentMap.<Term, Term>empty();
                for (java.util.Map.Entry<Term, Term> e : mp.entrySet()) {
                    Either<Error_, Term> r = hydra.Reduction.reduceTerm(
                        hydra.Lexical.emptyInferenceContext(), graph, true, Terms.apply(args.get(0), e.getValue()));
                    if (r.isLeft()) return (Either) r;
                    result = result.insert(e.getKey(), ((Either.Right<Error_, Term>) r).value);
                }
                return Either.right(Terms.map(result));
            });
    }

    /**
     * Transforms a flow value.
     * @param <K> the key type
     * @param <V1> the input value type
     * @param <V2> the output value type
     * @param mapping the function to apply to each value
     * @return a function that takes a map and returns the transformed map
     */
    public static <K, V1, V2> Function<java.util.Map<K, V1>, java.util.Map<K, V2>> apply(Function<V1, V2> mapping) {
        return (arg) -> apply(mapping, arg);
    }

    /**
     * Apply the function to both arguments.
     * @param <K> the key type
     * @param <V1> the input value type
     * @param <V2> the output value type
     * @param mapping the function to apply to each value
     * @param arg the input map
     * @return the transformed map
     */
    public static <K, V1, V2> java.util.Map<K, V2> apply(Function<V1, V2> mapping, java.util.Map<K, V1> arg) {
        PersistentMap<K, V2> result = PersistentMap.<K, V2>empty();
        for (java.util.Map.Entry<K, V1> e : arg.entrySet()) {
            result = result.insert(e.getKey(), mapping.apply(e.getValue()));
        }
        return result;
    }
}
