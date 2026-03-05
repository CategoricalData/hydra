package hydra.lib.maps;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


/**
 * Modifies a key's value.
 */
public class Alter extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.alter");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("v", "k",
                function(function(optional("v"), optional("v")), Types.var("k"), map("k", "v"), map("k", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.map(cx, t -> Either.right(t), t -> Either.right(t), graph, args.get(2)), mp -> {
                Term f = args.get(0);
                Term key = args.get(1);
                Maybe<Term> currentValue = Lookup.apply(key, mp);
                Either<InContext<OtherError>, Term> r = hydra.reduction.Reduction.reduceTerm(
                    hydra.monads.Monads.emptyContext(), graph, true, Terms.apply(f, Terms.optional(currentValue)));
                if (r.isLeft()) return (Either) r;
                Either<InContext<OtherError>, Maybe<Term>> maybeResult = hydra.extract.core.Core.maybeTerm(cx,
                    t -> Either.right(t), graph, ((Either.Right<InContext<OtherError>, Term>) r).value);
                if (maybeResult.isLeft()) return (Either) maybeResult;
                Maybe<Term> newValue = ((Either.Right<InContext<OtherError>, Maybe<Term>>) maybeResult).value;
                Map<Term, Term> result = FromList.orderedMap(mp);
                if (newValue.isJust()) {
                    result.put(key, newValue.fromJust());
                } else {
                    result.remove(key);
                }
                return Either.right(Terms.map(result));
            });
    }

    /**
     * Applies a function to modify the entry.
     * @param <K> the key type
     * @param <V> the value type
     * @param f the function to apply to the optional value
     * @return a curried function that takes a key and a map and returns the modified map
     */
    public static <K, V> Function<K, Function<Map<K, V>, Map<K, V>>> apply(
            Function<Maybe<V>, Maybe<V>> f) {
        return key -> mp -> apply(f, key, mp);
    }

    /**
     * Applies a function to modify the entry.
     * @param <K> the key type
     * @param <V> the value type
     * @param f the function to apply to the optional value
     * @param key the key to modify
     * @param mp the map to alter
     * @return the modified map
     */
    public static <K, V> Map<K, V> apply(Function<Maybe<V>, Maybe<V>> f, K key, Map<K, V> mp) {
        Map<K, V> result = FromList.orderedMap(mp);
        Maybe<V> currentValue = Lookup.apply(key, mp);
        Maybe<V> newValue = f.apply(currentValue);
        if (newValue.isJust()) {
            result.put(key, newValue.fromJust());
        } else {
            result.remove(key);
        }
        return result;
    }
}
