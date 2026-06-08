package hydra.lib.maps;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Optional;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
import hydra.util.Either;
import hydra.util.PersistentMap;


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
        return Types.constrained2("v", Types.NONE, "k", Types.ORD,
                function(function(optional("v"), optional("v")), Types.var("k"), map("k", "v"), map("k", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.map(t -> Either.right(t), t -> Either.right(t), graph, args.get(2)), (Map<Term, Term> mp) -> {
                Term f = args.get(0);
                Term key = args.get(1);
                Optional<Term> currentValue = Lookup.apply(key, mp);
                Either<Error_, Term> r = hydra.Reduction.reduceTerm(
                    hydra.Lexical.emptyInferenceContext(), graph, true, Terms.apply(f, Terms.optional(currentValue)));
                if (r.isLeft()) return (Either) r;
                Either<Error_, Optional<Term>> maybeResult = hydra.extract.Core.optionalTerm(
                    t -> Either.right(t), graph, ((Either.Right<Error_, Term>) r).value);
                if (maybeResult.isLeft()) return (Either) maybeResult;
                Optional<Term> newValue = ((Either.Right<Error_, Optional<Term>>) maybeResult).value;
                PersistentMap<Term, Term> base = PersistentMap.coerce(mp);
                PersistentMap<Term, Term> result = newValue.isGiven()
                    ? base.insert(key, newValue.fromGiven())
                    : base.delete(key);
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
            Function<Optional<V>, Optional<V>> f) {
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
    public static <K, V> Map<K, V> apply(Function<Optional<V>, Optional<V>> f, K key, Map<K, V> mp) {
        Optional<V> current = mp.containsKey(key) ? Optional.given(mp.get(key)) : Optional.none();
        Optional<V> newValue = f.apply(current);
        PersistentMap<K, V> base = PersistentMap.coerce(mp);
        return newValue.isGiven() ? base.insert(key, newValue.fromGiven()) : base.delete(key);
    }
}
