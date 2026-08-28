package hydra.overlay.java.lib.sets;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.boolean_;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.schemeOrd;
import static hydra.overlay.java.dsl.Types.set;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;
import hydra.overlay.java.util.PersistentSet;


/**
 * Filters a set by a predicate, keeping only elements for which the predicate is true.
 */
public class Filter extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Sets.filter().name;
    }

    @Override
    public TypeScheme type() {
        return schemeOrd("x", function(function(Types.var("x"), boolean_()), set("x"), set("x")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph ->
            hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.set(graph, args.get(1)), arg -> {
                PersistentSet<Term> result = PersistentSet.<Term>empty();
                for (Term x : arg) {
                    Either<Error_, Term> r = hydra.Reduction.reduceTerm(
                        hydra.Lexical.emptyInferenceContext(), graph, true, Terms.apply(args.get(0), x));
                    if (r.isLeft()) return (Either) r;
                    Either<Error_, Boolean> b = hydra.extract.Core.boolean_(graph,
                        ((Either.Right<Error_, Term>) r).value);
                    if (b.isLeft()) return (Either) b;
                    if (((Either.Right<Error_, Boolean>) b).value) {
                        result = result.insert(x);
                    }
                }
                return Either.right(Terms.set(result));
            });
    }

    /**
     * Filters a set by a predicate, keeping only elements for which the predicate is true.
     * @param <X> the element type
     * @param predicate the predicate to test elements
     * @return a function that filters a set by the predicate
     */
    public static <X> Function<Set<X>, Set<X>> apply(Function<X, Boolean> predicate) {
        return (arg) -> apply(predicate, arg);
    }

    /**
     * Filters a set by a predicate, keeping only elements for which the predicate is true.
     * @param <X> the element type
     * @param predicate the predicate to test elements
     * @param arg the set to filter
     * @return a new set containing only elements for which the predicate returns true
     */
    public static <X> Set<X> apply(Function<X, Boolean> predicate, Set<X> arg) {
        PersistentSet<X> result = PersistentSet.<X>empty();
        for (X x : arg) {
            if (predicate.apply(x)) {
                result = result.insert(x);
            }
        }
        return result;
    }
}
