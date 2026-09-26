package hydra.core.overlay.java.lib.optionals;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Maps a flow function over Optional.
 */
public class MapOptional extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.optionals.mapOptional"
     */
    public Name name() {
        return hydra.core.lib.Optionals.mapOptional().name;
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for mapping an optional-returning function over a list
     */
    @Override
    public TypeScheme type() {
        return scheme("a", "b", function(function("a", optional("b")), list("a"), list("b")));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that maps an optional-returning function over a list
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Model.list(graph, args.get(1)), inputList -> {
                Term f = args.get(0);
                ConsList<Term> reversed = ConsList.empty();
                for (Term item : inputList) {
                    Either<Error_, Term> r = hydra.core.Reduction.reduceTerm(
                        hydra.core.Lexical.emptyInferenceContext(), graph, true, Terms.apply(f, item));
                    if (r.isLeft()) return (Either) r;
                    Either<Error_, Optional<Term>> maybeResult = hydra.core.extract.Model.optionalTerm(
                        t -> Either.right(t), graph, ((Either.Right<Error_, Term>) r).value);
                    if (maybeResult.isLeft()) return (Either) maybeResult;
                    Optional<Term> maybe = ((Either.Right<Error_, Optional<Term>>) maybeResult).value;
                    if (maybe.isGiven()) {
                        reversed = ConsList.cons(maybe.fromGiven(), reversed);
                    }
                }
                return Either.right(Terms.list(reversed.reverse()));
            });
    }

    /**
     * Maps an optional-returning function over a list and collects present values. Curried version.
     * @param <X> the input element type
     * @param <Y> the output element type
     * @param f the optional-returning function to map
     * @return a function that takes a list and returns a list of present values
     */
    public static <X, Y> Function<List<X>, List<Y>> apply(Function<X, Optional<Y>> f) {
        return (list) -> apply(f, list);
    }

    /**
     * Maps an optional-returning function over a list and collects present values.
     * @param <X> the input element type
     * @param <Y> the output element type
     * @param f the optional-returning function to map
     * @param list the list to map over
     * @return a list containing only the present values from applying the function
     */
    public static <X, Y> List<Y> apply(Function<X, Optional<Y>> f, List<X> list) {
        ConsList<Y> reversed = ConsList.empty();
        for (X item : list) {
            Optional<Y> maybe = f.apply(item);
            if (maybe.isGiven()) {
                reversed = ConsList.cons(maybe.fromGiven(), reversed);
            }
        }
        return reversed.reverse();
    }
}
