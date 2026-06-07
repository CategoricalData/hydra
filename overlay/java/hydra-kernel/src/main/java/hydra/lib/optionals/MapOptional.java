package hydra.lib.optionals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.ConsList;
import hydra.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Maps a flow function over Optional.
 */
public class MapOptional extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.optionals.mapOptional"
     */
    public Name name() {
        return new Name("hydra.lib.optionals.mapOptional");
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
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.list(graph, args.get(1)), inputList -> {
                Term f = args.get(0);
                ConsList<Term> reversed = ConsList.empty();
                for (Term item : inputList) {
                    Either<Error_, Term> r = hydra.Reduction.reduceTerm(
                        hydra.Lexical.emptyInferenceContext(), graph, true, Terms.apply(f, item));
                    if (r.isLeft()) return (Either) r;
                    Either<Error_, Optional<Term>> maybeResult = hydra.extract.Core.optionalTerm(
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
