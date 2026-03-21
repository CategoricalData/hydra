package hydra.lib.maybes;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import hydra.util.ConsList;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Maps a flow function over Maybe.
 */
public class MapMaybe extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.mapMaybe"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.mapMaybe");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.list(cx, graph, args.get(1)), inputList -> {
                Term f = args.get(0);
                List<Term> results = new ArrayList<>();
                for (Term item : inputList) {
                    Either<InContext<Error_>, Term> r = hydra.reduction.Reduction.reduceTerm(
                        hydra.lexical.Lexical.emptyContext(), graph, true, Terms.apply(f, item));
                    if (r.isLeft()) return (Either) r;
                    Either<InContext<Error_>, Maybe<Term>> maybeResult = hydra.extract.core.Core.maybeTerm(cx,
                        t -> Either.right(t), graph, ((Either.Right<InContext<Error_>, Term>) r).value);
                    if (maybeResult.isLeft()) return (Either) maybeResult;
                    Maybe<Term> maybe = ((Either.Right<InContext<Error_>, Maybe<Term>>) maybeResult).value;
                    if (maybe.isJust()) {
                        results.add(maybe.fromJust());
                    }
                }
                return Either.right(Terms.list(results));
            });
    }

    /**
     * Maps an optional-returning function over a list and collects present values. Curried version.
     * @param <X> the input element type
     * @param <Y> the output element type
     * @param f the optional-returning function to map
     * @return a function that takes a list and returns a list of present values
     */
    public static <X, Y> Function<ConsList<X>, ConsList<Y>> apply(Function<X, Maybe<Y>> f) {
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
    public static <X, Y> ConsList<Y> apply(Function<X, Maybe<Y>> f, ConsList<X> list) {
        ArrayList<Y> result = new ArrayList<>();
        for (X item : list) {
            Maybe<Y> maybe = f.apply(item);
            if (maybe.isJust()) {
                result.add(maybe.fromJust());
            }
        }
        return ConsList.fromList(result);
    }
}
