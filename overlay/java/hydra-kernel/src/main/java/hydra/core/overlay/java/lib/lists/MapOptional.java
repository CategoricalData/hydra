package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;
import hydra.core.overlay.java.util.Optional;


/**
 * Traverse an optional value in the list monad: mapOptional(f, m) returns the single-element
 * list containing none when m is none; for given(x) it returns given(y) for each element y of
 * f(x), in order.
 */
public class MapOptional extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Lists.mapOptional().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("x", "y",
            function(
                function(Types.var("x"), list("y")),
                optional("x"),
                list(optional("y"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(
            hydra.core.extract.Model.optionalTerm(t -> Either.right(t), graph, args.get(1)), maybe -> {
                if (maybe.isNone()) {
                    return Either.right(Terms.list(Collections.singletonList(Terms.optional(Optional.none()))));
                }
                Term val = maybe.fromGiven();
                Term f = args.get(0);
                Either<Error_, Term> r = hydra.core.Reduction.reduceTerm(
                    hydra.core.Lexical.emptyInferenceContext(), graph, true, Terms.apply(f, val));
                if (r.isLeft()) return (Either) r;
                Either<Error_, List<Term>> choices = hydra.core.extract.Model.list(graph,
                    ((Either.Right<Error_, Term>) r).value);
                if (choices.isLeft()) return (Either) choices;
                List<Term> result = new ArrayList<>();
                for (Term y : ((Either.Right<Error_, List<Term>>) choices).value) {
                    result.add(Terms.optional(Optional.given(y)));
                }
                return Either.right(Terms.list(result));
            });
    }

    /**
     * Traverse an optional value in the list monad.
     * @param <A> the input type
     * @param <B> the choice element type
     * @param f the list-returning function to apply
     * @param m the optional value to traverse
     * @return a single-element list containing none for a none input, or one list element
     *         (wrapped in given) per choice in f(x) for input given(x)
     */
    public static <A, B> List<Optional<B>> apply(Function<A, List<B>> f, Optional<A> m) {
        if (m.isNone()) {
            return Collections.singletonList(Optional.none());
        }
        List<Optional<B>> result = new ArrayList<>();
        for (B y : f.apply(m.fromGiven())) {
            result.add(Optional.given(y));
        }
        return result;
    }
}
