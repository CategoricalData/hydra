package hydra.core.overlay.java.lib.eithers;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.either;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.var;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Either;

/**
 * Extract all Left values from a list of Eithers.
 */
public class Lefts extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.core.lib.eithers.lefts");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(list(either(var("a"), var("b"))), list(var("a"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((java.util.List<hydra.core.overlay.java.util.Either<Term, Term>> eithers) ->
                new Term.List(apply(eithers)),
            hydra.core.extract.Core.listOf(arg -> hydra.core.extract.Core.eitherTerm(t -> Either.right(t), t -> Either.right(t), graph, arg), graph, args.get(0)));
    }

    /**
     * Extract all Left values from a list of Eithers, discarding the Right values.
     *
     * @param <A> the left type
     * @param <B> the right type
     * @param eithers the list of Either values
     * @return a list containing only the Left values
     */
    public static <A, B> List<A> apply(List<hydra.core.overlay.java.util.Either<A, B>> eithers) {
        ConsList<A> reversed = ConsList.empty();
        for (hydra.core.overlay.java.util.Either<A, B> either : eithers) {
            if (either.isLeft()) {
                reversed = ConsList.cons(((hydra.core.overlay.java.util.Either.Left<A, B>) either).value, reversed);
            }
        }
        return reversed.reverse();
    }
}
