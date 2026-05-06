package hydra.lib.eithers;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.ConsList;
import hydra.util.Either;

/**
 * Extract all Left values from a list of Eithers.
 */
public class Lefts extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.lefts");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(list(either(var("a"), var("b"))), list(var("a"))));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((java.util.List<hydra.util.Either<Term, Term>> eithers) ->
                new Term.List(apply(eithers)),
            hydra.extract.Core.listOf(arg -> hydra.extract.Core.eitherTerm(t -> Either.right(t), t -> Either.right(t), graph, arg), graph, args.get(0)));
    }

    /**
     * Extract all Left values from a list of Eithers, discarding the Right values.
     *
     * @param <A> the left type
     * @param <B> the right type
     * @param eithers the list of Either values
     * @return a list containing only the Left values
     */
    public static <A, B> List<A> apply(List<hydra.util.Either<A, B>> eithers) {
        ConsList<A> reversed = ConsList.empty();
        for (hydra.util.Either<A, B> either : eithers) {
            if (either.isLeft()) {
                reversed = ConsList.cons(((hydra.util.Either.Left<A, B>) either).value, reversed);
            }
        }
        return reversed.reverse();
    }
}
