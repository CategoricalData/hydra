package hydra.overlay.java.lib.eithers;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.either;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.var;
import hydra.errors.Error_;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Either;

/**
 * Extract all Right values from a list of Eithers.
 */
public class Rights extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.rights");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(list(either(var("a"), var("b"))), list(var("b"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((java.util.List<hydra.overlay.java.util.Either<Term, Term>> eithers) ->
                new Term.List(apply(eithers)),
            hydra.extract.Core.listOf(arg -> hydra.extract.Core.eitherTerm(t -> Either.right(t), t -> Either.right(t), graph, arg), graph, args.get(0)));
    }

    /**
     * Extract all Right values from a list of Eithers, discarding the Left values.
     *
     * @param <A> the left type
     * @param <B> the right type
     * @param eithers the list of Either values
     * @return a list containing only the Right values
     */
    public static <A, B> List<B> apply(List<hydra.overlay.java.util.Either<A, B>> eithers) {
        ConsList<B> reversed = ConsList.empty();
        for (hydra.overlay.java.util.Either<A, B> either : eithers) {
            if (either.isRight()) {
                reversed = ConsList.cons(((hydra.overlay.java.util.Either.Right<A, B>) either).value, reversed);
            }
        }
        return reversed.reverse();
    }
}
