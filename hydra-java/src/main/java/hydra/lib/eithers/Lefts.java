package hydra.lib.eithers;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;

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
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(
            Expect.list(arg -> Expect.<Graph, Term, Term>either(arg), args.get(0)),
            eithers -> {
                List<Term> lefts = new ArrayList<>();
                for (hydra.util.Either<Term, Term> e : eithers) {
                    e.accept(new hydra.util.Either.Visitor<Term, Term, Void>() {
                        @Override
                        public Void visit(hydra.util.Either.Left<Term, Term> left) {
                            lefts.add(left.value);
                            return null;
                        }

                        @Override
                        public Void visit(hydra.util.Either.Right<Term, Term> right) {
                            return null;
                        }
                    });
                }
                return new Term.List(lefts);
            });
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
        List<A> result = new ArrayList<>();
        for (hydra.util.Either<A, B> either : eithers) {
            if (either.isLeft()) {
                result.add(((hydra.util.Either.Left<A, B>) either).value);
            }
        }
        return result;
    }
}
