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
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(
            Expect.list(arg -> Expect.<Graph, Term, Term>either(arg), args.get(0)),
            eithers -> {
                List<Term> rights = new ArrayList<>();
                for (hydra.util.Either<Term, Term> e : eithers) {
                    e.accept(new hydra.util.Either.Visitor<Term, Term, Void>() {
                        @Override
                        public Void visit(hydra.util.Either.Left<Term, Term> left) {
                            return null;
                        }

                        @Override
                        public Void visit(hydra.util.Either.Right<Term, Term> right) {
                            rights.add(right.value);
                            return null;
                        }
                    });
                }
                return new Term.List(rights);
            });
    }

    /**
     * Extract all Right values from a list of Eithers, discarding the Left values.
     *
     * @param <A> the left type
     * @param <B> the right type
     * @param eithers the list of Either values
     * @return a list containing only the Right values
     */
    public static <A, B> List<B> apply(List<hydra.util.Either<A, B>> eithers) {
        List<B> result = new ArrayList<>();
        for (hydra.util.Either<A, B> either : eithers) {
            if (either.isRight()) {
                result.add(((hydra.util.Either.Right<A, B>) either).value);
            }
        }
        return result;
    }
}
