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

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;

/**
 * Map a function over the Right side of an Either value.
 */
public class Map extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.map");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b", "z",
            function(
                function(var("a"), var("b")),
                either(var("z"), var("a")),
                either(var("z"), var("b"))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term fn = args.get(0);
            return Flows.map(Expect.<Graph, Term, Term>either(args.get(1)),
                e -> e.accept(new hydra.util.Either.Visitor<Term, Term, Term>() {
                    @Override
                    public Term visit(hydra.util.Either.Left<Term, Term> left) {
                        return Terms.left(left.value);
                    }

                    @Override
                    public Term visit(hydra.util.Either.Right<Term, Term> right) {
                        return Terms.right(Terms.apply(fn, right.value));
                    }
                }));
        };
    }

    /**
     * Apply a function to the Right side of an Either value.
     */
    public static <A, B, Z> hydra.util.Either<Z, B> apply(
            Function<A, B> fn,
            hydra.util.Either<Z, A> either) {
        if (either.isLeft()) {
            return new hydra.util.Either.Left<>(((hydra.util.Either.Left<Z, A>) either).value);
        } else {
            return new hydra.util.Either.Right<>(fn.apply(((hydra.util.Either.Right<Z, A>) either).value));
        }
    }

    /**
     * Curried version for method references.
     */
    public static <A, B, Z> Function<hydra.util.Either<Z, A>, hydra.util.Either<Z, B>> apply(
            Function<A, B> fn) {
        return either -> apply(fn, either);
    }
}
