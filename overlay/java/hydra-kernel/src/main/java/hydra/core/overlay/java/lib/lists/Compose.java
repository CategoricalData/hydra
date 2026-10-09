package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Either;


/**
 * Kleisli composition for the list monad: compose(f, g, x) = bind(f(x), g).
 */
public class Compose extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Lists.compose().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("x", "y", "z",
            function(
                function(Types.var("x"), list("y")),
                function(Types.var("y"), list("z")),
                Types.var("x"),
                list("z")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            Term f = args.get(0);
            Term g = args.get(1);
            Term x = args.get(2);
            Either<Error_, Term> fx = hydra.core.Reduction.reduceTerm(
                hydra.core.Lexical.emptyInferenceContext(), graph, true, Terms.apply(f, x));
            if (fx.isLeft()) return (Either) fx;
            Either<Error_, List<Term>> fxList = hydra.core.extract.Model.list(graph,
                ((Either.Right<Error_, Term>) fx).value);
            if (fxList.isLeft()) return (Either) fxList;
            ConsList<Term> reversed = ConsList.empty();
            for (Term y : ((Either.Right<Error_, List<Term>>) fxList).value) {
                Either<Error_, Term> r = hydra.core.Reduction.reduceTerm(
                    hydra.core.Lexical.emptyInferenceContext(), graph, true, Terms.apply(g, y));
                if (r.isLeft()) return (Either) r;
                Either<Error_, List<Term>> inner = hydra.core.extract.Model.list(graph,
                    ((Either.Right<Error_, Term>) r).value);
                if (inner.isLeft()) return (Either) inner;
                for (Term z : ((Either.Right<Error_, List<Term>>) inner).value) {
                    reversed = ConsList.cons(z, reversed);
                }
            }
            return Either.right(Terms.list(reversed.reverse()));
        };
    }

    /**
     * Kleisli composition for the list monad: compose(f, g, x) = bind(f(x), g).
     * @param <A> the input type
     * @param <B> the intermediate element type
     * @param <C> the result element type
     * @param f the first list-returning function
     * @param g the second list-returning function
     * @param x the input value
     * @return the concatenation of g(y) for each y in f(x)
     */
    public static <A, B, C> List<C> apply(Function<A, List<B>> f, Function<B, List<C>> g, A x) {
        return Bind.apply(f.apply(x), g);
    }
}
