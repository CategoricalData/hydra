package hydra.lib.maybes;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.errors.OtherError;
import hydra.util.Either;


/**
 * Extracts the value from Just.
 */
public class FromJust extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.fromJust"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.fromJust");
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for extracting a value from an optional
     */
    @Override
    public TypeScheme type() {
        return scheme("a", function(optional("a"), "a"));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that extracts the value from Just or fails
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.maybeTerm(cx, t -> Either.right(t), graph, args.get(0)), opt -> opt.isJust() ? Either.right(opt.fromJust()) : Either.left(new InContext<>(new Error_.Other(new OtherError("fromJust: Nothing")), cx)));
    }

    /**
     * Unwraps Just value (unsafe operation).
     * @param <X> the value type
     * @param opt the optional value
     * @return the contained value
     * @throws RuntimeException if the optional is empty
     */
    public static <X> X apply(Maybe<X> opt) {
        if (!opt.isJust()) {
            throw new RuntimeException("fromJust: Nothing");
        }
        return opt.fromJust();
    }
}
