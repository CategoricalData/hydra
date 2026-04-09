package hydra.tools;

import hydra.context.Context;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.errors.Error_;
import hydra.errors.Error_;
import hydra.graph.Graph;
import hydra.graph.Primitive;
import hydra.util.Either;

import java.util.List;
import java.util.function.Function;


/**
 * Any of Hydra's primitive functions, implemented in Java.
 */
public abstract class PrimitiveFunction {
    /**
     * The unique name of the primitive function.
     * @return the name of this primitive function
     */
    public abstract Name name();

    /**
     * The datatype of the primitive function.
     * @return the type scheme of this primitive function
     */
    public abstract TypeScheme type();

    /**
     * A dynamic/interpreted implementation of the function.
     * Subclasses implement this with Either-based logic.
     * @return the function implementation
     */
    protected abstract Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation();

    /**
     * The primitive function as a term.
     * @return the primitive function as a Hydra term
     */
    public Term term() {
        return hydra.dsl.Terms.primitive(name());
    }

    /**
     * The primitive function as a native Hydra Primitive object.
     * @return the primitive function as a Hydra Primitive object
     */
    public Primitive toNative() {
        Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> impl = implementation();
        Function<Context, Function<Graph, Function<List<Term>, Either<Error_, Term>>>> nativeImpl =
            cx -> graph -> args -> {
                Either<Error_, Term> result = impl.apply(args).apply(cx).apply(graph);
                if (result.isRight()) {
                    return Either.right(((Either.Right<Error_, Term>) result).value);
                } else {
                    Error_ ic = ((Either.Left<Error_, Term>) result).value;
                    return Either.left(ic);
                }
            };
        return new Primitive(name(), type(), nativeImpl);
    }
}
