package hydra.tools;

import hydra.Scoping;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.errors.Error_;
import hydra.graph.Graph;
import hydra.graph.Primitive;
import hydra.packaging.PrimitiveDefinition;
import hydra.typing.InferenceContext;
import hydra.typing.Parameter;
import hydra.typing.TermSignature;
import hydra.util.Either;
import hydra.util.Maybe;

import java.util.ArrayList;
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
    protected abstract Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation();

    /**
     * The 0-based positions of value parameters that must be passed lazily (thunked) at call sites
     * in hosts that distinguish strict from lazy evaluation (issue #391). Defaults to none; lazy
     * primitives (e.g. maybes.cases, logic.ifElse) override this. Mirrors Hydra.Dsl.Prims.lazyArgs
     * on the Haskell host, which is the source of truth for the laziness metadata.
     * @return the lazy parameter positions
     */
    protected List<Integer> lazyParams() {
        return java.util.Collections.emptyList();
    }

    /**
     * The primitive function as a term.
     * @return the primitive function as a Hydra term
     */
    public Term term() {
        return hydra.dsl.Terms.primitive(name());
    }

    /**
     * Derive the term signature from {@link #type()} and apply this primitive's {@link #lazyParams()}
     * by marking those parameter positions lazy. typeSchemeToTermSignature cannot infer laziness (a
     * TypeScheme carries none), so the per-primitive lazyParams() is what records it for coders.
     */
    private TermSignature signatureWithLaziness() {
        TermSignature sig = Scoping.typeSchemeToTermSignature(type());
        List<Integer> lazy = lazyParams();
        if (lazy.isEmpty()) {
            return sig;
        }
        List<Parameter> params = new ArrayList<>(sig.parameters);
        for (Integer i : lazy) {
            if (i >= 0 && i < params.size()) {
                params.set(i, params.get(i).withIsLazy(Boolean.TRUE));
            }
        }
        return sig.withParameters(params);
    }

    /**
     * The primitive function as a native Hydra Primitive object.
     * @return the primitive function as a Hydra Primitive object
     */
    public Primitive toNative() {
        Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> impl = implementation();
        Function<InferenceContext, Function<Graph, Function<List<Term>, Either<Error_, Term>>>> nativeImpl =
            cx -> graph -> args -> {
                Either<Error_, Term> result = impl.apply(args).apply(cx).apply(graph);
                if (result.isRight()) {
                    return Either.right(((Either.Right<Error_, Term>) result).value);
                } else {
                    Error_ ic = ((Either.Left<Error_, Term>) result).value;
                    return Either.left(ic);
                }
            };
        PrimitiveDefinition definition = new PrimitiveDefinition(
            name(),
            Maybe.nothing(),
            signatureWithLaziness(),
            Boolean.TRUE,
            Boolean.TRUE,
            Maybe.nothing());
        return new Primitive(definition, nativeImpl);
    }
}
