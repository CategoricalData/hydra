package hydra.core.overlay.java.tools;

import hydra.core.Reduction;
import hydra.core.model.Application;
import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.errors.Error_;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.util.Either;
import hydra.core.packaging.PrimitiveDefinition;
import hydra.core.typing.InferenceContext;

import java.util.List;
import java.util.function.Function;


/**
 * A {@link PrimitiveFunction} for a kernel primitive which has no native Java implementation,
 * but does declare a portable, cross-compilable {@code defaultImplementation} term (see
 * {@code hydra.core.lib.Defaults.defaultImplementations()}). Its {@link #implementation()} evaluates
 * that term against the call arguments via {@link Reduction#reduceTerm}, rather than running
 * hand-written Java logic.
 *
 * <p>Note: unlike the Haskell kernel source ({@code Lib/Defaults.hs}), which stores these terms
 * reified as data (requiring a decode step) because a single {@code TermMap} can't hold
 * heterogeneously-typed executable terms, the Java code generator resolves this reification at
 * generation time — {@code Defaults.defaultImplementations()} already yields real, directly
 * reducible {@link Term} values. No decode step is needed or possible here (there is nothing to
 * decode: the values are not {@code Term.Inject}-shaped).
 */
public class DefaultFallbackPrimitiveFunction extends PrimitiveFunction {
    private final PrimitiveDefinition definition;
    private final Term defaultImplementation;

    public DefaultFallbackPrimitiveFunction(PrimitiveDefinition definition, Term defaultImplementation) {
        this.definition = definition;
        this.defaultImplementation = defaultImplementation;
    }

    @Override
    public Name name() {
        return definition.name;
    }

    @Override
    public TypeScheme type() {
        return hydra.core.Scoping.termSignatureToTypeScheme(definition.signature);
    }

    @Override
    protected boolean isPure() {
        return definition.isPure;
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            Term applied = defaultImplementation;
            for (Term arg : args) {
                applied = new Term.Application(new Application(applied, arg));
            }
            InferenceContext cx = new InferenceContext(0, java.util.Collections.emptyList());
            return Reduction.<InferenceContext>reduceTerm(cx, graph, Boolean.TRUE, applied);
        };
    }
}
