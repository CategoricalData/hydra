package hydra.dsl.meta;

import hydra.core.Name;
import hydra.phantoms.TTerm;
import hydra.util.Maybe;

import java.util.ArrayList;
import java.util.List;

/**
 * Fluent builder for chaining doc, lambdas, and single-binding lets without nesting.
 *
 * <p>Usage:
 * <pre>
 *   lam("x")
 *     .lam("y")
 *     .let("z", expr)
 *     .to(body)
 * </pre>
 *
 * <p>Produces: {@code lambda x -> lambda y -> let z = expr in body}
 *
 * <p>Mirrors Python's {@code ExprBuilder} in {@code hydra.dsl.meta.phantoms}.
 */
public class ExprBuilder {

    // Each intro is one of:
    //   ("doc", description, null)
    //   ("lambda", paramName, null)
    //   ("let", bindingName, value)
    private final List<Intro> intros;

    ExprBuilder(List<Intro> intros) {
        this.intros = intros;
    }

    /** Add a doc annotation. */
    public ExprBuilder doc(String s) {
        List<Intro> next = new ArrayList<>(intros);
        next.add(Intro.doc(s));
        return new ExprBuilder(next);
    }

    /** Add a lambda parameter. */
    public ExprBuilder lam(String v) {
        List<Intro> next = new ArrayList<>(intros);
        next.add(Intro.lambda(v));
        return new ExprBuilder(next);
    }

    /** Add multiple lambda parameters. */
    public ExprBuilder lams(String... vs) {
        List<Intro> next = new ArrayList<>(intros);
        for (String v : vs) {
            next.add(Intro.lambda(v));
        }
        return new ExprBuilder(next);
    }

    /** Add a single-binding let. */
    public ExprBuilder let(String name, TTerm<?> value) {
        List<Intro> next = new ArrayList<>(intros);
        next.add(Intro.let(name, value));
        return new ExprBuilder(next);
    }

    /** Finalize the chain with a body, producing a TTerm. */
    @SuppressWarnings("unchecked")
    public <B> Expr<B> to(TTerm<?> body) {
        return BuilderUtils.buildTerm(intros, body);
    }
}
