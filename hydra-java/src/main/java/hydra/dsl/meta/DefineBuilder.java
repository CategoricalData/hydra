package hydra.dsl.meta;

import hydra.core.Name;
import hydra.phantoms.TBinding;
import hydra.phantoms.TTerm;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Fluent builder that starts with a definition name and produces a TBinding.
 *
 * <p>Usage:
 * <pre>
 *   define("freeVariablesInType")
 *     .doc("Find the free variables...")
 *     .lam("typ")
 *     .let("dfltVars", expr)
 *     .to(body)
 * </pre>
 *
 * <p>Mirrors Python's {@code DefineBuilder} in {@code hydra.dsl.meta.phantoms}.
 */
public class DefineBuilder {

    private final Name name;
    private final List<Intro> intros;

    DefineBuilder(Name name) {
        this.name = name;
        this.intros = Collections.emptyList();
    }

    private DefineBuilder(Name name, List<Intro> intros) {
        this.name = name;
        this.intros = intros;
    }

    /** Add a doc annotation. */
    public DefineBuilder doc(String s) {
        List<Intro> next = new ArrayList<>(intros);
        next.add(Intro.doc(s));
        return new DefineBuilder(name, next);
    }

    /** Add a lambda parameter. */
    public DefineBuilder lam(String v) {
        List<Intro> next = new ArrayList<>(intros);
        next.add(Intro.lambda(v));
        return new DefineBuilder(name, next);
    }

    /** Add multiple lambda parameters. */
    public DefineBuilder lams(String... vs) {
        List<Intro> next = new ArrayList<>(intros);
        for (String v : vs) {
            next.add(Intro.lambda(v));
        }
        return new DefineBuilder(name, next);
    }

    /** Add a single-binding let. */
    public DefineBuilder let(String name, TTerm<?> value) {
        List<Intro> next = new ArrayList<>(intros);
        next.add(Intro.let(name, value));
        return new DefineBuilder(this.name, next);
    }

    /** Finalize the chain with a body, producing a TBinding. */
    @SuppressWarnings("unchecked")
    public <A> TBinding<A> to(TTerm<?> body) {
        Expr<?> term = BuilderUtils.buildTerm(intros, body);
        return new TBinding<>(name, (TTerm<A>) term);
    }
}
