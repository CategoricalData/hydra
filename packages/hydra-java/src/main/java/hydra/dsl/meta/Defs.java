package hydra.dsl.meta;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.packaging.Definition;
import hydra.packaging.ModuleDependency;
import hydra.packaging.ModuleName;
import hydra.packaging.PackageName;
import hydra.packaging.TermDefinition;
import hydra.phantoms.TTerm;
import hydra.typing.TermSignature;
import hydra.util.Maybe;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

/**
 * Meta-DSL helpers for assembling Hydra module definitions in Java.
 *
 * <p>Mirrors the role of the {@code define}/{@code definitionInNamespace}
 * idiom in the Haskell DSL. Each Hydra definition lives in a containing
 * source class as a {@link Def} field. The {@code Def} carries an
 * eagerly-computed {@link Name} and a lazily-computed {@link Definition}
 * — the laziness lets two definitions reference each other regardless of
 * source order.
 *
 * <p>Typical usage in a source class:
 * <pre>
 *   public class Coder {
 *     public static final ModuleName NS = new ModuleName("hydra.java.coder");
 *
 *     private static Def define(String localName, Supplier&lt;TTerm&lt;?&gt;&gt; body) {
 *       return Defs.define(NS, localName, body);
 *     }
 *
 *     public static final Def addComment = define("addComment", () -&gt;
 *       lambda("decl", "field", ref(Coder.applyJavaArg)));
 *     public static final Def applyJavaArg = define("applyJavaArg", () -&gt;
 *       ...);
 *
 *     public static final List&lt;Definition&gt; DEFINITIONS = Defs.definitionsOf(
 *       Coder.addComment, Coder.applyJavaArg);
 *   }
 * </pre>
 */
public final class Defs {
    private Defs() {}

    /**
     * A Hydra term definition with a deferred body.
     *
     * <p>{@link #name()} returns the fully-qualified {@link Name} eagerly
     * (available from the moment the {@code Def} field is initialized).
     * {@link #definition()} materializes the {@link Definition} on first
     * call by invoking the body supplier, caching the result.
     *
     * <p>The deferral lets a {@code Def} field's initializer reference
     * other {@code Def} fields declared later in the same class — the
     * lambda passed to the constructor captures them by reference, and
     * the body isn't built until {@link #definition()} is called, after
     * all fields are initialized.
     */
    public static final class Def {
        private final Name fqName;
        private final Supplier<Term> bodySupplier;
        private volatile Definition cached;

        Def(ModuleName ns, String localName, Supplier<TTerm<?>> bodyBuilder) {
            this.fqName = new Name(ns.value + "." + localName);
            this.bodySupplier = () -> bodyBuilder.get().value;
        }

        /** The fully-qualified Name of this definition. */
        public Name name() {
            return fqName;
        }

        /** The Definition. Builds it on first call (lazy). */
        public Definition definition() {
            Definition d = cached;
            if (d == null) {
                synchronized (this) {
                    d = cached;
                    if (d == null) {
                        d = new Definition.Term(new TermDefinition(
                            fqName,
                            bodySupplier.get(),
                            Maybe.<TermSignature>nothing()));
                        cached = d;
                    }
                }
            }
            return d;
        }
    }

    /** Construct a {@link Def} for the given namespace. */
    public static Def define(ModuleName ns, String localName, Supplier<TTerm<?>> body) {
        return new Def(ns, localName, body);
    }

    /**
     * Reference another {@link Def} by its fully-qualified name —
     * equivalent to {@code variable(d.name())}. Safe at any time,
     * including inside another {@code Def}'s body supplier (only
     * {@code d.name()} is read, which does not depend on the body).
     */
    public static <A> TTerm<A> ref(Def d) {
        return Phantoms.var(d.name());
    }

    /**
     * Collect {@link Def} fields into a {@code List<Definition>} for a
     * source module's DEFINITIONS list. Invokes {@link Def#definition()}
     * on each, which forces lazy body construction.
     */
    public static List<Definition> definitionsOf(Def... defs) {
        ArrayList<Definition> out = new ArrayList<>(defs.length);
        for (Def d : defs) {
            out.add(d.definition());
        }
        return out;
    }

    /** Construct a {@link ModuleDependency} on the given module, without a package qualifier. */
    public static ModuleDependency unqualifiedDep(ModuleName module) {
        return new ModuleDependency(module, Maybe.<PackageName>nothing());
    }

    /** Wrap a list of {@link ModuleName}s as a list of unqualified {@link ModuleDependency} values. */
    public static List<ModuleDependency> unqualifiedDeps(ModuleName... modules) {
        ArrayList<ModuleDependency> out = new ArrayList<>(modules.length);
        for (ModuleName m : modules) {
            out.add(unqualifiedDep(m));
        }
        return out;
    }
}
