package hydra.overlay.java.dsl.meta;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.packaging.EntityMetadata;
import hydra.packaging.Definition;
import hydra.packaging.ModuleDependency;
import hydra.packaging.ModuleName;
import hydra.packaging.PackageName;
import hydra.packaging.TermDefinition;
import hydra.typed.TypedTerm;
import hydra.typing.TermSignature;
import hydra.overlay.java.util.Optional;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.IdentityHashMap;
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
 *     private static Def define(String localName, Supplier&lt;TypedTerm&lt;?&gt;&gt; body) {
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

        Def(ModuleName ns, String localName, Supplier<TypedTerm<?>> bodyBuilder) {
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
                            Optional.<EntityMetadata>none(),
                            Optional.<TermSignature>none(),
                            bodySupplier.get()));
                        cached = d;
                    }
                }
            }
            return d;
        }
    }

    /** Construct a {@link Def} for the given namespace. */
    public static Def define(ModuleName ns, String localName, Supplier<TypedTerm<?>> body) {
        return new Def(ns, localName, body);
    }

    /**
     * Begin a fluent definition: {@code define(NS, "name").doc("...").lam("x").lam("y").to(() -> body)}.
     *
     * <p>Reads top-to-bottom (declaration order) instead of the inside-out
     * {@code define("name", () -> doc("...", lambda("x", lambda("y", body))))} nesting. The terminal
     * {@link DefBuilder#to} yields the same {@link Def} the flat form would: it composes the recorded
     * doc + lambda parameters around the body as {@code doc(description, lambda([params], body))},
     * omitting the {@code doc}/{@code lambda} wrappers when none were specified. The body stays lazy
     * (passed as a {@link Supplier}) so cross-referencing {@code Def} fields resolve regardless of
     * source order, exactly as with {@link #define(ModuleName, String, Supplier)}.
     */
    public static DefBuilder define(ModuleName ns, String localName) {
        return new DefBuilder(ns, localName);
    }

    /**
     * Fluent builder for a {@link Def}. Records an optional doc description and zero or more lambda
     * parameters, then {@link #to} closes over a lazy body to produce the {@code Def}. See
     * {@link #define(ModuleName, String)}.
     */
    public static final class DefBuilder {
        private final ModuleName ns;
        private final String localName;
        private String description;
        private final ArrayList<String> params = new ArrayList<>();

        DefBuilder(ModuleName ns, String localName) {
            this.ns = ns;
            this.localName = localName;
        }

        /** Attach a doc description, wrapping the eventual body in {@code doc(description, ...)}. */
        public DefBuilder doc(String description) {
            this.description = description;
            return this;
        }

        /** Add one lambda parameter (applied outermost-first, matching {@code lambda("x", lambda("y", ...))}). */
        public DefBuilder lam(String param) {
            this.params.add(param);
            return this;
        }

        /** Add several lambda parameters in order (equivalent to chained {@link #lam} calls). */
        public DefBuilder lams(String... ps) {
            for (String p : ps) {
                this.params.add(p);
            }
            return this;
        }

        /** Close over the (lazy) body and produce the {@link Def}. */
        public Def to(Supplier<TypedTerm<?>> body) {
            List<String> ps = new ArrayList<>(params);
            String desc = description;
            return new Def(ns, localName, () -> {
                TypedTerm<?> t = ps.isEmpty() ? body.get() : Phantoms.lambda(ps, body.get());
                return desc == null ? t : Phantoms.doc(desc, t);
            });
        }
    }

    /**
     * Reference another {@link Def} by its fully-qualified name —
     * equivalent to {@code variable(d.name())}. Safe at any time,
     * including inside another {@code Def}'s body supplier (only
     * {@code d.name()} is read, which does not depend on the body).
     */
    public static <A> TypedTerm<A> ref(Def d) {
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

    /**
     * Verify that every {@code static final Def} field declared directly in {@code sourceClass} is
     * among the given {@code registered} defs (typically the same array passed to
     * {@link #definitionsOf(Def...)}). Throws {@link IllegalStateException} naming any orphaned field
     * — a {@code Def} authored in the class but never added to the module's assembly list, which would
     * otherwise silently vanish from the generated module.
     *
     * <p>Call once per source class, immediately after building {@code DEFINITIONS}:
     * <pre>
     *   private static final List&lt;Definition&gt; DEFINITIONS = definitionsOf(fooDef, barDef);
     *   static { Defs.checkComplete(Names.class, fooDef, barDef); }
     * </pre>
     *
     * <p>Only the omission direction is checked: a registered {@code Def} that isn't a field of
     * {@code sourceClass} is not an error (e.g. a def imported from a sibling class), so this is not a
     * full set-equality check.
     */
    public static void checkComplete(Class<?> sourceClass, Def... registered) {
        IdentityHashMap<Def, Boolean> registeredSet = new IdentityHashMap<>(registered.length);
        for (Def d : registered) {
            registeredSet.put(d, Boolean.TRUE);
        }
        List<String> orphans = new ArrayList<>();
        for (Field field : sourceClass.getDeclaredFields()) {
            if (!Modifier.isStatic(field.getModifiers()) || field.getType() != Def.class) {
                continue;
            }
            try {
                field.setAccessible(true);
                Def value = (Def) field.get(null);
                if (value != null && !registeredSet.containsKey(value)) {
                    orphans.add(field.getName());
                }
            } catch (IllegalAccessException e) {
                throw new IllegalStateException(
                    "Defs.checkComplete: could not read field " + field.getName()
                        + " of " + sourceClass.getName(), e);
            }
        }
        if (!orphans.isEmpty()) {
            throw new IllegalStateException(
                "Defs.checkComplete: " + sourceClass.getName()
                    + " declares Def field(s) missing from its DEFINITIONS list: "
                    + String.join(", ", orphans)
                    + ". Add each to the definitionsOf(...) call, or remove the unused field.");
        }
    }

    /** Construct a {@link ModuleDependency} on the given module, without a package qualifier. */
    public static ModuleDependency unqualifiedDep(ModuleName module) {
        return new ModuleDependency(module, Optional.<PackageName>none());
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
