package hydra.core.overlay.java.dsl;
import hydra.core.Scoping;
import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.Type;
import hydra.core.model.TypeScheme;
import hydra.core.model.TypeVariableConstraints;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.packaging.EntityMetadata;
import hydra.core.packaging.Definition;
import hydra.core.packaging.ModuleName;
import hydra.core.packaging.TermDefinition;
import hydra.core.packaging.TypeDefinition;
import hydra.core.typing.TermSignature;
import hydra.core.overlay.java.util.Optional;

import java.util.Collections;

/**
 * Hand-written Java DSL helpers for assembling Hydra type and term
 * definitions in the Java-language source modules ({@code Coder.java},
 * {@code Serde.java}, etc.). Mirror of Haskell's
 * {@code Hydra.Java.Dsl.Helpers}.
 *
 * <p>Type-level helpers ({@link #typeref}, {@link #doc(String, Type)},
 * {@link #typeDef}) build the records, unions, and wraps that source
 * modules expose as Hydra types. Term-level helpers ({@link #termDef},
 * {@link #docTerm}) construct {@link Definition} instances directly.
 *
 * <p>For the deferred-body {@code Def} wrapper used by auto-ported source
 * modules, see {@link hydra.core.overlay.java.dsl.meta.Defs}.
 */
public final class Helpers {
    private Helpers() {}

    /** Hydra type annotation key for human-readable docs. */
    public static final Name DESCRIPTION = new Name("description");

    /** Attach a "description" annotation to a Type. */
    public static Type doc(String description, Type base) {
        return Types.annot(DESCRIPTION, Terms.string(description), base);
    }

    /** Attach a "description" annotation to a Term. */
    public static Term docTerm(String description, Term base) {
        return Terms.annot(description, base);
    }

    /**
     * Construct a TypeVariable reference for a fully-qualified name.
     * {@code typeref("hydra.core.model", "Type")} → {@code Type.Variable("hydra.core.model.Type")}.
     */
    public static Type typeref(ModuleName ns, String local) {
        return Types.variable(ns.value + "." + local);
    }

    /** Same as {@link #typeref(ModuleName, String)} but for a String namespace. */
    public static Type typeref(String ns, String local) {
        return Types.variable(ns + "." + local);
    }

    /** Build a type Definition in the given namespace. */
    public static Definition typeDef(ModuleName ns, String localName, Type typ) {
        Name fqName = new Name(ns.value + "." + localName);
        TypeScheme ts = new TypeScheme(
            Collections.emptyList(),
            typ,
            Collections.<Name, TypeVariableConstraints>emptyMap());
        return new Definition.Type(new TypeDefinition(fqName, Optional.<EntityMetadata>none(), ts));
    }

    /**
     * Build a term Definition in the given namespace with no type scheme
     * (inference fills it in).
     */
    public static Definition termDef(ModuleName ns, String localName, Term term) {
        Name fqName = new Name(ns.value + "." + localName);
        return new Definition.Term(new TermDefinition(
            fqName,
            Optional.<EntityMetadata>none(),
            Optional.<TermSignature>none(),
            term));
    }

    /** Build a term Definition with a pre-computed TypeScheme. */
    public static Definition termDefTyped(ModuleName ns, String localName, Term term, TypeScheme ts) {
        Name fqName = new Name(ns.value + "." + localName);
        return new Definition.Term(new TermDefinition(
            fqName,
            Optional.<EntityMetadata>none(),
            Optional.<TermSignature>given(Scoping.typeSchemeToTermSignature(ts)),
            term));
    }

    /** Build a TypeScheme from a variables list and a body type. */
    public static TypeScheme typeScheme(java.util.List<Name> variables, Type body) {
        return new TypeScheme(
            variables,
            body,
            Collections.<Name, TypeVariableConstraints>emptyMap());
    }
}
