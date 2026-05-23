package hydra.dsl.java;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.core.TypeVariableMetadata;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.packaging.Definition;
import hydra.packaging.ModuleName;
import hydra.packaging.TermDefinition;
import hydra.packaging.TypeDefinition;
import hydra.util.Maybe;

import java.util.Collections;
import java.util.Map;

/**
 * Hand-written Java DSL helpers for assembling Hydra type and term
 * definitions in the Java-language source modules ({@code Coder.java},
 * {@code Serde.java}, etc.). Mirror of Haskell's
 * {@code Hydra.Dsl.Java.Helpers}.
 *
 * <p>Type-level helpers ({@link #typeref}, {@link #doc(String, Type)},
 * {@link #typeDef}) build the records, unions, and wraps that source
 * modules expose as Hydra types. Term-level helpers ({@link #termDef},
 * {@link #docTerm}) construct {@link Definition} instances directly.
 *
 * <p>For the deferred-body {@code Def} wrapper used by auto-ported source
 * modules, see {@link hydra.dsl.meta.Defs}.
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
     * {@code typeref("hydra.core", "Type")} → {@code Type.Variable("hydra.core.Type")}.
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
            Maybe.<Map<Name, TypeVariableMetadata>>nothing());
        return new Definition.Type(new TypeDefinition(fqName, ts));
    }

    /**
     * Build a term Definition in the given namespace with no type scheme
     * (inference fills it in).
     */
    public static Definition termDef(ModuleName ns, String localName, Term term) {
        Name fqName = new Name(ns.value + "." + localName);
        return new Definition.Term(new TermDefinition(
            fqName,
            term,
            Maybe.<TypeScheme>nothing()));
    }

    /** Build a term Definition with a pre-computed TypeScheme. */
    public static Definition termDefTyped(ModuleName ns, String localName, Term term, TypeScheme ts) {
        Name fqName = new Name(ns.value + "." + localName);
        return new Definition.Term(new TermDefinition(
            fqName,
            term,
            Maybe.<TypeScheme>just(ts)));
    }

    /** Build a TypeScheme from a variables list and a body type. */
    public static TypeScheme typeScheme(java.util.List<Name> variables, Type body) {
        return new TypeScheme(
            variables,
            body,
            Maybe.<Map<Name, TypeVariableMetadata>>nothing());
    }
}
