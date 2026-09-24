package hydra.sources.java;
import hydra.core.model.Field;
import hydra.core.overlay.java.dsl.meta.Defs;
import hydra.core.overlay.java.dsl.meta.Defs.Def;
import static hydra.core.overlay.java.dsl.meta.Defs.define;
import static hydra.core.overlay.java.dsl.meta.Defs.definitionsOf;
import static hydra.core.overlay.java.dsl.meta.Defs.unqualifiedDeps;
import hydra.core.model.Name;
import hydra.core.model.Type;
import hydra.core.dsl.Core;
import hydra.core.dsl.Packaging;
import hydra.core.overlay.java.dsl.Types;
import hydra.java.dsl.Environment;
import hydra.java.dsl.Syntax;
import hydra.core.dsl.lib.Eithers;
import hydra.core.dsl.lib.Equality;
import hydra.core.dsl.lib.Lists;
import hydra.core.dsl.lib.Literals;
import hydra.core.dsl.lib.Logic;
import hydra.core.dsl.lib.Maps;
import hydra.core.dsl.lib.Math_;
import hydra.core.dsl.lib.Optionals;
import hydra.core.dsl.lib.Pairs;
import hydra.core.dsl.lib.Sets;
import hydra.core.dsl.lib.Strings;
import hydra.core.packaging.Definition;
import hydra.core.packaging.EntityMetadata;
import hydra.core.packaging.Module;
import hydra.core.packaging.ModuleName;
import hydra.core.packaging.ModuleDependency;
import hydra.core.typed.TypedTerm;
import hydra.core.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Supplier;

import static hydra.core.overlay.java.dsl.Phantoms.*;

/**
 * Language constraints and reserved words for Java.
 *
 * <p>Mirror of
 * {@code packages/hydra-java/src/main/haskell/Hydra/Sources/Java/Language.hs}.</p>
 */
public class Language {
    public static final ModuleName NS = new ModuleName("hydra.java.language");

    private static Def def(String localName, Supplier<TypedTerm<?>> body) {
        return define(NS, localName, body);
    }

    /** Fluent form: {@code def("name").doc("...").lam("x").to(() -> body)}. See Defs.DefBuilder. */
    private static Defs.DefBuilder def(String localName) {
        return define(NS, localName);
    }

    /** Build an injection of a unit-tagged variant. Mirrors the inlined form
     * that {@code hydra.core.dsl.variants.*} TypedTerm constants expand to in the
     * canonical JSON. */
    private static TypedTerm<?> variant(String typeName, String fieldName) {
        return inject(typeName, fieldName);
    }
    private static TypedTerm<?> literalVariant(String which) {
        return variant("hydra.core.variants.LiteralVariant", which);
    }
    private static TypedTerm<?> termVariantInj(String which) {
        return variant("hydra.core.variants.TermVariant", which);
    }
    private static TypedTerm<?> typeVariantInj(String which) {
        return variant("hydra.core.variants.TypeVariant", which);
    }
    private static TypedTerm<?> floatType(String which) {
        return variant("hydra.core.model.FloatType", which);
    }
    private static TypedTerm<?> integerType(String which) {
        return variant("hydra.core.model.IntegerType", which);
    }
    /** {@code Coders.language(name, constraints, features, caseConv, ext)} expands to a Record term. */
    private static TypedTerm<?> codersLanguage(TypedTerm<?> name, TypedTerm<?> constraints,
            TypedTerm<?> supportedFeatures, TypedTerm<?> caseConventions, TypedTerm<?> defaultFileExtension) {
        return record("hydra.core.coders.Language",
            field("name", name),
            field("constraints", constraints),
            field("supportedFeatures", supportedFeatures),
            field("caseConventions", caseConventions),
            field("defaultFileExtension", defaultFileExtension));
    }
    /** Construct a CaseConventions record (10 case-convention fields). */
    private static TypedTerm<?> codersCaseConventions(
            TypedTerm<?> constant, TypedTerm<?> directory, TypedTerm<?> enumValue,
            TypedTerm<?> field_, TypedTerm<?> file, TypedTerm<?> module,
            TypedTerm<?> term, TypedTerm<?> termVariable, TypedTerm<?> type_,
            TypedTerm<?> typeVariable) {
        return record("hydra.core.coders.CaseConventions",
            field("constant", constant),
            field("directory", directory),
            field("enumValue", enumValue),
            field("field", field_),
            field("file", file),
            field("module", module),
            field("term", term),
            field("termVariable", termVariable),
            field("type", type_),
            field("typeVariable", typeVariable));
    }
    /** Inject the named LanguageFeature variant (unit-tagged). */
    private static TypedTerm<?> codersLanguageFeature(String variant) {
        return inject("hydra.core.coders.LanguageFeature", variant);
    }
    /** Inject the named CaseConvention variant (unit-tagged). */
    private static TypedTerm<?> codersCaseConvention(String variant) {
        return inject("hydra.core.util.CaseConvention", variant);
    }
    /** Wrap a string as a FileExtension. */
    private static TypedTerm<?> codersFileExtension(String ext) {
        return wrap("hydra.core.file.FileExtension", string(ext));
    }
    /** {@code Coders.languageName2(x)} expands to a Wrap term. */
    private static TypedTerm<?> codersLanguageName2(TypedTerm<?> x) {
        return wrap("hydra.core.coders.LanguageName", x);
    }
    /** {@code Coders.languageConstraints2(...)} expands to a Record term. */
    private static TypedTerm<?> codersLanguageConstraints2(
            TypedTerm<?> literalVariants,
            TypedTerm<?> floatTypes,
            TypedTerm<?> integerTypes,
            TypedTerm<?> termVariants,
            TypedTerm<?> typeVariants,
            TypedTerm<?> types) {
        return record("hydra.core.coders.LanguageConstraints",
            field("literalVariants", literalVariants),
            field("floatTypes", floatTypes),
            field("integerTypes", integerTypes),
            field("termVariants", termVariants),
            field("typeVariants", typeVariants),
            field("types", types));
    }
    /** {@code Sets.fromList xs}: typeApplication of the kernel primitive
     * (so the element type is recorded), then applied to xs. Matches the
     * canonical encoding produced by the Haskell DSL. */
    private static TypedTerm<?> setsFromList(String elementTypeName, TypedTerm<?> listTerm) {
        return setsFromList(hydra.core.overlay.java.dsl.Types.variable(elementTypeName), listTerm);
    }
    @SuppressWarnings("unchecked")
    private static TypedTerm<?> setsFromList(hydra.core.model.Type elementType, TypedTerm<?> listTerm) {
        // The generated hydra.core.dsl.lib.Sets.fromList is precisely typed
        // (TypedTerm<List<X>> -> TypedTerm<Set<X>>), tighter than the old
        // hand-written wrapper's TypedTerm<?>. The phantom type is erased in the
        // emitted term, so this unchecked cast at the loosely-typed call boundary
        // is sound. See #467.
        return Sets.fromList((TypedTerm<java.util.List<Object>>) (TypedTerm<?>) listTerm);
    }

    public static final Def javaMaxTupleLength = def("javaMaxTupleLength")
        .doc("The maximum supported length of a tuple in Hydra-Java. "
            + "Note: if this constant is changed, also change Tuples.java correspondingly")
        .to(() ->
                int32(9));

    /** The {@code javaLanguage} definition: a {@code Language} record value. */
    // Mirror Haskell `lets [...] $ Coders.language ...`.
    public static final Def javaLanguage = def("javaLanguage")
        .doc("Language constraints for Java")
        .to(() ->
            let(
                binds(
                    field("literalVariants", setsFromList(
                        "hydra.core.variants.LiteralVariant",
                        list(
                            literalVariant("binary"),
                            literalVariant("boolean"),
                            literalVariant("decimal"),
                            literalVariant("float"),
                            literalVariant("integer"),
                            literalVariant("string")))),
                    field("floatTypes", setsFromList(
                        "hydra.core.model.FloatType",
                        list(
                            floatType("float32"),
                            floatType("float64")))),
                    field("integerTypes", setsFromList(
                        "hydra.core.model.IntegerType",
                        list(
                            integerType("bigint"),
                            integerType("int8"),
                            integerType("int16"),
                            integerType("int32"),
                            integerType("int64"),
                            integerType("uint16")))),
                    field("termVariants", setsFromList(
                        "hydra.core.variants.TermVariant",
                        list(
                            termVariantInj("application"),
                            termVariantInj("either"),
                            termVariantInj("cases"),
                            termVariantInj("lambda"),
                            termVariantInj("project"),
                            termVariantInj("unwrap"),
                            termVariantInj("typeApplication"),
                            termVariantInj("typeLambda"),
                            termVariantInj("let"),
                            termVariantInj("list"),
                            termVariantInj("literal"),
                            termVariantInj("map"),
                            termVariantInj("optional"),
                            termVariantInj("pair"),
                            termVariantInj("record"),
                            termVariantInj("set"),
                            termVariantInj("inject"),
                            termVariantInj("unit"),
                            termVariantInj("variable"),
                            termVariantInj("wrap")))),
                    field("typeVariants", setsFromList(
                        "hydra.core.variants.TypeVariant",
                        list(
                            typeVariantInj("annotated"),
                            typeVariantInj("application"),
                            typeVariantInj("either"),
                            typeVariantInj("effect"),
                            typeVariantInj("function"),
                            typeVariantInj("forall"),
                            typeVariantInj("list"),
                            typeVariantInj("literal"),
                            typeVariantInj("map"),
                            typeVariantInj("optional"),
                            typeVariantInj("pair"),
                            typeVariantInj("record"),
                            typeVariantInj("set"),
                            typeVariantInj("union"),
                            typeVariantInj("unit"),
                            typeVariantInj("variable"),
                            typeVariantInj("void"),
                            typeVariantInj("wrap")))),
                    field("typePredicate", lambda("_", bool(true)))),
                codersLanguage(
                    codersLanguageName2(string("hydra.java")),
                    codersLanguageConstraints2(
                        var("literalVariants"),
                        var("floatTypes"),
                        var("integerTypes"),
                        var("termVariants"),
                        var("typeVariants"),
                        var("typePredicate")),
                    setsFromList("hydra.core.coders.LanguageFeature",
                        list(codersLanguageFeature("nestedCaseStatements"))),
                    codersCaseConventions(
                        codersCaseConvention("upperSnake"), codersCaseConvention("camel"),
                        codersCaseConvention("upperSnake"), codersCaseConvention("camel"),
                        codersCaseConvention("pascal"),     codersCaseConvention("camel"),
                        codersCaseConvention("camel"),     codersCaseConvention("camel"),
                        codersCaseConvention("pascal"),    codersCaseConvention("pascal")),
                    codersFileExtension("java"))));

    private static TypedTerm<?> stringList(String... strs) {
        TypedTerm<?>[] terms = new TypedTerm<?>[strs.length];
        for (int i = 0; i < strs.length; i++) {
            terms[i] = string(strs[i]);
        }
        return list(terms);
    }

    public static final Def reservedWords = def("reservedWords")
        .doc("A set of reserved words in Java")
        .to(() -> {
        TypedTerm<?> specialNames = doc(
            "Special names reserved for use by Hydra",
            stringList("Elements", "PartialVisitor", "Visitor"));
        TypedTerm<?> classNames = doc(
            "java.lang classes as of JDK 7\n"
                + "See: https://docs.oracle.com/javase/7/docs/api/java/lang/package-summary.html",
            stringList(
                "AbstractMethodError", "Appendable", "ArithmeticException", "ArrayIndexOutOfBoundsException",
                "ArrayStoreException", "AssertionError", "AutoCloseable", "Boolean", "BootstrapMethodError", "Byte",
                "CharSequence", "Character", "Class", "ClassCastException", "ClassCircularityError", "ClassFormatError",
                "ClassLoader", "ClassNotFoundException", "ClassValue", "CloneNotSupportedException", "Cloneable", "Comparable",
                "Compiler", "Deprecated", "Double", "Enum", "EnumConstantNotPresentException", "Error", "Exception",
                "ExceptionInInitializerError", "Float", "IllegalAccessError", "IllegalAccessException",
                "IllegalArgumentException", "IllegalMonitorStateException", "IllegalStateException",
                "IllegalThreadStateException", "IncompatibleClassChangeError", "IndexOutOfBoundsException",
                "InheritableThreadLocal", "InstantiationError", "InstantiationException", "Integer", "InternalError",
                "InterruptedException", "Iterable", "LinkageError", "Long", "Math", "NegativeArraySizeException",
                "NoClassDefFoundError", "NoSuchFieldError", "NoSuchFieldException", "NoSuchMethodError", "NoSuchMethodException",
                "NullPointerException", "Number", "NumberFormatException", "Object", "OutOfMemoryError", "Override", "Package",
                "Process", "ProcessBuilder", "Readable", "ReflectiveOperationException", "Runnable", "Runtime",
                "RuntimeException", "RuntimePermission", "SafeVarargs", "SecurityException", "SecurityManager", "Short",
                "StackOverflowError", "StackTraceElement", "StrictMath", "String", "StringBuffer", "StringBuilder",
                "StringIndexOutOfBoundsException", "SuppressWarnings", "System", "Thread", "ThreadDeath",
                "ThreadGroup", "ThreadLocal", "Throwable", "TypeNotPresentException",
                "UnknownError", "UnsatisfiedLinkError", "UnsupportedClassVersionError",
                "UnsupportedOperationException", "VerifyError", "VirtualMachineError", "Void"));
        TypedTerm<?> keywords = doc(
            "Keywords and literals are taken from Oracle's Java Tutorials on 2022-05-27; said to be complete for Java 1.8 only\n"
                + "See: https://docs.oracle.com/javase/tutorial/java/nutsandbolts/_keywords.html",
            stringList(
                "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue",
                "default", "do", "double", "else", "enum", "extends", "final", "finally", "float", "for", "goto", "if",
                "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "package", "private",
                "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
                "throw", "throws", "transient", "try", "void", "volatile", "while"));
        TypedTerm<?> literals = stringList("false", "null", "true");

        return
            let(
                binds(
                    field("specialNames", specialNames),
                    field("classNames", classNames),
                    field("keywords", keywords),
                    field("literals", literals)),
                setsFromList(hydra.core.overlay.java.dsl.Types.string(), Lists.concat(list(
                        var("specialNames"),
                        var("classNames"),
                        var("keywords"),
                        var("literals")))));
        });

    private static final Def[] ALL_DEFS = {
            javaMaxTupleLength,
            javaLanguage,
            reservedWords
    };

    static {
        Defs.checkComplete(Language.class, ALL_DEFS);
    }

    private static final List<Definition> DEFINITIONS = definitionsOf(ALL_DEFS);

    // Haskell: moduleDependencies = [Lexical.ns] L.++ KernelTypes.kernelTypesNamespaces
    private static final List<ModuleDependency> DEPENDENCIES = unqualifiedDeps(
        new ModuleName("hydra.core.lexical"),
        new ModuleName("hydra.core.paths"),
        new ModuleName("hydra.core.ast"),
        new ModuleName("hydra.core.classes"),
        new ModuleName("hydra.core.coders"),
        new ModuleName("hydra.core.model"),
        new ModuleName("hydra.core.error.checking"),
        new ModuleName("hydra.core.error.model"),
        new ModuleName("hydra.core.error.packaging"),
        new ModuleName("hydra.core.errors"),
        new ModuleName("hydra.core.graph"),
        new ModuleName("hydra.core.json.model"),
        new ModuleName("hydra.core.packaging"),
        new ModuleName("hydra.core.parsing"),
        new ModuleName("hydra.core.query"),
        new ModuleName("hydra.core.relational"),
        new ModuleName("hydra.core.tabular"),
        new ModuleName("hydra.core.testing"),
        new ModuleName("hydra.core.topology"),
        new ModuleName("hydra.core.typed"),
        new ModuleName("hydra.core.typing"),
        new ModuleName("hydra.core.util"),
        new ModuleName("hydra.core.validation"),
        new ModuleName("hydra.core.variants"));

    public static final Module module_ = new Module(
        NS,
        Optional.given(new EntityMetadata(
            Optional.given("Language constraints and reserved words for Java"),
            java.util.List.of(),
            java.util.List.of(),
            Optional.none(),
            java.util.List.of())),
        DEPENDENCIES,
        DEFINITIONS);
}
