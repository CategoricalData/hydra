package hydra.sources.java;

import hydra.core.Field;
import hydra.core.Name;
import hydra.core.Type;
import hydra.dsl.Core;
import hydra.dsl.Packaging;
import hydra.dsl.Types;
import hydra.dsl.java.Environment;
import hydra.dsl.java.Syntax;
import hydra.dsl.meta.lib.Eithers;
import hydra.dsl.meta.lib.Equality;
import hydra.dsl.meta.lib.Lists;
import hydra.dsl.meta.lib.Literals;
import hydra.dsl.meta.lib.Logic;
import hydra.dsl.meta.lib.Maps;
import hydra.dsl.meta.lib.Math_;
import hydra.dsl.meta.lib.Maybes;
import hydra.dsl.meta.lib.Pairs;
import hydra.dsl.meta.lib.Sets;
import hydra.dsl.meta.lib.Strings;
import hydra.packaging.Definition;
import hydra.packaging.Module;
import hydra.packaging.Namespace;
import hydra.phantoms.TTerm;
import hydra.util.Maybe;

import java.util.Arrays;
import java.util.List;

import static hydra.dsl.meta.Phantoms.*;
import static hydra.dsl.java.Helpers.termDef;

/**
 * Language constraints and reserved words for Java.
 *
 * <p>Mirror of
 * {@code packages/hydra-java/src/main/haskell/Hydra/Sources/Java/Language.hs}.</p>
 */
public class Language {
    public static final Namespace NS = new Namespace("hydra.java.language");

    /** Build an injection of a unit-tagged variant. Mirrors the inlined form
     * that {@code hydra.dsl.variants.*} TTerm constants expand to in the
     * canonical JSON. */
    private static TTerm<?> variant(String typeName, String fieldName) {
        return injectUnit(typeName, fieldName);
    }
    private static TTerm<?> literalVariant(String which) {
        return variant("hydra.variants.LiteralVariant", which);
    }
    private static TTerm<?> termVariantInj(String which) {
        return variant("hydra.variants.TermVariant", which);
    }
    private static TTerm<?> typeVariantInj(String which) {
        return variant("hydra.variants.TypeVariant", which);
    }
    private static TTerm<?> floatType(String which) {
        return variant("hydra.core.FloatType", which);
    }
    private static TTerm<?> integerType(String which) {
        return variant("hydra.core.IntegerType", which);
    }
    /** {@code Coders.language(name, constraints)} expands to a Record term. */
    private static TTerm<?> codersLanguage(TTerm<?> name, TTerm<?> constraints) {
        return record("hydra.coders.Language",
            field("name", name),
            field("constraints", constraints));
    }
    /** {@code Coders.languageName2(x)} expands to a Wrap term. */
    private static TTerm<?> codersLanguageName2(TTerm<?> x) {
        return wrap("hydra.coders.LanguageName", x);
    }
    /** {@code Coders.languageConstraints2(...)} expands to a Record term. */
    private static TTerm<?> codersLanguageConstraints2(
            TTerm<?> literalVariants,
            TTerm<?> floatTypes,
            TTerm<?> integerTypes,
            TTerm<?> termVariants,
            TTerm<?> typeVariants,
            TTerm<?> types) {
        return record("hydra.coders.LanguageConstraints",
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
    private static TTerm<?> setsFromList(String elementTypeName, TTerm<?> listTerm) {
        return setsFromList(hydra.dsl.Types.variable(elementTypeName), listTerm);
    }
    private static TTerm<?> setsFromList(hydra.core.Type elementType, TTerm<?> listTerm) {
        return Sets.fromList(listTerm);
    }

    private static Definition javaMaxTupleLength() {
        return termDef(NS, "javaMaxTupleLength", doc(
                "The maximum supported length of a tuple in Hydra-Java. "
                    + "Note: if this constant is changed, also change Tuples.java correspondingly",
                int32(9)).value);
    }

    /** The {@code javaLanguage} definition: a {@code Language} record value. */
    private static Definition javaLanguage() {
        // Mirror Haskell `lets [...] $ Coders.language ...`.
        TTerm<?> body = doc(
            "Language constraints for Java",
            let(
                Arrays.<hydra.core.Field>asList(
                    field("literalVariants", setsFromList(
                        "hydra.variants.LiteralVariant",
                        list(
                            literalVariant("binary"),
                            literalVariant("boolean"),
                            literalVariant("decimal"),
                            literalVariant("float"),
                            literalVariant("integer"),
                            literalVariant("string")))),
                    field("floatTypes", setsFromList(
                        "hydra.core.FloatType",
                        list(
                            floatType("float32"),
                            floatType("float64")))),
                    field("integerTypes", setsFromList(
                        "hydra.core.IntegerType",
                        list(
                            integerType("bigint"),
                            integerType("int8"),
                            integerType("int16"),
                            integerType("int32"),
                            integerType("int64"),
                            integerType("uint16")))),
                    field("termVariants", setsFromList(
                        "hydra.variants.TermVariant",
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
                            termVariantInj("maybe"),
                            termVariantInj("pair"),
                            termVariantInj("record"),
                            termVariantInj("set"),
                            termVariantInj("inject"),
                            termVariantInj("unit"),
                            termVariantInj("variable"),
                            termVariantInj("wrap")))),
                    field("typeVariants", setsFromList(
                        "hydra.variants.TypeVariant",
                        list(
                            typeVariantInj("annotated"),
                            typeVariantInj("application"),
                            typeVariantInj("either"),
                            typeVariantInj("function"),
                            typeVariantInj("forall"),
                            typeVariantInj("list"),
                            typeVariantInj("literal"),
                            typeVariantInj("map"),
                            typeVariantInj("maybe"),
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
                        var("typePredicate")))));
        return termDef(NS, "javaLanguage", body.value);
    }

    private static TTerm<?> stringList(String... strs) {
        TTerm<?>[] terms = new TTerm<?>[strs.length];
        for (int i = 0; i < strs.length; i++) {
            terms[i] = string(strs[i]);
        }
        return list(terms);
    }

    private static Definition reservedWords() {
        TTerm<?> specialNames = doc(
            "Special names reserved for use by Hydra",
            stringList("Elements"));
        TTerm<?> classNames = doc(
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
        TTerm<?> keywords = doc(
            "Keywords and literals are taken from Oracle's Java Tutorials on 2022-05-27; said to be complete for Java 1.8 only\n"
                + "See: https://docs.oracle.com/javase/tutorial/java/nutsandbolts/_keywords.html",
            stringList(
                "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue",
                "default", "do", "double", "else", "enum", "extends", "final", "finally", "float", "for", "goto", "if",
                "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "package", "private",
                "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
                "throw", "throws", "transient", "try", "void", "volatile", "while"));
        TTerm<?> literals = stringList("false", "null", "true");

        TTerm<?> body = doc(
            "A set of reserved words in Java",
            let(
                Arrays.<hydra.core.Field>asList(
                    field("specialNames", specialNames),
                    field("classNames", classNames),
                    field("keywords", keywords),
                    field("literals", literals)),
                setsFromList(hydra.dsl.Types.string(), Lists.concat(list(
                        var("specialNames"),
                        var("classNames"),
                        var("keywords"),
                        var("literals"))))));
        return termDef(NS, "reservedWords", body.value);
    }

    private static final List<Definition> DEFINITIONS = Arrays.asList(
        javaMaxTupleLength(),
        javaLanguage(),
        reservedWords());

    // Haskell: moduleDependencies = [Lexical.ns] L.++ KernelTypes.kernelTypesNamespaces
    private static final List<Namespace> DEPENDENCIES = Arrays.asList(
        new Namespace("hydra.lexical"),
        new Namespace("hydra.paths"),
        new Namespace("hydra.ast"),
        new Namespace("hydra.classes"),
        new Namespace("hydra.coders"),
        new Namespace("hydra.context"),
        new Namespace("hydra.core"),
        new Namespace("hydra.error.checking"),
        new Namespace("hydra.error.core"),
        new Namespace("hydra.error.packaging"),
        new Namespace("hydra.errors"),
        new Namespace("hydra.graph"),
        new Namespace("hydra.json.model"),
        new Namespace("hydra.packaging"),
        new Namespace("hydra.parsing"),
        new Namespace("hydra.phantoms"),
        new Namespace("hydra.query"),
        new Namespace("hydra.relational"),
        new Namespace("hydra.tabular"),
        new Namespace("hydra.testing"),
        new Namespace("hydra.topology"),
        new Namespace("hydra.typing"),
        new Namespace("hydra.util"),
        new Namespace("hydra.validation"),
        new Namespace("hydra.variants"));

    public static final Module module_ = new Module(
        Maybe.just("Language constraints and reserved words for Java"),
        NS,
        DEPENDENCIES,
        DEFINITIONS);
}
