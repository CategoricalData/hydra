package hydra.sources.java;
import hydra.core.model.Field;
import hydra.core.overlay.java.dsl.meta.Defs;
import hydra.core.overlay.java.dsl.meta.Defs.Def;
import static hydra.core.overlay.java.dsl.meta.Defs.define;
import static hydra.core.overlay.java.dsl.meta.Defs.definitionsOf;
import static hydra.core.overlay.java.dsl.meta.Defs.unqualifiedDeps;
import hydra.core.model.Name;
import hydra.core.model.Type;
import hydra.core.dsl.Packaging;
import hydra.core.overlay.java.dsl.Types;
import hydra.java.dsl.Environment;
import hydra.java.dsl.Syntax;
import hydra.core.overlay.java.dsl.Phantoms;
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
 * Java test code generation codec for JUnit-based generation tests.
 *
 * <p>Mirror of
 * {@code packages/hydra-java/src/main/haskell/Hydra/Sources/Java/Testing.hs}.</p>
 */
public class Testing {
    public static final ModuleName NS = new ModuleName("hydra.java.testing");

    private static Def def(String localName, Supplier<TypedTerm<?>> body) {
        return define(NS, localName, body);
    }

    /** Fluent form: {@code def("name").doc("...").lam("x").to(() -> body)}. See Defs.DefBuilder. */
    private static Defs.DefBuilder def(String localName) {
        return define(NS, localName);
    }

    // ---- Primitive references ----
    private static TypedTerm<?> prim(String fqName) { return var(fqName); }

    private static TypedTerm<?> stringsCat2(TypedTerm<?> a, TypedTerm<?> b) {
        return apply(prim("hydra.core.lib.strings.concat2"), a, b);
    }
    private static TypedTerm<?> stringsCat(TypedTerm<?> list) {
        return apply(prim("hydra.core.lib.strings.concat"), list);
    }
    private static TypedTerm<?> stringsIntercalate(TypedTerm<?> sep, TypedTerm<?> list) {
        return apply(prim("hydra.core.lib.strings.join"), sep, list);
    }
    private static TypedTerm<?> stringsSplitOn(TypedTerm<?> sep, TypedTerm<?> s) {
        return apply(prim("hydra.core.lib.strings.splitOn"), sep, s);
    }
    private static TypedTerm<?> listsConcat(TypedTerm<?> list) {
        return apply(prim("hydra.core.lib.lists.concat"), list);
    }
    private static TypedTerm<?> listsConcat2(TypedTerm<?> a, TypedTerm<?> b) {
        return apply(prim("hydra.core.lib.lists.concat2"), a, b);
    }
    private static TypedTerm<?> listsDrop(TypedTerm<?> n, TypedTerm<?> list) {
        return apply(prim("hydra.core.lib.lists.drop"), n, list);
    }
    private static TypedTerm<?> listsMap(TypedTerm<?> f, TypedTerm<?> list) {
        return apply(prim("hydra.core.lib.lists.map"), f, list);
    }
    private static TypedTerm<?> listsIsEmpty(TypedTerm<?> list) {
        return apply(prim("hydra.core.lib.lists.isEmpty"), list);
    }
    private static TypedTerm<?> listsMaybeLast(TypedTerm<?> list) {
        return apply(prim("hydra.core.lib.lists.last"), list);
    }
    private static TypedTerm<?> listsMaybeInit(TypedTerm<?> list) {
        return apply(prim("hydra.core.lib.lists.init"), list);
    }
    private static TypedTerm<?> optionalsFromOptional(TypedTerm<?> dflt, TypedTerm<?> m) {
        return apply(prim("hydra.core.lib.optionals.withDefault"), dflt, m);
    }
    private static TypedTerm<?> eithersBind(TypedTerm<?> e, TypedTerm<?> f) {
        return apply(prim("hydra.core.lib.eithers.bind"), e, f);
    }
    private static TypedTerm<?> eithersMap(TypedTerm<?> f, TypedTerm<?> e) {
        return apply(prim("hydra.core.lib.eithers.map"), f, e);
    }
    private static TypedTerm<?> eithersMapList(TypedTerm<?> f, TypedTerm<?> list) {
        return apply(prim("hydra.core.lib.eithers.mapList"), f, list);
    }
    private static TypedTerm<?> logicIfElse(TypedTerm<?> cond, TypedTerm<?> t, TypedTerm<?> f) {
        return apply(prim("hydra.core.lib.logic.ifElse"), cond, t, f);
    }
    private static TypedTerm<?> logicOr(TypedTerm<?> a, TypedTerm<?> b) {
        return apply(prim("hydra.core.lib.logic.or"), a, b);
    }
    private static TypedTerm<?> equalityEqual(TypedTerm<?> a, TypedTerm<?> b) {
        return apply(prim("hydra.core.lib.equality.equal"), a, b);
    }
    private static TypedTerm<?> formattingCapitalize(TypedTerm<?> s) {
        return apply(prim("hydra.core.formatting.capitalize"), s);
    }
    private static TypedTerm<?> formattingNonAlnumToUnderscores(TypedTerm<?> s) {
        return apply(prim("hydra.core.formatting.nonAlnumToUnderscores"), s);
    }
    private static TypedTerm<?> formattingConvertCase(TypedTerm<?> from, TypedTerm<?> to, TypedTerm<?> s) {
        return apply(prim("hydra.core.formatting.convertCase"), from, to, s);
    }
    private static TypedTerm<?> packagingModuleNamespace(TypedTerm<?> m) {
        return apply(
            project("hydra.core.packaging.Module", "name"),
            m);
    }
    private static TypedTerm<?> unwrapNamespace(TypedTerm<?> ns) {
        return Packaging.unModuleName(tterm(ns.value));
    }
    private static TypedTerm<?> caseConventionLowerSnake() {
        return inject("hydra.core.util.CaseConvention", "lowerSnake");
    }
    private static TypedTerm<?> caseConventionPascal() {
        return inject("hydra.core.util.CaseConvention", "pascal");
    }
    private static TypedTerm<?> rightTerm(TypedTerm<?> t) {
        return Phantoms.right(t);
    }

    // ---- Right-side helpers ----
    /** {@code replaceChar old new s = Strings.join new (Strings.splitOn old s)} */
    private static TypedTerm<?> replaceChar(TypedTerm<?> oldChar, TypedTerm<?> newChar, TypedTerm<?> s) {
        return stringsIntercalate(newChar, stringsSplitOn(oldChar, s));
    }

    // ---- Definitions ----

    public static final Def buildJavaTestModule = def("buildJavaTestModule")
        .doc("Build the complete Java test module content")
        .lam("testModule").lam("testGroup").lam("testBody")
        .to(() ->
                let(
                    binds(
                        field("ns_", packagingModuleNamespace(var("testModule"))),
                        field("parts", stringsSplitOn(string("."), unwrapNamespace(var("ns_")))),
                        field("packageName", stringsIntercalate(string("."),
                            optionalsFromOptional(list(), listsMaybeInit(var("parts"))))),
                        field("className_", stringsCat2(
                            formattingCapitalize(optionalsFromOptional(string(""), listsMaybeLast(var("parts")))),
                            string("Test"))),
                        field("groupName_", apply(
                            project("hydra.core.testing.TestGroup", "name"),
                            var("testGroup"))),
                        field("standardImports", list(
                            string("import org.junit.jupiter.api.Test;"),
                            string("import static org.junit.jupiter.api.Assertions.*;"),
                            string("import java.util.*;"),
                            string("import hydra.core.overlay.java.util.*;"))),
                        field("header", stringsCat(list(
                            stringsCat2(string("// "), prim("hydra.core.constants.warningAutoGeneratedFile")),
                            string("\n"),
                            stringsCat2(string("// "), var("groupName_")),
                            string("\n\n"),
                            stringsCat(list(string("package "), var("packageName"), string(";\n\n"))),
                            stringsIntercalate(string("\n"), var("standardImports")),
                            string("\n\n"),
                            stringsCat(list(string("public class "), var("className_"), string(" {\n\n"))))))),
                    stringsCat(list(var("header"), var("testBody"), string("\n}\n")))));

    public static final Def findJavaImports = def("findJavaImports")
        .doc("Standard imports for Java JUnit test files")
        .to(() ->
                list(
                    string("import org.junit.jupiter.api.Test;"),
                    string("import static org.junit.jupiter.api.Assertions.*;"),
                    string("import java.util.*;")));

    public static final Def formatJavaTestName = def("formatJavaTestName")
        .doc("Format a test name for Java (PascalCase method name with 'test' prefix)")
        .lam("name")
        .to(() -> {
            TypedTerm<?> name = var("name");
            TypedTerm<?> replaced =
                replaceChar(string("-"), string(" Neg"),
                    replaceChar(string("."), string("Dot"),
                        replaceChar(string("+"), string(" Plus"),
                            replaceChar(string("/"), string(" Div"),
                                replaceChar(string("*"), string(" Mul"),
                                    replaceChar(string("#"), string(" Num"), name))))));
            return
                let(
                    binds(
                        field("replaced", replaced),
                        field("sanitized", formattingNonAlnumToUnderscores(var("replaced"))),
                        field("pascal_", formattingConvertCase(
                            caseConventionLowerSnake(),
                            caseConventionPascal(),
                            var("sanitized")))),
                    stringsCat2(string("test"), var("pascal_")));
        });

    public static final Def generateJavaTestCase = def("generateJavaTestCase")
        .doc("Generate a single JUnit test case from a test case with metadata")
        .lam("groupPath").lam("tcm")
        .to(() -> {
            TypedTerm<?> universalBranch = lambda("ucase",
                let(
                    binds(
                        field("actual_", apply(
                            apply(
                                project("hydra.core.testing.UniversalTestCase", "actual"),
                                var("ucase")),
                            unit())),
                        field("expected_", apply(
                            apply(
                                project("hydra.core.testing.UniversalTestCase", "expected"),
                                var("ucase")),
                            unit())),
                        field("fullName", logicIfElse(
                            listsIsEmpty(var("groupPath")),
                            var("name_"),
                            stringsIntercalate(string("_"),
                                listsConcat2(var("groupPath"), list(var("name_")))))),
                        field("formattedName",
                            apply(prim("hydra.java.testing.formatJavaTestName"), var("fullName")))),
                    right(list(
                        string("    @Test"),
                        stringsCat(list(string("    public void "), var("formattedName"), string("() {"))),
                        string("        assertEquals("),
                        stringsCat(list(string("            "), var("expected_"), string(","))),
                        stringsCat(list(string("            "), var("actual_"), string(");"))),
                        string("    }")))));
            return
                let(
                    binds(
                        field("name_", apply(
                            project("hydra.core.testing.TestCaseWithMetadata", "name"),
                            var("tcm"))),
                        field("tcase", apply(
                            project("hydra.core.testing.TestCaseWithMetadata", "case"),
                            var("tcm")))),
                    match("hydra.core.testing.TestCase", var("tcase"), field("universal", universalBranch)));
        });

    public static final Def generateJavaTestFile = def("generateJavaTestFile")
        .doc("Generate a Java test file for a test group")
        .lam("testModule").lam("testGroup").lam("_g")
        .to(() ->
                apply(
                    prim("hydra.java.testing.generateTestFileWithJavaCodec"),
                    var("testModule"),
                    var("testGroup")));

    public static final Def generateJavaTestGroupHierarchy = def("generateJavaTestGroupHierarchy")
        .doc("Generate test hierarchy for Java with nested subgroups")
        .lam("groupPath").lam("testGroup")
        .to(() -> {
            // Inner lambda that walks one subgroup
            TypedTerm<?> subgroupBlock = lambda("subgroup",
                let(
                    binds(
                        field("groupName", apply(
                            project("hydra.core.testing.TestGroup", "name"),
                            var("subgroup"))),
                        field("header", stringsCat2(string("    // "), var("groupName")))),
                    eithersMap(
                        lambda("content",
                            stringsCat(list(var("header"), string("\n\n"), var("content")))),
                        apply(
                            prim("hydra.java.testing.generateJavaTestGroupHierarchy"),
                            listsConcat2(var("groupPath"), list(var("groupName"))),
                            var("subgroup")))));
            return
                let(
                    binds(
                        field("cases_", apply(
                            project("hydra.core.testing.TestGroup", "cases"),
                            var("testGroup"))),
                        field("subgroups", apply(
                            project("hydra.core.testing.TestGroup", "subgroups"),
                            var("testGroup")))),
                    eithersBind(
                        eithersMap(
                            lambda("lines_",
                                stringsIntercalate(string("\n\n"), listsConcat(var("lines_")))),
                            eithersMapList(
                                lambda("tc",
                                    apply(
                                        prim("hydra.java.testing.generateJavaTestCase"),
                                        var("groupPath"),
                                        var("tc"))),
                                var("cases_"))),
                        lambda("testCasesStr",
                            eithersMap(
                                lambda("subgroupsStr",
                                    stringsCat(list(
                                        var("testCasesStr"),
                                        logicIfElse(
                                            logicOr(
                                                equalityEqual(var("testCasesStr"), string("")),
                                                equalityEqual(var("subgroupsStr"), string(""))),
                                            string(""),
                                            string("\n\n")),
                                        var("subgroupsStr")))),
                                eithersMap(
                                    lambda("blocks",
                                        stringsIntercalate(string("\n\n"), var("blocks"))),
                                    eithersMapList(subgroupBlock, var("subgroups")))))));
        });

    public static final Def generateTestFileWithJavaCodec = def("generateTestFileWithJavaCodec")
        .doc("Generate a complete test file for Java")
        .lam("testModule").lam("testGroup")
        .to(() ->
                eithersMap(
                    lambda("testBody",
                        let(
                            binds(
                                field("testModuleContent", apply(
                                    prim("hydra.java.testing.buildJavaTestModule"),
                                    var("testModule"),
                                    var("testGroup"),
                                    var("testBody"))),
                                field("ns_", packagingModuleNamespace(var("testModule"))),
                                field("parts", stringsSplitOn(string("."), unwrapNamespace(var("ns_")))),
                                field("dirParts", listsDrop(int32(1),
                                    optionalsFromOptional(list(), listsMaybeInit(var("parts"))))),
                                field("className_", stringsCat2(
                                    formattingCapitalize(optionalsFromOptional(string(""), listsMaybeLast(var("parts")))),
                                    string("Test"))),
                                field("fileName", stringsCat2(var("className_"), string(".java"))),
                                field("filePath", stringsCat(list(
                                    stringsIntercalate(string("/"), var("dirParts")),
                                    string("/"),
                                    var("fileName"))))),
                            pair(var("filePath"), var("testModuleContent")))),
                    apply(
                        prim("hydra.java.testing.generateJavaTestGroupHierarchy"),
                        list(),
                        var("testGroup"))));

    public static final Def namespaceToJavaClassName = def("namespaceToJavaClassName")
        .doc("Convert namespace to Java class name")
        .lam("ns_")
        .to(() ->
                stringsIntercalate(string("."),
                    listsMap(
                        prim("hydra.core.formatting.capitalize"),
                        stringsSplitOn(string("."), unwrapNamespace(var("ns_"))))));

    // Order matches the Haskell `definitions = [...]` list.
    private static final Def[] ALL_DEFS = {
            buildJavaTestModule,
            findJavaImports,
            formatJavaTestName,
            generateJavaTestCase,
            generateJavaTestFile,
            generateJavaTestGroupHierarchy,
            generateTestFileWithJavaCodec,
            namespaceToJavaClassName
    };

    static {
        Defs.checkComplete(Testing.class, ALL_DEFS);
    }

    private static final List<Definition> DEFINITIONS = definitionsOf(ALL_DEFS);

    // Haskell: [SerializationSource.ns, TestUtils.ns, Formatting.ns, Names.ns, Constants.ns]
    //         ++ (JavaSyntax.ns : kernelTypesNamespaces)
    private static final List<ModuleDependency> DEPENDENCIES = unqualifiedDeps(
        new ModuleName("hydra.core.serialization"),
        new ModuleName("hydra.core.test.utils"),
        new ModuleName("hydra.core.formatting"),
        new ModuleName("hydra.core.names"),
        new ModuleName("hydra.core.constants"),
        new ModuleName("hydra.java.syntax"),
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
            Optional.given("Java test code generation codec for JUnit-based generation tests"),
            java.util.List.of(),
            java.util.List.of(),
            Optional.none(),
            java.util.List.of())),
        DEPENDENCIES,
        DEFINITIONS);
}
