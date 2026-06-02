package hydra.sources.java;
import hydra.core.Field;
import static hydra.dsl.meta.Defs.unqualifiedDeps;
import hydra.core.Name;
import hydra.core.Type;
import hydra.dsl.Core;
import hydra.dsl.Packaging;
import hydra.dsl.Types;
import hydra.dsl.java.Environment;
import hydra.dsl.java.Syntax;
import hydra.dsl.meta.Phantoms;
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
import hydra.packaging.EntityMetadata;
import hydra.packaging.Module;
import hydra.packaging.ModuleName;
import hydra.packaging.ModuleDependency;
import hydra.typed.TypedTerm;
import hydra.util.Maybe;

import java.util.Arrays;
import java.util.List;

import static hydra.dsl.meta.Phantoms.*;
import static hydra.dsl.java.Helpers.termDef;

/**
 * Java test code generation codec for JUnit-based generation tests.
 *
 * <p>Mirror of
 * {@code packages/hydra-java/src/main/haskell/Hydra/Sources/Java/Testing.hs}.</p>
 */
public class Testing {
    public static final ModuleName NS = new ModuleName("hydra.java.testing");

    // ---- Primitive references ----
    private static TypedTerm<?> prim(String fqName) { return var(fqName); }

    private static TypedTerm<?> stringsCat2(TypedTerm<?> a, TypedTerm<?> b) {
        return apply(prim("hydra.lib.strings.cat2"), a, b);
    }
    private static TypedTerm<?> stringsCat(TypedTerm<?> list) {
        return apply(prim("hydra.lib.strings.cat"), list);
    }
    private static TypedTerm<?> stringsIntercalate(TypedTerm<?> sep, TypedTerm<?> list) {
        return apply(prim("hydra.lib.strings.intercalate"), sep, list);
    }
    private static TypedTerm<?> stringsSplitOn(TypedTerm<?> sep, TypedTerm<?> s) {
        return apply(prim("hydra.lib.strings.splitOn"), sep, s);
    }
    private static TypedTerm<?> listsConcat(TypedTerm<?> list) {
        return apply(prim("hydra.lib.lists.concat"), list);
    }
    private static TypedTerm<?> listsConcat2(TypedTerm<?> a, TypedTerm<?> b) {
        return apply(prim("hydra.lib.lists.concat2"), a, b);
    }
    private static TypedTerm<?> listsDrop(TypedTerm<?> n, TypedTerm<?> list) {
        return apply(prim("hydra.lib.lists.drop"), n, list);
    }
    private static TypedTerm<?> listsMap(TypedTerm<?> f, TypedTerm<?> list) {
        return apply(prim("hydra.lib.lists.map"), f, list);
    }
    private static TypedTerm<?> listsNull(TypedTerm<?> list) {
        return apply(prim("hydra.lib.lists.null"), list);
    }
    private static TypedTerm<?> listsMaybeLast(TypedTerm<?> list) {
        return apply(prim("hydra.lib.lists.maybeLast"), list);
    }
    private static TypedTerm<?> listsMaybeInit(TypedTerm<?> list) {
        return apply(prim("hydra.lib.lists.maybeInit"), list);
    }
    private static TypedTerm<?> maybesFromMaybe(TypedTerm<?> dflt, TypedTerm<?> m) {
        return apply(prim("hydra.lib.maybes.fromMaybe"), dflt, m);
    }
    private static TypedTerm<?> eithersBind(TypedTerm<?> e, TypedTerm<?> f) {
        return apply(prim("hydra.lib.eithers.bind"), e, f);
    }
    private static TypedTerm<?> eithersMap(TypedTerm<?> f, TypedTerm<?> e) {
        return apply(prim("hydra.lib.eithers.map"), f, e);
    }
    private static TypedTerm<?> eithersMapList(TypedTerm<?> f, TypedTerm<?> list) {
        return apply(prim("hydra.lib.eithers.mapList"), f, list);
    }
    private static TypedTerm<?> logicIfElse(TypedTerm<?> cond, TypedTerm<?> t, TypedTerm<?> f) {
        return apply(prim("hydra.lib.logic.ifElse"), cond, t, f);
    }
    private static TypedTerm<?> logicOr(TypedTerm<?> a, TypedTerm<?> b) {
        return apply(prim("hydra.lib.logic.or"), a, b);
    }
    private static TypedTerm<?> equalityEqual(TypedTerm<?> a, TypedTerm<?> b) {
        return apply(prim("hydra.lib.equality.equal"), a, b);
    }
    private static TypedTerm<?> formattingCapitalize(TypedTerm<?> s) {
        return apply(prim("hydra.formatting.capitalize"), s);
    }
    private static TypedTerm<?> formattingNonAlnumToUnderscores(TypedTerm<?> s) {
        return apply(prim("hydra.formatting.nonAlnumToUnderscores"), s);
    }
    private static TypedTerm<?> formattingConvertCase(TypedTerm<?> from, TypedTerm<?> to, TypedTerm<?> s) {
        return apply(prim("hydra.formatting.convertCase"), from, to, s);
    }
    private static TypedTerm<?> packagingModuleNamespace(TypedTerm<?> m) {
        return apply(
            project("hydra.packaging.Module", "name"),
            m);
    }
    private static TypedTerm<?> unwrapNamespace(TypedTerm<?> ns) {
        return Packaging.unModuleName(tterm(ns.value));
    }
    private static TypedTerm<?> caseConventionLowerSnake() {
        return injectUnit("hydra.util.CaseConvention", "lowerSnake");
    }
    private static TypedTerm<?> caseConventionPascal() {
        return injectUnit("hydra.util.CaseConvention", "pascal");
    }
    private static TypedTerm<?> rightTerm(TypedTerm<?> t) {
        return Phantoms.right(t);
    }

    // ---- Right-side helpers ----
    /** {@code replaceChar old new s = Strings.intercalate new (Strings.splitOn old s)} */
    private static TypedTerm<?> replaceChar(TypedTerm<?> oldChar, TypedTerm<?> newChar, TypedTerm<?> s) {
        return stringsIntercalate(newChar, stringsSplitOn(oldChar, s));
    }

    // ---- Definitions ----

    private static Definition buildJavaTestModule() {
        TypedTerm<?> body = doc(
            "Build the complete Java test module content",
            lambda("testModule", lambda("testGroup", lambda("testBody",
                let(
                    Arrays.<hydra.core.Field>asList(
                        field("ns_", packagingModuleNamespace(var("testModule"))),
                        field("parts", stringsSplitOn(string("."), unwrapNamespace(var("ns_")))),
                        field("packageName", stringsIntercalate(string("."),
                            maybesFromMaybe(list(), listsMaybeInit(var("parts"))))),
                        field("className_", stringsCat2(
                            formattingCapitalize(maybesFromMaybe(string(""), listsMaybeLast(var("parts")))),
                            string("Test"))),
                        field("groupName_", apply(
                            project("hydra.testing.TestGroup", "name"),
                            var("testGroup"))),
                        field("standardImports", list(
                            string("import org.junit.jupiter.api.Test;"),
                            string("import static org.junit.jupiter.api.Assertions.*;"),
                            string("import java.util.*;"),
                            string("import hydra.util.*;"))),
                        field("header", stringsCat(list(
                            stringsCat2(string("// "), prim("hydra.constants.warningAutoGeneratedFile")),
                            string("\n"),
                            stringsCat2(string("// "), var("groupName_")),
                            string("\n\n"),
                            stringsCat(list(string("package "), var("packageName"), string(";\n\n"))),
                            stringsIntercalate(string("\n"), var("standardImports")),
                            string("\n\n"),
                            stringsCat(list(string("public class "), var("className_"), string(" {\n\n"))))))),
                    stringsCat(list(var("header"), var("testBody"), string("\n}\n"))))))));
        return termDef(NS, "buildJavaTestModule", body.value);
    }

    private static Definition findJavaImports() {
        return termDef(NS, "findJavaImports", doc("Standard imports for Java JUnit test files",
                list(
                    string("import org.junit.jupiter.api.Test;"),
                    string("import static org.junit.jupiter.api.Assertions.*;"),
                    string("import java.util.*;"))).value);
    }

    private static Definition formatJavaTestName() {
        TypedTerm<?> name = var("name");
        TypedTerm<?> replaced =
            replaceChar(string("-"), string(" Neg"),
                replaceChar(string("."), string("Dot"),
                    replaceChar(string("+"), string(" Plus"),
                        replaceChar(string("/"), string(" Div"),
                            replaceChar(string("*"), string(" Mul"),
                                replaceChar(string("#"), string(" Num"), name))))));
        TypedTerm<?> body = doc(
            "Format a test name for Java (PascalCase method name with 'test' prefix)",
            lambda("name",
                let(
                    Arrays.<hydra.core.Field>asList(
                        field("replaced", replaced),
                        field("sanitized", formattingNonAlnumToUnderscores(var("replaced"))),
                        field("pascal_", formattingConvertCase(
                            caseConventionLowerSnake(),
                            caseConventionPascal(),
                            var("sanitized")))),
                    stringsCat2(string("test"), var("pascal_")))));
        return termDef(NS, "formatJavaTestName", body.value);
    }

    private static Definition generateJavaTestCase() {
        TypedTerm<?> universalBranch = lambda("ucase",
            let(
                Arrays.<hydra.core.Field>asList(
                    field("actual_", apply(
                        apply(
                            project("hydra.testing.UniversalTestCase", "actual"),
                            var("ucase")),
                        unit())),
                    field("expected_", apply(
                        apply(
                            project("hydra.testing.UniversalTestCase", "expected"),
                            var("ucase")),
                        unit())),
                    field("fullName", logicIfElse(
                        listsNull(var("groupPath")),
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

        TypedTerm<?> body = doc(
            "Generate a single JUnit test case from a test case with metadata",
            lambda("groupPath", lambda("tcm",
                let(
                    Arrays.<hydra.core.Field>asList(
                        field("name_", apply(
                            project("hydra.testing.TestCaseWithMetadata", "name"),
                            var("tcm"))),
                        field("tcase", apply(
                            project("hydra.testing.TestCaseWithMetadata", "case"),
                            var("tcm")))),
                    cases("hydra.testing.TestCase", var("tcase"), field("universal", universalBranch))))));
        return termDef(NS, "generateJavaTestCase", body.value);
    }

    private static Definition generateJavaTestFile() {
        TypedTerm<?> body = doc(
            "Generate a Java test file for a test group",
            lambda("testModule", lambda("testGroup", lambda("_g",
                apply(
                    prim("hydra.java.testing.generateTestFileWithJavaCodec"),
                    var("testModule"),
                    var("testGroup"))))));
        return termDef(NS, "generateJavaTestFile", body.value);
    }

    private static Definition generateJavaTestGroupHierarchy() {
        // Inner lambda that walks one subgroup
        TypedTerm<?> subgroupBlock = lambda("subgroup",
            let(
                Arrays.<hydra.core.Field>asList(
                    field("groupName", apply(
                        project("hydra.testing.TestGroup", "name"),
                        var("subgroup"))),
                    field("header", stringsCat2(string("    // "), var("groupName")))),
                eithersMap(
                    lambda("content",
                        stringsCat(list(var("header"), string("\n\n"), var("content")))),
                    apply(
                        prim("hydra.java.testing.generateJavaTestGroupHierarchy"),
                        listsConcat2(var("groupPath"), list(var("groupName"))),
                        var("subgroup")))));

        TypedTerm<?> body = doc(
            "Generate test hierarchy for Java with nested subgroups",
            lambda("groupPath", lambda("testGroup",
                let(
                    Arrays.<hydra.core.Field>asList(
                        field("cases_", apply(
                            project("hydra.testing.TestGroup", "cases"),
                            var("testGroup"))),
                        field("subgroups", apply(
                            project("hydra.testing.TestGroup", "subgroups"),
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
                                    eithersMapList(subgroupBlock, var("subgroups"))))))))));
        return termDef(NS, "generateJavaTestGroupHierarchy", body.value);
    }

    private static Definition generateTestFileWithJavaCodec() {
        TypedTerm<?> body = doc(
            "Generate a complete test file for Java",
            lambda("testModule", lambda("testGroup",
                eithersMap(
                    lambda("testBody",
                        let(
                            Arrays.<hydra.core.Field>asList(
                                field("testModuleContent", apply(
                                    prim("hydra.java.testing.buildJavaTestModule"),
                                    var("testModule"),
                                    var("testGroup"),
                                    var("testBody"))),
                                field("ns_", packagingModuleNamespace(var("testModule"))),
                                field("parts", stringsSplitOn(string("."), unwrapNamespace(var("ns_")))),
                                field("dirParts", listsDrop(int32(1),
                                    maybesFromMaybe(list(), listsMaybeInit(var("parts"))))),
                                field("className_", stringsCat2(
                                    formattingCapitalize(maybesFromMaybe(string(""), listsMaybeLast(var("parts")))),
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
                        var("testGroup"))))));
        return termDef(NS, "generateTestFileWithJavaCodec", body.value);
    }

    private static Definition namespaceToJavaClassName() {
        TypedTerm<?> body = doc(
            "Convert namespace to Java class name",
            lambda("ns_",
                stringsIntercalate(string("."),
                    listsMap(
                        prim("hydra.formatting.capitalize"),
                        stringsSplitOn(string("."), unwrapNamespace(var("ns_")))))));
        return termDef(NS, "namespaceToJavaClassName", body.value);
    }

    // Order matches the Haskell `definitions = [...]` list.
    private static final List<Definition> DEFINITIONS = Arrays.asList(
        buildJavaTestModule(),
        findJavaImports(),
        formatJavaTestName(),
        generateJavaTestCase(),
        generateJavaTestFile(),
        generateJavaTestGroupHierarchy(),
        generateTestFileWithJavaCodec(),
        namespaceToJavaClassName());

    // Haskell: [SerializationSource.ns, TestUtils.ns, Formatting.ns, Names.ns, Constants.ns]
    //         ++ (JavaSyntax.ns : kernelTypesNamespaces)
    private static final List<ModuleDependency> DEPENDENCIES = unqualifiedDeps(
        new ModuleName("hydra.serialization"),
        new ModuleName("hydra.test.utils"),
        new ModuleName("hydra.formatting"),
        new ModuleName("hydra.names"),
        new ModuleName("hydra.constants"),
        new ModuleName("hydra.java.syntax"),
        new ModuleName("hydra.paths"),
        new ModuleName("hydra.ast"),
        new ModuleName("hydra.classes"),
        new ModuleName("hydra.coders"),
        new ModuleName("hydra.core"),
        new ModuleName("hydra.error.checking"),
        new ModuleName("hydra.error.core"),
        new ModuleName("hydra.error.packaging"),
        new ModuleName("hydra.errors"),
        new ModuleName("hydra.graph"),
        new ModuleName("hydra.json.model"),
        new ModuleName("hydra.packaging"),
        new ModuleName("hydra.parsing"),
        new ModuleName("hydra.query"),
        new ModuleName("hydra.relational"),
        new ModuleName("hydra.tabular"),
        new ModuleName("hydra.testing"),
        new ModuleName("hydra.topology"),
        new ModuleName("hydra.typed"),
        new ModuleName("hydra.typing"),
        new ModuleName("hydra.util"),
        new ModuleName("hydra.validation"),
        new ModuleName("hydra.variants"));

    public static final Module module_ = new Module(
        NS,
        Maybe.just(new EntityMetadata(
            Maybe.just("Java test code generation codec for JUnit-based generation tests"),
            java.util.List.of(),
            java.util.List.of(),
            Maybe.nothing())),
        DEPENDENCIES,
        DEFINITIONS);
}
