package hydra.sources.java;

import hydra.core.Field;
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
import hydra.packaging.Module;
import hydra.packaging.Namespace;
import hydra.phantoms.TTerm;
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
    public static final Namespace NS = new Namespace("hydra.java.testing");

    // ---- Primitive references ----
    private static TTerm<?> prim(String fqName) { return var(fqName); }

    private static TTerm<?> stringsCat2(TTerm<?> a, TTerm<?> b) {
        return apply(prim("hydra.lib.strings.cat2"), a, b);
    }
    private static TTerm<?> stringsCat(TTerm<?> list) {
        return apply(prim("hydra.lib.strings.cat"), list);
    }
    private static TTerm<?> stringsIntercalate(TTerm<?> sep, TTerm<?> list) {
        return apply(prim("hydra.lib.strings.intercalate"), sep, list);
    }
    private static TTerm<?> stringsSplitOn(TTerm<?> sep, TTerm<?> s) {
        return apply(prim("hydra.lib.strings.splitOn"), sep, s);
    }
    private static TTerm<?> listsConcat(TTerm<?> list) {
        return apply(prim("hydra.lib.lists.concat"), list);
    }
    private static TTerm<?> listsConcat2(TTerm<?> a, TTerm<?> b) {
        return apply(prim("hydra.lib.lists.concat2"), a, b);
    }
    private static TTerm<?> listsDrop(TTerm<?> n, TTerm<?> list) {
        return apply(prim("hydra.lib.lists.drop"), n, list);
    }
    private static TTerm<?> listsMap(TTerm<?> f, TTerm<?> list) {
        return apply(prim("hydra.lib.lists.map"), f, list);
    }
    private static TTerm<?> listsNull(TTerm<?> list) {
        return apply(prim("hydra.lib.lists.null"), list);
    }
    private static TTerm<?> listsMaybeLast(TTerm<?> list) {
        return apply(prim("hydra.lib.lists.maybeLast"), list);
    }
    private static TTerm<?> listsMaybeInit(TTerm<?> list) {
        return apply(prim("hydra.lib.lists.maybeInit"), list);
    }
    private static TTerm<?> maybesFromMaybe(TTerm<?> dflt, TTerm<?> m) {
        return apply(prim("hydra.lib.maybes.fromMaybe"), dflt, m);
    }
    private static TTerm<?> eithersBind(TTerm<?> e, TTerm<?> f) {
        return apply(prim("hydra.lib.eithers.bind"), e, f);
    }
    private static TTerm<?> eithersMap(TTerm<?> f, TTerm<?> e) {
        return apply(prim("hydra.lib.eithers.map"), f, e);
    }
    private static TTerm<?> eithersMapList(TTerm<?> f, TTerm<?> list) {
        return apply(prim("hydra.lib.eithers.mapList"), f, list);
    }
    private static TTerm<?> logicIfElse(TTerm<?> cond, TTerm<?> t, TTerm<?> f) {
        return apply(prim("hydra.lib.logic.ifElse"), cond, t, f);
    }
    private static TTerm<?> logicOr(TTerm<?> a, TTerm<?> b) {
        return apply(prim("hydra.lib.logic.or"), a, b);
    }
    private static TTerm<?> equalityEqual(TTerm<?> a, TTerm<?> b) {
        return apply(prim("hydra.lib.equality.equal"), a, b);
    }
    private static TTerm<?> formattingCapitalize(TTerm<?> s) {
        return apply(prim("hydra.formatting.capitalize"), s);
    }
    private static TTerm<?> formattingNonAlnumToUnderscores(TTerm<?> s) {
        return apply(prim("hydra.formatting.nonAlnumToUnderscores"), s);
    }
    private static TTerm<?> formattingConvertCase(TTerm<?> from, TTerm<?> to, TTerm<?> s) {
        return apply(prim("hydra.formatting.convertCase"), from, to, s);
    }
    private static TTerm<?> packagingModuleNamespace(TTerm<?> m) {
        return apply(
            project("hydra.packaging.Module", "namespace"),
            m);
    }
    private static TTerm<?> unwrapNamespace(TTerm<?> ns) {
        return Packaging.unNamespace(tterm(ns.value));
    }
    private static TTerm<?> caseConventionLowerSnake() {
        return injectUnit("hydra.util.CaseConvention", "lowerSnake");
    }
    private static TTerm<?> caseConventionPascal() {
        return injectUnit("hydra.util.CaseConvention", "pascal");
    }
    private static TTerm<?> rightTerm(TTerm<?> t) {
        return Phantoms.right(t);
    }

    // ---- Right-side helpers ----
    /** {@code replaceChar old new s = Strings.intercalate new (Strings.splitOn old s)} */
    private static TTerm<?> replaceChar(TTerm<?> oldChar, TTerm<?> newChar, TTerm<?> s) {
        return stringsIntercalate(newChar, stringsSplitOn(oldChar, s));
    }

    // ---- Definitions ----

    private static Definition buildJavaTestModule() {
        TTerm<?> body = doc(
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
        TTerm<?> name = var("name");
        TTerm<?> replaced =
            replaceChar(string("-"), string(" Neg"),
                replaceChar(string("."), string("Dot"),
                    replaceChar(string("+"), string(" Plus"),
                        replaceChar(string("/"), string(" Div"),
                            replaceChar(string("*"), string(" Mul"),
                                replaceChar(string("#"), string(" Num"), name))))));
        TTerm<?> body = doc(
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
        TTerm<?> universalBranch = lambda("ucase",
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

        TTerm<?> body = doc(
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
        TTerm<?> body = doc(
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
        TTerm<?> subgroupBlock = lambda("subgroup",
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

        TTerm<?> body = doc(
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
        TTerm<?> body = doc(
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
        TTerm<?> body = doc(
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
    private static final List<Namespace> DEPENDENCIES = Arrays.asList(
        new Namespace("hydra.serialization"),
        new Namespace("hydra.test.utils"),
        new Namespace("hydra.formatting"),
        new Namespace("hydra.names"),
        new Namespace("hydra.constants"),
        new Namespace("hydra.java.syntax"),
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
        Maybe.just("Java test code generation codec for JUnit-based generation tests"),
        NS,
        DEPENDENCIES,
        DEFINITIONS);
}
