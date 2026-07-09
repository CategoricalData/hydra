package hydra.sources.java;
import hydra.core.Field;
import hydra.core.Name;
import hydra.core.Type;
import hydra.dsl.Core;
import hydra.dsl.Packaging;
import hydra.overlay.java.dsl.Types;
import hydra.dsl.java.Environment;
import hydra.dsl.java.Syntax;
import hydra.dsl.lib.Eithers;
import hydra.dsl.lib.Equality;
import hydra.dsl.lib.Lists;
import hydra.dsl.lib.Literals;
import hydra.dsl.lib.Logic;
import hydra.dsl.lib.Maps;
import hydra.dsl.lib.Math_;
import hydra.dsl.lib.Optionals;
import hydra.dsl.lib.Pairs;
import hydra.dsl.lib.Sets;
import hydra.dsl.lib.Strings;
import hydra.packaging.Definition;
import hydra.packaging.EntityMetadata;
import hydra.packaging.Module;
import hydra.packaging.ModuleDependency;
import hydra.packaging.ModuleName;
import hydra.typed.TypedTerm;
import hydra.overlay.java.util.Optional;

import java.util.Arrays;
import java.util.List;

import static hydra.overlay.java.dsl.meta.Phantoms.*;
import hydra.overlay.java.dsl.meta.Defs;
import hydra.overlay.java.dsl.meta.Defs.Def;
import static hydra.overlay.java.dsl.meta.Defs.define;
import static hydra.overlay.java.dsl.meta.Defs.unqualifiedDeps;
import static hydra.overlay.java.dsl.meta.Defs.ref;
import static hydra.overlay.java.dsl.meta.Defs.definitionsOf;
import java.util.function.Supplier;
import hydra.java.syntax.Identifier;
import hydra.java.syntax.PackageName;

/**
 * Java naming constants and package name utilities.
 *
 * <p>Mirror of
 * {@code packages/hydra-java/src/main/haskell/Hydra/Sources/Java/Names.hs}.</p>
 */
public class Names {
    public static final ModuleName NS = new ModuleName("hydra.java.names");

    private static Def def(String localName, Supplier<TypedTerm<?>> body) {
        return define(NS, localName, body);
    }

    /** Fluent form: {@code def("name").doc("...").lam("x").to(() -> body)}. See Defs.DefBuilder. */
    private static Defs.DefBuilder def(String localName) {
        return define(NS, localName);
    }

    // ---- AUTO-PORTED defs (untyped; inference assigns schemes; see #344) ----

    public static final Def acceptMethodName = def("acceptMethodName")
        .to(() ->
                string("accept"));

    public static final Def applyMethodName = def("applyMethodName")
        .to(() ->
                string("apply"));

    public static final Def compareToMethodName = def("compareToMethodName")
        .to(() ->
                string("compareTo"));

    public static final Def equalsMethodName = def("equalsMethodName")
        .to(() ->
                string("equals"));

    public static final Def getMethodName = def("getMethodName")
        .to(() ->
                string("get"));

    public static final Def hashCodeMethodName = def("hashCodeMethodName")
        .to(() ->
                string("hashCode"));

    public static final Def hydraCorePackageName = def("hydraCorePackageName")
        .doc("The hydra.core package name")
        .to(() ->
                just(apply(ref(Names.javaPackageName), list(string("hydra"), string("core")))));

    public static final Def hydraUtilPackageName = def("hydraUtilPackageName")
        .doc("The hydra.overlay.java.util package name")
        .to(() ->
                just(apply(ref(Names.javaPackageName), list(string("hydra"), string("overlay"), string("java"), string("util")))));

    public static final Def instanceName = def("instanceName")
        .to(() ->
                string("instance"));

    public static final Def javaLangPackageName = def("javaLangPackageName")
        .doc("The java.lang package name")
        .to(() ->
                just(apply(ref(Names.javaPackageName), list(string("java"), string("lang")))));

    public static final Def javaPackageName = def("javaPackageName")
        .doc("Construct a Java package name from a list of string parts")
        .lam("parts")
        .to(() ->
                wrap(PackageName.TYPE_,
                    Lists.map(
                        lambda("p", wrap(Identifier.TYPE_, var("p"))),
                        var("parts"))));

    public static final Def javaUtilFunctionPackageName = def("javaUtilFunctionPackageName")
        .doc("The java.util.function package name")
        .to(() ->
                just(
                    apply(
                        ref(Names.javaPackageName),
                        list(string("java"), string("util"), string("function")))));

    public static final Def javaUtilPackageName = def("javaUtilPackageName")
        .doc("The java.util package name")
        .to(() ->
                just(apply(ref(Names.javaPackageName), list(string("java"), string("util")))));

    public static final Def otherInstanceName = def("otherInstanceName")
        .to(() ->
                string("other"));

    public static final Def otherwiseMethodName = def("otherwiseMethodName")
        .to(() ->
                string("otherwise"));

    public static final Def partialVisitorName = def("partialVisitorName")
        .to(() ->
                string("PartialVisitor"));

    public static final Def setMethodName = def("setMethodName")
        .to(() ->
                string("set"));

    public static final Def valueFieldName = def("valueFieldName")
        .to(() ->
                string("value"));

    public static final Def visitMethodName = def("visitMethodName")
        .to(() ->
                string("visit"));

    public static final Def visitorName = def("visitorName")
        .to(() ->
                string("Visitor"));

    public static final Def visitorReturnParameter = def("visitorReturnParameter")
        .to(() ->
                string("R"));

    private static final Def[] ALL_DEFS = {
            acceptMethodName,
            applyMethodName,
            compareToMethodName,
            equalsMethodName,
            getMethodName,
            hashCodeMethodName,
            hydraCorePackageName,
            hydraUtilPackageName,
            instanceName,
            javaLangPackageName,
            javaPackageName,
            javaUtilFunctionPackageName,
            javaUtilPackageName,
            otherInstanceName,
            otherwiseMethodName,
            partialVisitorName,
            setMethodName,
            valueFieldName,
            visitMethodName,
            visitorName,
            visitorReturnParameter};

    static {
        Defs.checkComplete(Names.class, ALL_DEFS);
    }

    private static final List<Definition> DEFINITIONS = definitionsOf(ALL_DEFS);

    private static final List<ModuleDependency> DEPENDENCIES = unqualifiedDeps(
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
        Optional.given(new EntityMetadata(
            Optional.given("Java naming constants and package name utilities"),
            java.util.List.of(),
            java.util.List.of(),
            Optional.none())),
        DEPENDENCIES,
        DEFINITIONS);
}
