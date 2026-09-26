package hydra.sources.java;
import hydra.core.model.Field;
import hydra.core.model.Name;
import hydra.core.model.Type;
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
import hydra.core.packaging.ModuleDependency;
import hydra.core.packaging.ModuleName;
import hydra.core.typed.TypedTerm;
import hydra.core.overlay.java.util.Optional;

import java.util.Arrays;
import java.util.List;

import static hydra.core.overlay.java.dsl.Phantoms.*;
import hydra.core.overlay.java.dsl.meta.Defs;
import hydra.core.overlay.java.dsl.meta.Defs.Def;
import static hydra.core.overlay.java.dsl.meta.Defs.define;
import static hydra.core.overlay.java.dsl.meta.Defs.unqualifiedDeps;
import static hydra.core.overlay.java.dsl.meta.Defs.ref;
import static hydra.core.overlay.java.dsl.meta.Defs.definitionsOf;
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
        .doc("The hydra.core.model package name")
        .to(() ->
                just(apply(ref(Names.javaPackageName), list(string("hydra"), string("core")))));

    public static final Def hydraOrdinalMethodName = def("hydraOrdinalMethodName")
        .doc("The name of the generated method returning a union variant's declared ordinal position, "
            + "used to order variants consistently with Haskell's declaration-order deriving Ord (#612)")
        .to(() ->
                string("hydraOrdinal"));

    public static final Def hydraUtilPackageName = def("hydraUtilPackageName")
        .doc("The hydra.core.overlay.java.util package name")
        .to(() ->
                just(apply(ref(Names.javaPackageName), list(string("hydra"), string("core"), string("overlay"), string("java"), string("util")))));

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

    public static final Def toStringMethodName = def("toStringMethodName")
        .to(() ->
                string("toString"));

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
            hydraOrdinalMethodName,
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
            toStringMethodName,
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
            Optional.given("Java naming constants and package name utilities"),
            java.util.List.of(),
            java.util.List.of(),
            Optional.none(),
            java.util.List.of())),
        DEPENDENCIES,
        DEFINITIONS);
}
