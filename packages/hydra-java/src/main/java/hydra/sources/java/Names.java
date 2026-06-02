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
import hydra.packaging.EntityMetadata;
import hydra.packaging.Module;
import hydra.packaging.ModuleName;
import hydra.packaging.ModuleDependency;
import hydra.typed.TypedTerm;
import hydra.util.Maybe;

import java.util.Arrays;
import java.util.List;

import static hydra.dsl.meta.Phantoms.*;
import hydra.dsl.meta.Defs.Def;
import static hydra.dsl.meta.Defs.define;
import static hydra.dsl.meta.Defs.unqualifiedDeps;
import static hydra.dsl.meta.Defs.ref;
import static hydra.dsl.meta.Defs.definitionsOf;
import java.util.function.Supplier;
import hydra.java.syntax.Identifier;  // AUTO-IMPORT (hydra-java DSL)
import hydra.java.syntax.PackageName;  // AUTO-IMPORT (hydra-java DSL)

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

    // ---- AUTO-PORTED defs (untyped; inference assigns schemes; see #344) ----

    public static final Def acceptMethodName = def(
        "acceptMethodName",
        () -> string("accept"));

    public static final Def applyMethodName = def(
        "applyMethodName",
        () -> string("apply"));

    public static final Def compareToMethodName = def(
        "compareToMethodName",
        () -> string("compareTo"));

    public static final Def equalsMethodName = def(
        "equalsMethodName",
        () -> string("equals"));

    public static final Def getMethodName = def(
        "getMethodName",
        () -> string("get"));

    public static final Def hashCodeMethodName = def(
        "hashCodeMethodName",
        () -> string("hashCode"));

    public static final Def hydraCorePackageName = def(
        "hydraCorePackageName",
        () -> doc("The hydra.core package name",
                just(apply(ref(Names.javaPackageName), list(string("hydra"), string("core"))))));

    public static final Def hydraUtilPackageName = def(
        "hydraUtilPackageName",
        () -> doc("The hydra.util package name",
                just(apply(ref(Names.javaPackageName), list(string("hydra"), string("util"))))));

    public static final Def instanceName = def(
        "instanceName",
        () -> string("instance"));

    public static final Def javaLangPackageName = def(
        "javaLangPackageName",
        () -> doc("The java.lang package name",
                just(apply(ref(Names.javaPackageName), list(string("java"), string("lang"))))));

    public static final Def javaPackageName = def(
        "javaPackageName",
        () -> doc("Construct a Java package name from a list of string parts",
                lambda("parts",
                    wrap(PackageName.TYPE_,
                        Lists.map(
                            lambda("p", wrap(Identifier.TYPE_, var("p"))),
                            var("parts"))))));

    public static final Def javaUtilFunctionPackageName = def(
        "javaUtilFunctionPackageName",
        () -> doc("The java.util.function package name",
                just(
                    apply(
                        ref(Names.javaPackageName),
                        list(string("java"), string("util"), string("function"))))));

    public static final Def javaUtilPackageName = def(
        "javaUtilPackageName",
        () -> doc("The java.util package name",
                just(apply(ref(Names.javaPackageName), list(string("java"), string("util"))))));

    public static final Def otherInstanceName = def(
        "otherInstanceName",
        () -> string("other"));

    public static final Def otherwiseMethodName = def(
        "otherwiseMethodName",
        () -> string("otherwise"));

    public static final Def partialVisitorName = def(
        "partialVisitorName",
        () -> string("PartialVisitor"));

    public static final Def setMethodName = def(
        "setMethodName",
        () -> string("set"));

    public static final Def valueFieldName = def(
        "valueFieldName",
        () -> string("value"));

    public static final Def visitMethodName = def(
        "visitMethodName",
        () -> string("visit"));

    public static final Def visitorName = def(
        "visitorName",
        () -> string("Visitor"));

    public static final Def visitorReturnParameter = def(
        "visitorReturnParameter",
        () -> string("R"));









    private static final List<Definition> DEFINITIONS = definitionsOf(
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
            visitorReturnParameter);

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
        Maybe.just(new EntityMetadata(
            Maybe.just("Java naming constants and package name utilities"),
            java.util.List.of(),
            java.util.List.of(),
            Maybe.nothing())),
        DEPENDENCIES,
        DEFINITIONS);
}
