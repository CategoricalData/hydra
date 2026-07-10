package hydra.sources.jvm;
import hydra.dsl.lib.Equality;
import hydra.dsl.lib.Lists;
import hydra.dsl.lib.Logic;
import hydra.dsl.lib.Math_;
import hydra.dsl.lib.Optionals;
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

public class Serde {
    public static final ModuleName NS = new ModuleName("hydra.jvm.serde");

    private static Def def(String localName, Supplier<TypedTerm<?>> body) {
        return define(NS, localName, body);
    }

    /** Fluent form: {@code def("name").doc("...").to(() -> body)}. See Defs.DefBuilder. */
    private static hydra.overlay.java.dsl.meta.Defs.DefBuilder def(String localName) {
        return define(NS, localName);
    }

    public static final Def escapeJavaChar = def("escapeJavaChar")
        .doc("Escape a single character for inclusion in a Java string or char literal")
        .to(() -> lambda("c",
                Logic.ifElse(
                    Equality.equal(var("c"), int32(34)),
                    string("\\\""),
                    Logic.ifElse(
                        Equality.equal(var("c"), int32(92)),
                        string("\\\\"),
                        Logic.ifElse(
                            Equality.equal(var("c"), int32(10)),
                            string("\\n"),
                            Logic.ifElse(
                                Equality.equal(var("c"), int32(13)),
                                string("\\r"),
                                Logic.ifElse(
                                    Equality.equal(var("c"), int32(9)),
                                    string("\\t"),
                                    Logic.ifElse(
                                        Equality.equal(var("c"), int32(8)),
                                        string("\\b"),
                                        Logic.ifElse(
                                            Equality.equal(var("c"), int32(12)),
                                            string("\\f"),
                                            Logic.ifElse(
                                                Logic.and(
                                                    Equality.gte(var("c"), int32(32)),
                                                    Equality.lt(var("c"), int32(127))),
                                                Strings.fromList(list(var("c"))),
                                                apply(ref(Serde.javaUnicodeEscape), var("c"))))))))))));

    public static final Def escapeJavaString = def(
        "escapeJavaString",
        () -> lambda("s",
                Strings.cat(
                    Lists.map(
                        lambda("c", apply(ref(Serde.escapeJavaChar), var("c"))),
                        Strings.toList(var("s"))))));

    public static final Def hexDigit = def(
        "hexDigit",
        () -> lambda("n",
                Logic.ifElse(
                    Equality.lt(var("n"), int32(10)),
                    Math_.add(var("n"), int32(48)),
                    Math_.add(Math_.sub(var("n"), int32(10)), int32(65)))));

    public static final Def javaUnicodeEscape = def(
        "javaUnicodeEscape",
        () -> lambda("n",
                Logic.ifElse(
                    Equality.gt(var("n"), int32(65535)),
                    let(
                        field("n'",
                            Math_.sub(var("n"), int32(65536))),
                        field("hi",
                            Math_.add(
                                int32(55296),
                                Optionals.fromOptional(int32(0), Math_.maybeDiv(var("n'"), int32(1024))))),
                        field("lo",
                            Math_.add(
                                int32(56320),
                                Optionals.fromOptional(int32(0), Math_.maybeMod(var("n'"), int32(1024))))),
                        Strings.cat2(
                            Strings.cat2(string("\\u"), apply(ref(Serde.padHex4), var("hi"))),
                            Strings.cat2(string("\\u"), apply(ref(Serde.padHex4), var("lo"))))),
                    Strings.cat2(string("\\u"), apply(ref(Serde.padHex4), var("n"))))));

    public static final Def padHex4 = def(
        "padHex4",
        () -> lambda("n",
                let(
                    field("d3",
                        Optionals.fromOptional(int32(0), Math_.maybeDiv(var("n"), int32(4096)))),
                    field("r3",
                        Optionals.fromOptional(int32(0), Math_.maybeMod(var("n"), int32(4096)))),
                    field("d2",
                        Optionals.fromOptional(int32(0), Math_.maybeDiv(var("r3"), int32(256)))),
                    field("r2",
                        Optionals.fromOptional(int32(0), Math_.maybeMod(var("r3"), int32(256)))),
                    field("d1",
                        Optionals.fromOptional(int32(0), Math_.maybeDiv(var("r2"), int32(16)))),
                    field("d0",
                        Optionals.fromOptional(int32(0), Math_.maybeMod(var("r2"), int32(16)))),
                    Strings.fromList(
                        list(
                            apply(ref(Serde.hexDigit), var("d3")),
                            apply(ref(Serde.hexDigit), var("d2")),
                            apply(ref(Serde.hexDigit), var("d1")),
                            apply(ref(Serde.hexDigit), var("d0")))))));

    private static final Def[] ALL_DEFS = {
            escapeJavaChar,
            escapeJavaString,
            hexDigit,
            javaUnicodeEscape,
            padHex4
    };

    static {
        Defs.checkComplete(Serde.class, ALL_DEFS);
    }

    private static final List<Definition> DEFINITIONS = definitionsOf(ALL_DEFS);

    private static final List<ModuleDependency> DEPENDENCIES = unqualifiedDeps(
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
            Optional.given("Common JVM serialization helpers: Java/Scala string and character escaping"),
            java.util.List.of(),
            java.util.List.of(),
            Optional.none())),
        DEPENDENCIES,
        DEFINITIONS);
}
