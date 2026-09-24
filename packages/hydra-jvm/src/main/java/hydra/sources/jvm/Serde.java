package hydra.sources.jvm;
import hydra.core.dsl.lib.Equality;
import hydra.core.dsl.lib.Lists;
import hydra.core.dsl.lib.Logic;
import hydra.core.dsl.lib.Math_;
import hydra.core.dsl.lib.Optionals;
import hydra.core.dsl.lib.Ordering;
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

public class Serde {
    public static final ModuleName NS = new ModuleName("hydra.jvm.serde");

    private static Def def(String localName, Supplier<TypedTerm<?>> body) {
        return define(NS, localName, body);
    }

    /** Fluent form: {@code def("name").doc("...").to(() -> body)}. See Defs.DefBuilder. */
    private static hydra.core.overlay.java.dsl.meta.Defs.DefBuilder def(String localName) {
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
                                                    Ordering.gte(var("c"), int32(32)),
                                                    Ordering.lt(var("c"), int32(127))),
                                                Strings.fromList(list(var("c"))),
                                                apply(ref(Serde.javaUnicodeEscape), var("c"))))))))))));

    public static final Def escapeJavaString = def(
        "escapeJavaString",
        () -> lambda("s",
                Strings.concat(
                    Lists.map(
                        lambda("c", apply(ref(Serde.escapeJavaChar), var("c"))),
                        Strings.toList(var("s"))))));

    public static final Def hexDigit = def(
        "hexDigit",
        () -> lambda("n",
                Logic.ifElse(
                    Ordering.lt(var("n"), int32(10)),
                    Math_.add(var("n"), int32(48)),
                    Math_.add(Math_.sub(var("n"), int32(10)), int32(65)))));

    public static final Def javaUnicodeEscape = def(
        "javaUnicodeEscape",
        () -> lambda("n",
                Logic.ifElse(
                    Ordering.gt(var("n"), int32(65535)),
                    let(
                        field("n'",
                            Math_.sub(var("n"), int32(65536))),
                        field("hi",
                            Math_.add(
                                int32(55296),
                                Optionals.withDefault(int32(0), Math_.div(var("n'"), int32(1024))))),
                        field("lo",
                            Math_.add(
                                int32(56320),
                                Optionals.withDefault(int32(0), Math_.mod(var("n'"), int32(1024))))),
                        Strings.concat2(
                            Strings.concat2(string("\\u"), apply(ref(Serde.padHex4), var("hi"))),
                            Strings.concat2(string("\\u"), apply(ref(Serde.padHex4), var("lo"))))),
                    Strings.concat2(string("\\u"), apply(ref(Serde.padHex4), var("n"))))));

    public static final Def padHex4 = def(
        "padHex4",
        () -> lambda("n",
                let(
                    field("d3",
                        Optionals.withDefault(int32(0), Math_.div(var("n"), int32(4096)))),
                    field("r3",
                        Optionals.withDefault(int32(0), Math_.mod(var("n"), int32(4096)))),
                    field("d2",
                        Optionals.withDefault(int32(0), Math_.div(var("r3"), int32(256)))),
                    field("r2",
                        Optionals.withDefault(int32(0), Math_.mod(var("r3"), int32(256)))),
                    field("d1",
                        Optionals.withDefault(int32(0), Math_.div(var("r2"), int32(16)))),
                    field("d0",
                        Optionals.withDefault(int32(0), Math_.mod(var("r2"), int32(16)))),
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
            Optional.given("Common JVM serialization helpers: Java/Scala string and character escaping"),
            java.util.List.of(),
            java.util.List.of(),
            Optional.none(),
            java.util.List.of())),
        DEPENDENCIES,
        DEFINITIONS);
}
