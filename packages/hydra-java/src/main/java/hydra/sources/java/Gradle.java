package hydra.sources.java;
import hydra.core.Type;
import static hydra.overlay.java.dsl.meta.Defs.unqualifiedDeps;
import hydra.overlay.java.dsl.Types;
import hydra.packaging.Definition;
import hydra.packaging.EntityMetadata;
import hydra.packaging.Module;
import hydra.packaging.ModuleName;
import hydra.packaging.ModuleDependency;
import hydra.overlay.java.util.Optional;

import java.util.Arrays;
import java.util.List;

import static hydra.overlay.java.dsl.Helpers.doc;
import static hydra.overlay.java.dsl.Helpers.typeDef;
import static hydra.overlay.java.dsl.Helpers.typeref;

/**
 * Build configuration for a Gradle-built distribution package: the host-specific information needed
 * to complete a generated package's build, beyond the generated sources themselves.
 *
 * <p>This type is keyed by build SYSTEM (Gradle), not by language. A Gradle build descriptor —
 * Maven-coordinate dependencies, Gradle plugin ids, source directories, a raw Groovy escape hatch —
 * is about Gradle, and nothing in it is specific to any one JVM language. So any language built with
 * Gradle (Java today; potentially others) shares this single configuration type rather than
 * duplicating it per language. Other build systems get their own sibling types (e.g. a hypothetical
 * {@code hydra.stack.StackBuildConfiguration}).</p>
 *
 * <p>The configuration is intended to be supplied alongside a package as overlaid data (the encoded
 * form is the package's {@code build.json}); a head reads it and translates the declared intent into
 * Gradle's idioms. Nothing here is specific to Hydra: it describes the build of an arbitrary
 * translingual project's Gradle package.</p>
 *
 * <p>Provisional home: the module name {@code hydra.gradle} is build-system-keyed, but this source
 * currently lives in the hydra-java package because Java is the only Gradle-built language at
 * present. When a second Gradle-built language appears, it should move to a shared home.</p>
 */
public class Gradle {
    public static final ModuleName NS = new ModuleName("hydra.gradle");
    private static final ModuleName PACKAGING_NS = new ModuleName("hydra.packaging");
    private static final ModuleName FILE_NS = new ModuleName("hydra.file");

    private static Type packaging(String local) { return typeref(PACKAGING_NS, local); }
    private static Type file(String local) { return typeref(FILE_NS, local); }

    private static Definition typeDefHere(String localName, Type t) {
        return typeDef(NS, localName, t);
    }

    private static Definition gradleBuildConfigurationDef() {
        return typeDefHere("GradleBuildConfiguration", doc(
            "The build configuration for a single Gradle-built distribution package, beyond its"
                + " generated sources.",
            Types.record(
                Types.field("dependencies", doc(
                    "Third-party (non-project) dependencies required by the package. Inter-package"
                        + " dependencies within the project are derived separately and are not listed"
                        + " here. Each dependency's scope (api/runtime/test/tool) is significant; its"
                        + " name carries the Maven group and artifact separated by a colon.",
                    Types.list(packaging("PackageDependency")))),
                Types.field("excludes", doc(
                    "Source paths or patterns to exclude from compilation, relative to the package"
                        + " root.",
                    Types.list(Types.string()))),
                Types.field("extraSourceDirs", doc(
                    "Additional source directories to fold into the package's main source set,"
                        + " beyond the generated and overlaid sources (e.g. a directory of"
                        + " build-tool-generated sources).",
                    Types.list(file("FilePath")))),
                Types.field("plugins", doc(
                    "Gradle plugin identifiers to apply to the build, e.g. \"antlr\". Applied in the"
                        + " given order.",
                    Types.list(Types.string()))),
                Types.field("antlr", doc(
                    "ANTLR grammar-generation configuration, present when the package uses the antlr"
                        + " plugin. When given, the host emits the generateGrammarSource configuration"
                        + " and the compileJava-depends-on-generateGrammarSource ordering. Modeled as"
                        + " structured vocabulary rather than a raw Groovy fragment.",
                    Types.optional(antlr("AntlrConfig")))))));
    }

    private static Type antlr(String local) { return typeref(NS, local); }

    private static Definition antlrConfigDef() {
        return typeDefHere("AntlrConfig", doc(
            "Configuration for the Gradle antlr plugin's grammar generation (generateGrammarSource).",
            Types.record(
                Types.field("arguments", doc(
                    "Arguments passed to the ANTLR tool, e.g. [\"-visitor\"] or [\"-visitor\","
                        + " \"-listener\"]. Faithful to ANTLR's command-line interface; covers all"
                        + " ANTLR flags without enumerating them.",
                    Types.list(Types.string()))),
                Types.field("outputDirectory", doc(
                    "Directory into which ANTLR emits generated lexer/parser sources, relative to the"
                        + " package root (e.g. \"build/generated-src/antlr/main\"). The host also folds"
                        + " this into the main source set.",
                    file("FilePath"))))));
    }

    private static final List<Definition> DEFINITIONS = Arrays.asList(
        antlrConfigDef(),
        gradleBuildConfigurationDef()
    );

    private static final List<ModuleDependency> DEPENDENCIES = unqualifiedDeps(PACKAGING_NS, FILE_NS);

    public static final Module module_ = new Module(
        NS,
        Optional.given(new EntityMetadata(
            Optional.given("Build configuration for Gradle-built distribution packages."),
            java.util.List.of(),
            java.util.List.of(),
            Optional.none())),
        DEPENDENCIES,
        DEFINITIONS);
}
