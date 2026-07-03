#!/usr/bin/env python3
"""Generate a standalone Gradle build (build.gradle + settings.gradle) for one Hydra
Java distribution package under dist/java/<pkg>/.

Each emitted build is self-contained: from `dist/java/<pkg>/`, running
`./gradlew build` resolves dependencies, compiles, runs tests, and produces a
publishable JAR + POM. The POM declares `net.fortytwo.hydra.java:<pkg>:<version>`
with `api`-scope dependencies on every Hydra package listed in
`packages/<pkg>/package.json`'s `dependencies` array, so an external consumer
that adds e.g. `net.fortytwo.hydra.java:hydra-rdf` to their project automatically
pulls `hydra-kernel` transitively.

Inputs:
  packages/<pkg>/package.json (read for name, description, dependencies)
  hydra.json                   (read for currentVersion)

Outputs:
  dist/java/<pkg>/build.gradle
  dist/java/<pkg>/settings.gradle
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
import sys


DEVELOPER_ID = "joshsh"
DEVELOPER_NAME = "Joshua Shinavier"
DEVELOPER_EMAIL = "josh@fortytwo.net"
LICENSE_NAME = "The Apache License, Version 2.0"
LICENSE_URL = "http://www.apache.org/licenses/LICENSE-2.0.txt"
HOMEPAGE = "https://github.com/CategoricalData/hydra"
SCM_CONNECTION = "scm:git://github.com/CategoricalData/hydra.git"
# #519: per-JVM-language Maven group, so the Java and Scala (and future Clojure)
# artifact sets never collide on coordinates. Java publishes the unsuffixed
# artifact ids under net.fortytwo.hydra.java; Scala publishes the same ids (with
# sbt's _3 cross-version suffix) under net.fortytwo.hydra.scala.
GROUP_ID = "net.fortytwo.hydra.java"

# Map a hydra.gradle DependencyScope variant tag to the Gradle dependency
# configuration that realizes it. (#511) The build.json carries scope as a
# single-key object, e.g. {"api": {}} / {"tool": {}}.
SCOPE_TO_CONFIGURATION = {
    "api": "api",
    "runtime": "runtimeOnly",
    "test": "testImplementation",
    "tool": "antlr",  # the antlr plugin's build-tool configuration
}


def load_overlay_build_config(repo_root: str, name: str) -> dict:
    """Load overlay/java/<pkg>/build.json — the encoded hydra.gradle
    GradleBuildConfiguration for this package (#511). Returns {} when absent
    (most packages have no overlay build config).

    INTERIM: this reads the JSON directly. The build.json format is the canonical
    encoding of hydra.gradle.GradleBuildConfiguration (validated by round-trip);
    when the build system is nativized (#416) this hand parse is replaced by the
    generated hydra.decode.gradle decoder. The on-disk format does not change.
    """
    path = os.path.join(repo_root, "overlay", "java", name, "build.json")
    if not os.path.isfile(path):
        return {}
    with open(path) as f:
        return json.load(f)


def _scope_tag(scope_obj) -> str:
    """The single variant key of an encoded DependencyScope, or 'api' if absent."""
    if not scope_obj:
        return "api"
    return next(iter(scope_obj.keys()))


def _version_string(version_obj) -> str:
    """Extract the concrete version from an encoded VersionSpecifier.
    {"exact": "5.0.2"} -> "5.0.2"; {"any": {}} -> "" (unpinned)."""
    if "exact" in version_obj:
        return version_obj["exact"]
    return ""


def render_build_gradle(name: str, description: str, version: str, deps: list[str],
                        overlay: dict | None = None) -> str:
    overlay = overlay or {}
    dep_lines = []
    # Hydra inter-package deps (from package.json), always api-scoped.
    for dep in deps:
        dep_lines.append(f"    api '{GROUP_ID}:{dep}:{version}'")
    # Third-party deps from the overlay build.json, scope-mapped (#511). Each is
    # an encoded hydra.packaging.PackageDependency: {name, version, scope?}.
    for d in overlay.get("dependencies", []):
        coord = d["name"]
        ver = _version_string(d.get("version", {}))
        cfg = SCOPE_TO_CONFIGURATION.get(_scope_tag(d.get("scope")), "api")
        suffix = f":{ver}" if ver else ""
        dep_lines.append(f"    {cfg} '{coord}{suffix}'")
    deps_block = "\n".join(dep_lines) if dep_lines else "    // no Hydra inter-package dependencies"

    # The published Maven artifact is main only; the generated test tree
    # (when it exists) is part of the kernel's released sources but isn't
    # compiled here. Compiling tests requires the hand-written test
    # infrastructure (TestEnv, TestSuiteRunner, JUnit-dependent base classes)
    # which is split across heads/java/src/test/java and packages/hydra-java/
    # in the developer rollup; the rollup is where `gradle test` runs.
    # Standalone per-package builds focus on producing a publishable jar.
    test_deps_block = ""
    test_task_block = """

// The standalone build is publish-focused; test compilation/execution lives
// in the developer rollup at packages/hydra-java/build.gradle, which sees
// heads/java/src/test/java and JUnit. Skip the test tasks here so a fresh
// `gradle build` from this directory produces the jar without requiring
// the rollup's test infrastructure.
tasks.matching { it.name in ['compileTestJava', 'test', 'processTestResources'] }.configureEach {
    enabled = false
}"""

    excludes = overlay.get("excludes", [])
    if excludes:
        exclude_lines = "\n".join(f"    exclude '{p}'" for p in excludes)
        compile_block = f"\n\ncompileJava {{\n{exclude_lines}\n}}"
    else:
        compile_block = ""

    # Extra plugins from the overlay (e.g. "antlr"), applied after the base set.
    overlay_plugins = overlay.get("plugins", [])
    plugins_block = "".join(f"\n    id '{p}'" for p in overlay_plugins)

    # Extra source dirs folded into the main source set (e.g. ANTLR output).
    extra_src_dirs = overlay.get("extraSourceDirs", [])
    if extra_src_dirs:
        src_lines = "\n".join(
            f'            srcDir file("$projectDir/{d}")' for d in extra_src_dirs)
        source_sets_block = (
            "\n\n// Extra source directories folded into the main source set (#511).\n"
            "sourceSets {\n    main {\n        java {\n" + src_lines + "\n        }\n    }\n}")
    else:
        source_sets_block = ""

    # ANTLR grammar generation, modeled structurally (#511) rather than as a raw
    # Groovy fragment. Present when the package uses the antlr plugin.
    antlr = overlay.get("antlr")
    if antlr:
        args = antlr.get("arguments", [])
        out_dir = antlr.get("outputDirectory", "build/generated-src/antlr/main")
        args_groovy = ", ".join(f"'{a}'" for a in args)
        antlr_block = (
            "\n\n// ANTLR grammar generation (from hydra.gradle AntlrConfig, #511).\n"
            "compileJava.dependsOn generateGrammarSource\n"
            # The antlr output dir is on the main source set, so every task that\n
            # reads those sources (sourcesJar via withSourcesJar(), javadoc) must\n
            # also depend on generateGrammarSource. Gradle 8+ treats a missing\n
            # dependency as a fatal validation error ("uses this output ... without\n
            # declaring an explicit or implicit dependency").\n
            "sourcesJar.dependsOn generateGrammarSource\n"
            "javadoc.dependsOn generateGrammarSource\n"
            "generateGrammarSource {\n"
            f"    arguments += [{args_groovy}]\n"
            f'    outputDirectory = file("$projectDir/{out_dir}")\n'
            "}")
    else:
        antlr_block = ""

    return f"""// Generated file. Do not edit.
// Regenerated by bin/lib/generate-java-package-build.py from packages/{name}/package.json
// and hydra.json (currentVersion). Each dist/java/<pkg>/ is a standalone Gradle build:
// from this directory, `gradle build` produces a tested jar and
// `gradle publishAggregationToCentralPortal` uploads to Sonatype Central Portal.

// nmcp publish plugins (Sonatype Central Portal) require Java 17 to load.
// Apply them conditionally so the build still loads under Java 11 (for
// compile/test). When running on Java 17+, publish tasks become available.
buildscript {{
    repositories {{ mavenCentral() }}
    if (JavaVersion.current().isCompatibleWith(JavaVersion.VERSION_17)) {{
        dependencies {{
            classpath 'com.gradleup.nmcp:nmcp:1.4.4'
        }}
    }}
}}

plugins {{
    id 'java-library'
    id 'maven-publish'
    id 'signing'{plugins_block}
}}

if (JavaVersion.current().isCompatibleWith(JavaVersion.VERSION_17)) {{
    apply plugin: 'com.gradleup.nmcp'
    apply plugin: 'com.gradleup.nmcp.aggregation'
}}

group = '{GROUP_ID}'
version = '{version}'

java {{
    sourceCompatibility = JavaVersion.VERSION_11
    targetCompatibility = JavaVersion.VERSION_11
    withJavadocJar()
    withSourcesJar()
}}

tasks.withType(JavaCompile).configureEach {{
    options.encoding = 'UTF-8'
}}

tasks.withType(Javadoc).configureEach {{
    options.encoding = 'UTF-8'
}}

repositories {{
    mavenCentral()
    mavenLocal()
}}

dependencies {{
{deps_block}{test_deps_block}
    // The aggregation plugin needs an explicit list of projects whose
    // publications it should bundle. For a standalone per-package build the
    // only project is the root project itself.
    if (JavaVersion.current().isCompatibleWith(JavaVersion.VERSION_17)) {{
        nmcpAggregation project
    }}
}}{compile_block}{source_sets_block}{antlr_block}{test_task_block}

publishing {{
    publications {{
        mavenJava(MavenPublication) {{
            from components.java
            artifactId = '{name}'
            pom {{
                name = '{name}'
                description = '{description}'
                url = '{HOMEPAGE}'
                licenses {{
                    license {{
                        name = '{LICENSE_NAME}'
                        url = '{LICENSE_URL}'
                    }}
                }}
                developers {{
                    developer {{
                        id = '{DEVELOPER_ID}'
                        name = '{DEVELOPER_NAME}'
                        email = '{DEVELOPER_EMAIL}'
                    }}
                }}
                scm {{
                    url = '{HOMEPAGE}'
                    connection = '{SCM_CONNECTION}'
                    developerConnection = '{SCM_CONNECTION}'
                }}
            }}
        }}
    }}
}}

signing {{
    required {{ gradle.taskGraph.hasTask('publishAggregationToCentralPortal') ||
                gradle.taskGraph.hasTask('publishToCentralPortal') }}
    sign publishing.publications.mavenJava
}}

// sourcesJar may see a file via multiple sourceSet srcDirs. Skip duplicates;
// the source content is identical, the path collision is just a Gradle quirk.
tasks.named('sourcesJar') {{
    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
}}

// Bundle LICENSE + NOTICE into META-INF of every jar (main, sources, javadoc),
// the Maven-standard location, so the published artifacts carry the Apache-2.0
// license text and the project NOTICE — not just the POM license declaration.
// The files are copied into this package dir by generate-java-package-build.py.
tasks.withType(Jar).configureEach {{
    metaInf {{
        from("$projectDir/LICENSE")
        from("$projectDir/NOTICE")
    }}
}}

// Sonatype Central Portal credentials are read from gradle.properties /
// environment. Set sonatypeUsername + sonatypePassword (token-based) in
// ~/.gradle/gradle.properties, or via -Psonatype{{Username,Password}}=...
//
// nmcp's CentralPortalOptions declares username/password as
// Property<String>. We .set() them with the Provider rather than using
// `=`, because Groovy's `=` on a Property field accepts a String but not
// a Provider — the Provider would silently be coerced to its toString().
// The .set() form is explicit and matches nmcp's lazy-evaluation model.
if (JavaVersion.current().isCompatibleWith(JavaVersion.VERSION_17)) {{
    nmcpAggregation {{
        centralPortal {{
            username.set(providers.gradleProperty('sonatypeUsername'))
            password.set(providers.gradleProperty('sonatypePassword'))
            // USER_MANAGED leaves the deployment in the Central Portal UI for
            // manual review-and-publish before promotion. Switch to AUTOMATIC
            // once the workflow is trusted end-to-end.
            publishingType = 'USER_MANAGED'
        }}
    }}
}}
"""


def render_settings_gradle(name: str) -> str:
    return f"""// Generated file. Do not edit.
rootProject.name = '{name}'
"""


def main() -> int:
    p = argparse.ArgumentParser(description=__doc__.splitlines()[0] if __doc__ else None)
    p.add_argument("package", help="Package name (e.g. hydra-kernel)")
    p.add_argument(
        "--repo-root",
        default=os.environ.get("HYDRA_ROOT_DIR"),
        help="Hydra worktree root (default: $HYDRA_ROOT_DIR)",
    )
    p.add_argument(
        "--out-dir",
        help="Override output directory (default: <repo-root>/dist/java/<package>)",
    )
    args = p.parse_args()

    if not args.repo_root:
        print("error: --repo-root or $HYDRA_ROOT_DIR is required", file=sys.stderr)
        return 2

    pkg_json_path = os.path.join(args.repo_root, "packages", args.package, "package.json")
    if not os.path.isfile(pkg_json_path):
        print(f"error: no such package.json: {pkg_json_path}", file=sys.stderr)
        return 1

    with open(pkg_json_path) as f:
        meta = json.load(f)

    pkg_name = meta.get("name") or args.package
    description = meta.get("description") or pkg_name
    deps = list(meta.get("dependencies") or [])

    # hydra.json:currentVersion is the single source of truth (the standalone
    # VERSION file was retired in #347).
    with open(os.path.join(args.repo_root, "hydra.json")) as f:
        version = json.load(f)["currentVersion"]

    out_dir = args.out_dir or os.path.join(args.repo_root, "dist", "java", args.package)
    os.makedirs(out_dir, exist_ok=True)

    build_path = os.path.join(out_dir, "build.gradle")
    settings_path = os.path.join(out_dir, "settings.gradle")

    overlay = load_overlay_build_config(args.repo_root, args.package)

    with open(build_path, "w") as f:
        f.write(render_build_gradle(pkg_name, description, version, deps, overlay))
    with open(settings_path, "w") as f:
        f.write(render_settings_gradle(pkg_name))

    # Copy LICENSE + NOTICE into the package dir so the build.gradle metaInf
    # block can bundle them into every jar. Like the README, these must be
    # package-local (a path escaping the package root is absent from a
    # published/standalone build).
    for fname in ("LICENSE", "NOTICE"):
        shutil.copyfile(os.path.join(args.repo_root, fname), os.path.join(out_dir, fname))

    print(f"  wrote {build_path}")
    print(f"  wrote {settings_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
