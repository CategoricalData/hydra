package hydra;

import hydra.overlay.java.build.Generation;
import hydra.packaging.Module;
import hydra.packaging.ModuleName;

import java.io.File;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * #459 Layer 1 transform: JSON -&gt; target language, scoped to a single (package, source-set)
 * pair per invocation. Java-host counterpart to
 * {@code heads/haskell/bin/transform-json-to-target.sh}, matching its CLI contract so
 * {@code sync.sh}'s per-language assemblers can call either interchangeably behind a
 * {@code --generator-host} selector.
 *
 * <p>Unlike {@link Bootstrap} (which always generates for the WHOLE loaded universe, kernel
 * + optionally every coder package, for the cross-host bootstrap demo), this driver narrows
 * {@code modsToGenerate} to exactly one package's declared modules — the same scoping
 * {@code hydra.overlay.java.build.Generation}'s {@code groupByPackage} uses elsewhere. The full
 * per-package JSON universe is still loaded so cross-package type references resolve; only the
 * target package's own modules are written to {@code --output}.
 *
 * Usage:
 * <pre>{@code
 *   java hydra.TransformJsonToTarget --target <lang> --package <pkg> [main|test]
 *       [--output <dir>] [--dist-json-root <dir>] [--include-dsls] [--include-tests]
 * }</pre>
 */
public class TransformJsonToTarget {

    // Coder packages loaded on top of the kernel + Haskell baseline whenever the requested
    // package needs cross-package coder types in scope. Mirrors Bootstrap.CODER_PACKAGES.
    private static final List<String> CODER_PACKAGES = java.util.Arrays.asList(
            "hydra-java", "hydra-python", "hydra-scala", "hydra-lisp");

    public static void main(String[] args) throws Exception {
        long totalStart = System.currentTimeMillis();

        String target = null;
        String pkg = null;
        String sourceSet = "main";
        String distJsonRoot = null;
        // outBase mirrors bootstrap-from-json's --output semantics EXACTLY: the flag's value
        // is a BASE directory, not the literal write path. The exec appends pkg/src/main/<target>
        // (Main.hs: outBase FP.</> pkg FP.</> ("src/main/" ++ target)) — assemble-distribution.sh
        // scripts pass $DIST_ROOT here and compute the same OUT_MAIN path themselves for their
        // digest bookkeeping, so both sides must agree on the suffix or freshness checks corrupt.
        String outBase = null;
        boolean includeDsls = false;
        boolean includeTests = false;
        // --prune-stale / --keep-paths-from <f>: accepted for CLI-contract parity with the
        // Haskell exec (so assemble-distribution.sh can forward them unconditionally to either
        // generator host without per-host argument filtering), but NOT YET IMPLEMENTED here —
        // this driver does not prune orphaned output files. Silently accepted, not acted on.
        // TODO(#459): implement pruning to reach full parity; until then a Java-driven assemble
        // can leave stale generated files behind on a module rename/delete.

        List<String> positional = new ArrayList<>();
        for (int i = 0; i < args.length; i++) {
            switch (args[i]) {
                case "--target":
                    if (i + 1 < args.length) target = args[++i];
                    break;
                case "--package":
                    if (i + 1 < args.length) pkg = args[++i];
                    break;
                case "--dist-json-root":
                    if (i + 1 < args.length) distJsonRoot = args[++i];
                    break;
                case "--output":
                    if (i + 1 < args.length) outBase = args[++i];
                    break;
                case "--include-dsls":
                    includeDsls = true;
                    break;
                case "--include-tests":
                    includeTests = true;
                    break;
                case "--prune-stale":
                    break; // accepted, not yet implemented (see note above)
                case "--keep-paths-from":
                    if (i + 1 < args.length) i++; // consume the value; accepted, not yet implemented
                    break;
                default:
                    if (!args[i].startsWith("--")) positional.add(args[i]);
                    break;
            }
        }
        if (!positional.isEmpty()) sourceSet = positional.get(0);
        if ("test".equals(sourceSet)) includeTests = true;

        if (target == null || pkg == null || distJsonRoot == null || outBase == null) {
            System.out.println("Usage: java hydra.TransformJsonToTarget --target <lang> "
                    + "--package <pkg> [main|test] --dist-json-root <dir> --output <dir> "
                    + "[--include-dsls] [--include-tests]");
            System.exit(1);
        }

        Map<hydra.core.Name, hydra.core.Type> schemaMap = Generation.bootstrapSchemaMap();

        // Baseline: hydra-kernel + hydra-haskell, exactly as Bootstrap loads it, so cross-package
        // kernel type references always resolve regardless of which package is requested.
        List<Module> kernelMods = Bootstrap.loadPackageMain(distJsonRoot, "hydra-kernel", schemaMap);
        List<Module> haskellMods = Bootstrap.loadPackageMain(distJsonRoot, "hydra-haskell", schemaMap);
        List<Module> universe = new ArrayList<>(kernelMods);
        universe.addAll(haskellMods);

        // Coder packages: loaded whenever the requested package is itself a coder package, or
        // (like the Haskell driver's --include-coders) needs cross-package coder types in scope.
        // Mirrors transform-json-to-target.sh's LOAD_FLAGS case split.
        boolean needsCoders = CODER_PACKAGES.contains(pkg) || "hydra-jvm".equals(pkg)
                || "hydra-scala".equals(pkg) || "hydra-go".equals(pkg)
                || pkg.startsWith("hydra-pg") || pkg.startsWith("hydra-rdf")
                || pkg.startsWith("hydra-coq") || pkg.startsWith("hydra-typescript")
                || pkg.startsWith("hydra-ext") || pkg.startsWith("hydra-wasm")
                || pkg.startsWith("hydra-bench") || pkg.startsWith("hydra-build");
        if (needsCoders) {
            for (String coderPkg : CODER_PACKAGES) {
                universe.addAll(Bootstrap.loadPackageMain(distJsonRoot, coderPkg, schemaMap));
            }
        }

        // The requested package's own main modules (loaded again if it's a baseline/coder
        // package already in the universe — loadPackageMain is idempotent per call, and the
        // small re-load cost keeps this driver's package-scoping logic uniform for every pkg).
        List<Module> pkgMainMods = Bootstrap.loadPackageMain(distJsonRoot, pkg, schemaMap);
        List<Module> universeMods = new ArrayList<>(universe);
        for (Module m : pkgMainMods) {
            boolean already = false;
            for (Module u : universeMods) {
                if (u.name.value.equals(m.name.value)) { already = true; break; }
            }
            if (!already) universeMods.add(m);
        }

        List<Module> modsToGenerate;
        if (includeTests) {
            String pkgMainDir = Bootstrap.packageMainDir(distJsonRoot, pkg);
            String testJsonDir = distJsonRoot + File.separator + pkg
                    + File.separator + "src" + File.separator + "test" + File.separator + "json";
            List<ModuleName> testNs = Bootstrap.readManifestFieldOrEmpty(pkgMainDir, "testModules");
            modsToGenerate = testNs.isEmpty() ? new ArrayList<>()
                    : Generation.loadModulesFromJson(testJsonDir, schemaMap, testNs);
            universeMods.addAll(modsToGenerate);
        } else if (includeDsls) {
            // mainDslModules lists the SOURCE type-module names (e.g. hydra.jvm.serde), not the
            // derived DSL wrapper module names — the wrapper's own JSON lives at the name
            // hydra.Dsls.dslModuleName(ns) produces (e.g. hydra.dsl.jvm.serde), already generated
            // and stored as its own file. Mirrors bootstrap-from-json/Main.hs's loadPackageDsl:
            // derive names via dslModuleName, then filter to those that actually exist on disk
            // before loading (a source module may have no DSL-eligible bindings).
            String pkgMainDir = Bootstrap.packageMainDir(distJsonRoot, pkg);
            List<ModuleName> dslSrcNs = Bootstrap.readManifestFieldOrEmpty(pkgMainDir, "mainDslModules");
            modsToGenerate = new ArrayList<>(pkgMainMods);
            List<ModuleName> derivedNs = new ArrayList<>();
            for (ModuleName ns : dslSrcNs) derivedNs.add(hydra.Dsls.dslModuleName(ns));
            List<ModuleName> existingNs = new ArrayList<>();
            for (ModuleName ns : derivedNs) {
                java.nio.file.Path p = Paths.get(pkgMainDir, Codegen.moduleNameToPath(ns) + ".json");
                if (java.nio.file.Files.isRegularFile(p)) existingNs.add(ns);
            }
            if (!existingNs.isEmpty()) {
                modsToGenerate.addAll(Generation.loadModulesFromJson(pkgMainDir, schemaMap, existingNs));
            }
        } else {
            modsToGenerate = pkgMainMods;
        }

        System.out.println("Mapping " + modsToGenerate.size() + " modules ("
                + pkg + "/" + sourceSet + ") to " + target + "...");

        // Matches bootstrap-from-json's --output convention exactly: outBase FP.</> pkg FP.</>
        // ("src/main/" ++ target) for the main source-set, src/test/<target> for test. Callers
        // (assemble-distribution.sh scripts) pass the package's dist-root as outBase and derive
        // the identical OUT_MAIN/OUT_TEST path themselves for digest bookkeeping.
        String srcSetDir = includeTests ? "test" : "main";
        String outDir = outBase + File.separator + pkg + File.separator + "src"
                + File.separator + srcSetDir + File.separator + target;

        long stepStart = System.currentTimeMillis();
        switch (target) {
            case "java":
                GenerationTargets.writeJava(outDir, universeMods, modsToGenerate);
                break;
            case "python":
                GenerationTargets.writePython(outDir, universeMods, modsToGenerate);
                break;
            case "scala":
                GenerationTargets.writeScala(outDir, universeMods, modsToGenerate);
                break;
            case "typescript":
                GenerationTargets.writeTypeScript(outDir, universeMods, modsToGenerate);
                break;
            case "haskell":
                GenerationTargets.writeHaskell(outDir, universeMods, modsToGenerate);
                break;
            case "clojure":
                GenerationTargets.writeLispDialect(outDir, "clojure", "clj", universeMods, modsToGenerate);
                break;
            case "scheme":
                GenerationTargets.writeLispDialect(outDir, "scheme", "scm", universeMods, modsToGenerate);
                break;
            case "common-lisp":
                GenerationTargets.writeLispDialect(outDir, "commonLisp", "lisp", universeMods, modsToGenerate);
                break;
            case "emacs-lisp":
                GenerationTargets.writeLispDialect(outDir, "emacsLisp", "el", universeMods, modsToGenerate);
                break;
            default:
                System.out.println("Unknown target: " + target);
                System.exit(1);
        }

        // #473 lib pass + redirect, mirroring Bootstrap's main-pass handling — required for
        // self-host correctness on every non-Haskell target. repoRoot is the parent of
        // distJsonRoot's parent dist/ (same convention Bootstrap derives from --json-dir).
        if (!target.equals("haskell")) {
            String repoRoot = Paths.get(distJsonRoot).toAbsolutePath().getParent().toString();
            Bootstrap.runLibPass(target, outDir, universeMods, modsToGenerate);
            Bootstrap.redirectLibCalls(repoRoot, target, outDir);
            Bootstrap.redirectTestEnv(target, outDir);
        }

        long totalTime = System.currentTimeMillis() - totalStart;
        System.out.println("Done. Time: " + Bootstrap.formatTime(totalTime));
    }
}
