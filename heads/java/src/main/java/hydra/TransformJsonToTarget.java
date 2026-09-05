package hydra;

import hydra.overlay.java.build.Generation;
import hydra.packaging.Definition;
import hydra.packaging.Module;
import hydra.packaging.ModuleName;
import hydra.packaging.TermDefinition;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

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
    // package needs cross-package coder types in scope. Mirrors the Haskell driver's
    // coderPackages list (heads/haskell/src/exec/bootstrap-from-json/Main.hs) — hydra-java
    // and hydra-scala both reference hydra.jvm.serde helpers, so hydra-jvm must be loaded
    // whenever any coder package is (#459: omitting it here threw UntypedTermVariable on
    // hydra.jvm.serde.javaUnicodeEscape when generating hydra-java standalone).
    private static final List<String> CODER_PACKAGES = java.util.Arrays.asList(
            "hydra-jvm", "hydra-java", "hydra-python", "hydra-scala", "hydra-lisp",
            "hydra-typescript", "hydra-go");

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
        // --prune-stale / --keep-paths-from <f> (#459 H1): after writing, delete any file in the
        // output dir that was NOT just written and is NOT protected by the keep-paths manifest —
        // the Java-driver counterpart to the Haskell bootstrap-from-json prune walk (Main.hs
        // pruneDir), so a module rename/delete does not leave an orphaned generated file behind.
        // The keep-set is the paths GenerationTargets actually wrote (returned from write*), so it
        // can never disagree with what was emitted. --keep-paths-from protects hand-written overlay
        // files (mirrors the Haskell exec's --keep-paths-from). Both are forwarded unconditionally
        // by assemble-distribution.sh to either generator host.
        boolean pruneStale = false;
        String keepPathsFrom = null;

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
                    pruneStale = true;
                    break;
                case "--keep-paths-from":
                    if (i + 1 < args.length) keepPathsFrom = args[++i];
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

        // Extra package dependencies for ext packages, mirroring bootstrap-from-json/Main.hs's
        // packageDeps: hydra-pg's own DSL (hydra.dsl.pg.model) references hydra.rdf.utils, and
        // hydra-ext similarly depends on hydra-rdf, so both need hydra-rdf's main modules in the
        // universe — otherwise generation fails with "untyped term variable:
        // hydra.rdf.utils.resourceToNode" (#459: hit generating hydra-pg/main --include-dsls
        // standalone).
        if (("hydra-pg".equals(pkg) || "hydra-ext".equals(pkg)) && !"hydra-rdf".equals(pkg)) {
            universe.addAll(Bootstrap.loadPackageMain(distJsonRoot, "hydra-rdf", schemaMap));
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
            // #546/#547 (mirrored from bootstrap-from-json/Main.hs): hydra-kernel's always-emitted
            // hydra.test.testSuite references hydra.test.build.* test modules (which in turn
            // reference hydra.build.* main modules), so whenever ANY package's tests are generated,
            // both hydra-build's main AND test modules must be in the universe for those refs to
            // type — otherwise generation fails with "Unknown variable:
            // hydra.test.build.modules.allTests" (#459: hit generating hydra-kernel/test standalone,
            // since hydra-build was never in CODER_PACKAGES/needsCoders for a plain --package
            // hydra-kernel request).
            if (!"hydra-build".equals(pkg)) {
                List<Module> buildUniverseMods = new ArrayList<>(Bootstrap.loadPackageMain(
                        distJsonRoot, "hydra-build", schemaMap));
                String buildMainDir = Bootstrap.packageMainDir(distJsonRoot, "hydra-build");
                String buildTestJsonDir = distJsonRoot + File.separator + "hydra-build"
                        + File.separator + "src" + File.separator + "test" + File.separator + "json";
                List<ModuleName> buildTestNs = Bootstrap.readManifestFieldOrEmpty(
                        buildMainDir, "testModules");
                if (!buildTestNs.isEmpty()) {
                    buildUniverseMods.addAll(
                            Generation.loadModulesFromJson(buildTestJsonDir, schemaMap, buildTestNs));
                }
                for (Module m : buildUniverseMods) {
                    boolean already = false;
                    for (Module u : universeMods) {
                        if (u.name.value.equals(m.name.value)) { already = true; break; }
                    }
                    if (!already) universeMods.add(m);
                }
            }
            String pkgMainDir = Bootstrap.packageMainDir(distJsonRoot, pkg);
            String testJsonDir = distJsonRoot + File.separator + pkg
                    + File.separator + "src" + File.separator + "test" + File.separator + "json";
            List<ModuleName> testNs = Bootstrap.readManifestFieldOrEmpty(pkgMainDir, "testModules");
            modsToGenerate = testNs.isEmpty() ? new ArrayList<>()
                    : Generation.loadModulesFromJson(testJsonDir, schemaMap, testNs);
            universeMods.addAll(modsToGenerate);
            // Filter skip-emit test namespaces (e.g. hydra.test.testEnv) from what gets WRITTEN:
            // these are type-only stubs in the DSL whose hand-written per-language counterparts
            // are the source of truth. Emitting them collides with (or overwrites) hand-written
            // code that registers primitives for the test graph. The universe above still needs
            // the stub for type-checking, so this filters modsToGenerate only, after the
            // universe add. Mirrors testSkipEmitModuleNames in Hydra.Sources.Test.All and
            // Bootstrap.java's equivalent filter.
            Set<String> testSkipEmit = new HashSet<>();
            testSkipEmit.add("hydra.test.testEnv");
            List<Module> testModsFiltered = new ArrayList<>();
            for (Module m : modsToGenerate) {
                if (!testSkipEmit.contains(m.name.value)) {
                    testModsFiltered.add(m);
                }
            }
            modsToGenerate = testModsFiltered;
            // #719/#727: strip scale-distinct decimal test CASES (not whole modules) for the
            // 4 lossy-double hosts that cannot pass them yet (Literal.decimal has no scale
            // field on these targets). Mirrors heads/haskell/src/exec/bootstrap-from-json/
            // Main.hs's scaleDistinctTestNames/dropsScaleDistinctTests/stripScaleDistinctCases
            // -- that Haskell-side filter never ran for targets generated via THIS transcriber
            // (java/python/scala/typescript go through TransformJsonToTarget, not
            // bootstrap-from-json), so it was silently a no-op for them. Remove this whole
            // block (and the Haskell-side one) once all lossy-double hosts carry a real
            // (coefficient, scale) decimal (#727).
            if (dropsScaleDistinctTests(target)) {
                List<Module> scaleFiltered = new ArrayList<>();
                for (Module m : modsToGenerate) {
                    List<Definition> defs = new ArrayList<>();
                    for (Definition d : m.definitions) {
                        if (d instanceof Definition.Term) {
                            TermDefinition td = ((Definition.Term) d).value;
                            defs.add(new Definition.Term(td.withBody(stripScaleDistinctCases(td.body))));
                        } else {
                            defs.add(d);
                        }
                    }
                    scaleFiltered.add(m.withDefinitions(defs));
                }
                modsToGenerate = scaleFiltered;
            }
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
        // repoRoot is the parent of distJsonRoot's parent dist/ (same convention Bootstrap derives
        // from --json-dir) — computed here (rather than only after the switch, as before) because
        // writeJava now needs it to derive overlaySubs via Bootstrap's existing libSubsForTarget scan.
        String repoRoot = Paths.get(distJsonRoot).toAbsolutePath().getParent().toString();
        // #459 (H1): accumulate every path written across all passes (main + lib), relative to
        // outDir, so the prune keep-set is exactly what was emitted. Redirect passes (below) edit
        // files in place and do not change the file set, so they contribute nothing to the keep-set.
        List<String> written = new ArrayList<>();
        switch (target) {
            case "java":
                written.addAll(GenerationTargets.writeJava(repoRoot, outDir, universeMods, modsToGenerate));
                break;
            case "python":
                written.addAll(GenerationTargets.writePython(repoRoot, outDir, universeMods, modsToGenerate));
                break;
            case "scala":
                written.addAll(GenerationTargets.writeScala(repoRoot, outDir, universeMods, modsToGenerate));
                break;
            case "typescript":
                written.addAll(GenerationTargets.writeTypeScript(repoRoot, outDir, universeMods, modsToGenerate));
                break;
            case "haskell":
                written.addAll(GenerationTargets.writeHaskell(repoRoot, outDir, universeMods, modsToGenerate));
                break;
            case "clojure":
                written.addAll(GenerationTargets.writeLispDialect(repoRoot, outDir, "clojure", "clj", universeMods, modsToGenerate));
                break;
            case "scheme":
                written.addAll(GenerationTargets.writeLispDialect(repoRoot, outDir, "scheme", "scm", universeMods, modsToGenerate));
                break;
            case "common-lisp":
                written.addAll(GenerationTargets.writeLispDialect(repoRoot, outDir, "commonLisp", "lisp", universeMods, modsToGenerate));
                break;
            case "emacs-lisp":
                written.addAll(GenerationTargets.writeLispDialect(repoRoot, outDir, "emacsLisp", "el", universeMods, modsToGenerate));
                break;
            default:
                System.out.println("Unknown target: " + target);
                System.exit(1);
        }

        // #473 lib pass + redirect, mirroring Bootstrap's main-pass handling — required for
        // self-host correctness on every target, including haskell (#703: the haskell exclusion
        // here left hydra.lib.* primitive-only modules unemitted for the haskell target, since
        // the ordinary main pass's hasTermDefinitions/hasTypeDefinitions filters never match
        // _Definition_primitive — see Bootstrap.runLibPass's comment). redirectLibCalls and
        // redirectTestEnv already self-guard to the targets they apply to, so enabling this
        // block for haskell is a safe no-op for those two calls.
        written.addAll(Bootstrap.runLibPass(repoRoot, target, outDir, universeMods, modsToGenerate));
        Bootstrap.redirectLibCalls(repoRoot, target, outDir);
        Bootstrap.redirectTestEnv(target, outDir);

        // #459 (H1): prune stale outputs — delete any file under outDir not just written and not
        // protected by --keep-paths-from. The Java counterpart to bootstrap-from-json's pruneDir
        // walk: a module rename/delete otherwise leaves an orphaned generated file that the dist/json
        // consistency check ("differs from regenerated") would fail on within a single run.
        if (pruneStale) {
            pruneStaleOutputs(outDir, written, keepPathsFrom);
        }

        long totalTime = System.currentTimeMillis() - totalStart;
        System.out.println("Done. Time: " + Bootstrap.formatTime(totalTime));
    }

    // #459 (H1): delete every regular file under outDir whose path (relative to outDir) is NOT in
    // the just-written keep-set and NOT protected by the --keep-paths-from manifest, then remove any
    // directories left empty. Mirrors bootstrap-from-json/Main.hs pruneDir + pruneEmptyDirs. The
    // keep-set is the set of paths the writers ACTUALLY produced (returned from write*/runLibPass),
    // so it can never disagree with what was emitted for this target.
    private static void pruneStaleOutputs(String outDir, List<String> writtenRel, String keepPathsFrom)
            throws IOException {
        java.nio.file.Path root = Paths.get(outDir);
        if (!Files.isDirectory(root)) {
            return;
        }
        // Keep-set: normalized relative paths that were written this run.
        java.util.Set<String> keep = new java.util.HashSet<>();
        for (String rel : writtenRel) {
            keep.add(rel.replace(File.separatorChar, '/'));
        }
        // Extend with the caller's keep-paths manifest (hand-written overlay files copied in Step 0,
        // etc.). copy-overlay.sh writes TAB-separated "<OUT_DIR>\t<relPath>" lines (see copy-overlay.sh
        // and readKeepPathsFiles in bootstrap-from-json/Main.hs): the relPath is relative to OUT_DIR,
        // NOT necessarily this outDir, so we keep ONLY the relPaths whose OUT_DIR is this output dir.
        // Parsing the whole line as a path (the bug this replaces) never matches and would delete every
        // protected overlay file.
        if (keepPathsFrom != null) {
            java.nio.file.Path manifest = Paths.get(keepPathsFrom);
            if (Files.isRegularFile(manifest)) {
                String thisOutDir = root.toAbsolutePath().normalize().toString();
                for (String line : Files.readAllLines(manifest, StandardCharsets.UTF_8)) {
                    if (line.isEmpty() || line.startsWith("#")) continue;
                    int tab = line.indexOf('\t');
                    if (tab < 0) {
                        // Backward-compatible: a bare relPath with no OUT_DIR prefix is treated as
                        // relative to this outDir.
                        keep.add(line.trim().replace(File.separatorChar, '/'));
                        continue;
                    }
                    String dir = Paths.get(line.substring(0, tab)).toAbsolutePath().normalize().toString();
                    String rel = line.substring(tab + 1).trim();
                    if (dir.equals(thisOutDir) && !rel.isEmpty()) {
                        keep.add(rel.replace(File.separatorChar, '/'));
                    }
                }
            }
        }
        List<java.nio.file.Path> onDisk = new ArrayList<>();
        try (java.util.stream.Stream<java.nio.file.Path> walk = Files.walk(root)) {
            walk.filter(Files::isRegularFile).forEach(onDisk::add);
        }
        int pruned = 0;
        for (java.nio.file.Path p : onDisk) {
            String rel = root.relativize(p).toString().replace(File.separatorChar, '/');
            // Defensive: never prune a digest.json bookkeeping sibling if one lives inside the tree
            // (the current layout keeps it one level up, so this is belt-and-suspenders — matches
            // pruneDir's same guard).
            if (rel.equals("digest.json")) continue;
            if (!keep.contains(rel)) {
                System.out.println("  - " + p);
                Files.delete(p);
                pruned++;
            }
        }
        // Remove directories left empty by the deletions (deepest first).
        if (pruned > 0) {
            List<java.nio.file.Path> dirs = new ArrayList<>();
            try (java.util.stream.Stream<java.nio.file.Path> walk = Files.walk(root)) {
                walk.filter(Files::isDirectory).forEach(dirs::add);
            }
            dirs.sort(java.util.Comparator.comparingInt((java.nio.file.Path d) -> d.getNameCount()).reversed());
            for (java.nio.file.Path d : dirs) {
                if (d.equals(root)) continue;
                try (java.util.stream.Stream<java.nio.file.Path> entries = Files.list(d)) {
                    if (!entries.findAny().isPresent()) {
                        Files.delete(d);
                    }
                }
            }
        }
        System.out.println("Pruning stale outputs (#459 H1)... pruned " + pruned + " file(s).");
    }

    // #719/#727: the hosts whose Literal.decimal has no scale field yet. Mirrors
    // Main.hs's dropsScaleDistinctTests; keep the two lists in sync until #727 lands.
    private static final Set<String> SCALE_DISTINCT_DROP_TARGETS = new HashSet<>(
            Arrays.asList("common-lisp", "emacs-lisp", "scheme"));

    private static boolean dropsScaleDistinctTests(String target) {
        return SCALE_DISTINCT_DROP_TARGETS.contains(target);
    }

    // Test-case names that assert scale-distinctness (1.1 != 1.10) or exact-exponent
    // rendering beyond what a float64-backed decimal can represent. Mirrors Main.hs's
    // scaleDistinctTestNames.
    private static final Set<String> SCALE_DISTINCT_TEST_NAMES = new HashSet<>(Arrays.asList(
            "same value, different scale",
            "same value, scale tiebreak",
            "same value, scale tiebreak (larger scale)",
            "same value, scale tiebreak (transitively)",
            "tiny exponent",
            "huge exponent"));

    private static boolean isScaleDistinctCase(hydra.core.Term t) {
        if (!(t instanceof hydra.core.Term.Record)) return false;
        hydra.core.Record r = ((hydra.core.Term.Record) t).value;
        if (!r.typeName.value.equals("hydra.testing.TestCaseWithMetadata")) return false;
        for (hydra.core.Field f : r.fields) {
            if (f.name.value.equals("name") && f.term instanceof hydra.core.Term.Literal) {
                hydra.core.Literal lit = ((hydra.core.Term.Literal) f.term).value;
                if (lit instanceof hydra.core.Literal.String_) {
                    return SCALE_DISTINCT_TEST_NAMES.contains(((hydra.core.Literal.String_) lit).value);
                }
            }
        }
        return false;
    }

    // Mirrors Main.hs's stripScaleDistinctCases = rewriteTerm (\recurse t -> case recurse t of
    // TermList els -> TermList (filter (not . isScaleDistinctCase) els); t' -> t'). rewriteTerm's
    // callback receives `recurse`, the function that descends into and rebuilds every subterm one
    // level down (via fsub); calling recurse.apply(t) first (bottom-up) then inspecting the result
    // is the exact contract, matching the Haskell `recurse t` application.
    private static hydra.core.Term stripScaleDistinctCases(hydra.core.Term term0) {
        return hydra.Rewriting.rewriteTerm(
                recurse -> t -> {
                    hydra.core.Term recursedT = recurse.apply(t);
                    if (recursedT instanceof hydra.core.Term.List) {
                        List<hydra.core.Term> els = ((hydra.core.Term.List) recursedT).value;
                        List<hydra.core.Term> filtered = new ArrayList<>();
                        for (hydra.core.Term e : els) {
                            if (!isScaleDistinctCase(e)) filtered.add(e);
                        }
                        return new hydra.core.Term.List(filtered);
                    }
                    return recursedT;
                },
                term0);
    }
}
