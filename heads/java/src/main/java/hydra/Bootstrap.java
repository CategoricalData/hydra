package hydra;

import hydra.packaging.Module;
import hydra.packaging.ModuleName;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;


/**
 * Bootstrapping entry point: loads Hydra modules from JSON and generates
 * code for a target language. Demonstrates that Java can independently
 * regenerate Hydra from a language-independent JSON representation.
 *
 * Usage:
 * <pre>{@code
 *   java hydra.Bootstrap --target <lang> --json-dir <dist/json root> [OPTIONS]
 *
 * Options:
 *   --output <dir>         Output base directory (default: /tmp/hydra-bootstrapping-demo)
 *   --include-coders       Also load coder packages (hydra-java, hydra-python, hydra-scala, hydra-lisp)
 *   --include-tests        Also load and generate kernel test modules
 *   --kernel-only          Only generate kernel modules (exclude coder packages)
 *   --types-only           Only generate type-defining modules
 *   --ext-json-dir <dir>   Legacy flag; ignored under the per-package layout.
 * }</pre>
 */
public class Bootstrap {

    // Coder packages loaded on top of the kernel + Haskell baseline when
    // --include-coders is set.
    private static final List<String> CODER_PACKAGES = Arrays.asList(
            "hydra-java", "hydra-python", "hydra-scala", "hydra-lisp");

    public static void main(String[] args) throws Exception {
        long totalStart = System.currentTimeMillis();

        String target = null;
        String jsonDirArg = null;
        String outBase = "/tmp/hydra-bootstrapping-demo";
        boolean includeCoders = false;
        boolean includeTests = false;
        boolean typesOnly = false;
        boolean kernelOnly = false;

        for (int i = 0; i < args.length; i++) {
            switch (args[i]) {
                case "--target":
                    if (i + 1 < args.length) target = args[++i];
                    break;
                case "--json-dir":
                    if (i + 1 < args.length) jsonDirArg = args[++i];
                    break;
                case "--ext-json-dir":
                    // Legacy flag; ignored under the per-package layout.
                    if (i + 1 < args.length) i++;
                    break;
                case "--output":
                    if (i + 1 < args.length) outBase = args[++i];
                    break;
                case "--include-coders":
                    includeCoders = true;
                    break;
                case "--include-tests":
                    includeTests = true;
                    break;
                case "--types-only":
                    typesOnly = true;
                    break;
                case "--kernel-only":
                    kernelOnly = true;
                    break;
            }
        }

        if (target == null || jsonDirArg == null) {
            System.out.println("Usage: java hydra.Bootstrap --target <lang> --json-dir <dist/json root> [OPTIONS]");
            System.out.println();
            System.out.println("Options:");
            System.out.println("  --output <dir>         Output base directory");
            System.out.println("  --include-coders       Also load coder packages");
            System.out.println("  --include-tests        Also load and generate kernel test modules");
            System.out.println("  --kernel-only          Only generate kernel modules (exclude coder packages)");
            System.out.println("  --types-only           Only generate type-defining modules");
            System.out.println("  --ext-json-dir <dir>   Legacy flag; ignored under the per-package layout");
            System.exit(1);
        }

        // Backward compatibility: accept an old-style --json-dir ending in
        // <pkg>/src/main/json and strip down to the dist/json root.
        String distJsonRoot = legacyJsonDirToRoot(jsonDirArg);
        // #568: repo root (parent of dist/json's parent dist/), used to locate overlay/<lang>/
        // lib directories for the existence-based redirect sub-list. Falls back to LIB_SUBS_FALLBACK
        // in libSubsForTarget() if this guess is wrong (e.g. a relocated/packaged invocation).
        String repoRoot = Paths.get(distJsonRoot).toAbsolutePath().getParent().getParent().toString();

        String targetCap = target.substring(0, 1).toUpperCase() + target.substring(1);
        String outDir = outBase + File.separator + "java-to-" + target;

        System.out.println("==========================================");
        System.out.println("Mapping JSON to " + targetCap + " (via Java host)");
        System.out.println("==========================================");
        System.out.println("  Host language:   Java");
        System.out.println("  Target language: " + targetCap);
        System.out.println("  JSON root:       " + distJsonRoot);
        System.out.println("  Output:          " + outDir);
        System.out.println("  Include coders:  " + includeCoders);
        System.out.println("  Include tests:   " + includeTests);
        if (typesOnly) System.out.println("  Filter:          types only");
        if (kernelOnly) System.out.println("  Filter:          kernel only");
        System.out.println("==========================================");
        System.out.println();

        Map<hydra.core.Name, hydra.core.Type> schemaMap = Generation.bootstrapSchemaMap();

        // Step 1: Load baseline packages (hydra-kernel + hydra-haskell).
        System.out.println("Step 1: Loading baseline main modules from JSON...");
        long stepStart = System.currentTimeMillis();
        List<Module> kernelMods = loadPackageMain(distJsonRoot, "hydra-kernel", schemaMap);
        List<Module> haskellMods = loadPackageMain(distJsonRoot, "hydra-haskell", schemaMap);
        List<Module> baselineMods = new ArrayList<>(kernelMods);
        baselineMods.addAll(haskellMods);
        long stepTime = System.currentTimeMillis() - stepStart;

        int totalBindings = 0;
        for (Module m : baselineMods) totalBindings += m.definitions.size();
        System.out.println("  Loaded " + baselineMods.size() + " baseline modules (" + totalBindings + " bindings).");
        System.out.println("  Time: " + formatTime(stepTime));
        System.out.println();

        // Both hydra-kernel and hydra-haskell are part of the bootstrap baseline:
        // hydra-haskell provides the runtime AST modules that the generated DSL
        // source modules import, so it must pass --kernel-only filtering.
        Set<String> kernelNsSet = new HashSet<>();
        for (Module m : baselineMods) kernelNsSet.add(m.name.value);

        // Step 2: Optionally load coder packages.
        List<Module> coderMods = new ArrayList<>();
        if (includeCoders) {
            System.out.println("Step 2: Loading coder package modules from JSON...");
            stepStart = System.currentTimeMillis();
            for (String pkg : CODER_PACKAGES) {
                coderMods.addAll(loadPackageMain(distJsonRoot, pkg, schemaMap));
            }
            stepTime = System.currentTimeMillis() - stepStart;
            System.out.println("  Loaded " + coderMods.size() + " coder modules.");
            System.out.println("  Time: " + formatTime(stepTime));
            System.out.println();
        } else {
            System.out.println("Step 2: Skipping coder packages");
            System.out.println();
        }

        List<Module> allMainMods = new ArrayList<>(baselineMods);
        allMainMods.addAll(coderMods);

        // Apply filters
        List<Module> modsToGenerate = allMainMods;
        if (kernelOnly) {
            int before = modsToGenerate.size();
            List<Module> filteredGen = new ArrayList<>();
            for (Module m : modsToGenerate) {
                if (kernelNsSet.contains(m.name.value)) filteredGen.add(m);
            }
            modsToGenerate = filteredGen;
            List<Module> filteredAll = new ArrayList<>();
            for (Module m : allMainMods) {
                if (kernelNsSet.contains(m.name.value)) filteredAll.add(m);
            }
            allMainMods = filteredAll;
            System.out.println("Filtering to kernel modules...");
            System.out.println("  Before: " + before + " modules");
            System.out.println("  After:  " + modsToGenerate.size() + " kernel modules");
            System.out.println();
        }
        if (typesOnly) {
            int before = modsToGenerate.size();
            modsToGenerate = Generation.filterTypeModules(modsToGenerate);
            System.out.println("Filtering to type modules...");
            System.out.println("  Before: " + before + " modules");
            System.out.println("  After:  " + modsToGenerate.size() + " type modules");
            System.out.println();
        }

        // Keep the full (unfiltered) module list for test code generation,
        // since test modules may reference ext bindings even in kernel-only mode.
        List<Module> fullMods = new ArrayList<>(baselineMods);
        fullMods.addAll(coderMods);

        // #546/#547: the kernel test suite references hydra.test.build.* -> hydra.build.*
        // (Option A). When emitting tests, hydra-build's main modules must be in the
        // universe so those refs type-check; otherwise cross-host generation fails with
        // "Unknown variable: hydra.test.build.modules.allTests". Mirrors the Haskell
        // bootstrap-from-json fix (load hydra-build under --include-tests).
        if (includeTests) {
            List<Module> buildMainMods = loadPackageMain(distJsonRoot, "hydra-build", schemaMap);
            fullMods.addAll(buildMainMods);
        }

        // Generate main modules
        String outMain = outDir + File.separator + "src/main";
        System.out.println("Mapping " + modsToGenerate.size() + " modules to " + targetCap + "...");
        System.out.println("  Universe: " + allMainMods.size() + " modules");
        System.out.println("  Output: " + outMain);
        System.out.println();

        stepStart = System.currentTimeMillis();

        switch (target) {
            case "haskell":
                GenerationTargets.writeHaskell(outMain + "/haskell", allMainMods, modsToGenerate);
                break;
            case "java":
                GenerationTargets.writeJava(outMain + "/java", allMainMods, modsToGenerate);
                break;
            case "python":
                GenerationTargets.writePython(outMain + "/python", allMainMods, modsToGenerate);
                break;
            case "scala":
                GenerationTargets.writeScala(outMain + "/scala", allMainMods, modsToGenerate);
                break;
            case "typescript":
                GenerationTargets.writeTypeScript(outMain + "/typescript", allMainMods, modsToGenerate);
                break;
            case "clojure":
                GenerationTargets.writeLispDialect(outMain + "/clojure", "clojure", "clj", allMainMods, modsToGenerate);
                break;
            case "scheme":
                GenerationTargets.writeLispDialect(outMain + "/scheme", "scheme", "scm", allMainMods, modsToGenerate);
                break;
            case "common-lisp":
                GenerationTargets.writeLispDialect(outMain + "/common-lisp", "commonLisp", "lisp", allMainMods, modsToGenerate);
                break;
            case "emacs-lisp":
                GenerationTargets.writeLispDialect(outMain + "/emacs-lisp", "emacsLisp", "el", allMainMods, modsToGenerate);
                break;
            default:
                System.out.println("Unknown target: " + target);
                System.exit(1);
        }

        // #473 Step 0 — lib pass + redirect. The hydra.lib.* primitive IMPLEMENTATIONS live at
        // hydra.overlay.<lang>.lib.* (the analog of Haskell's Hydra.Overlay.Haskell.Lib.*), so hydra.lib.* is free
        // for the generated PrimitiveDefinition def-modules. This mirrors the Haskell driver's
        // twoPassLib logic in bootstrap-from-json/Main.hs: when the Java host generates a target
        // that consumes def-modules (everything except haskell, which uses the registry), it must
        //   (1) emit the hydra.lib.* def-modules from their LOWERED form (lib pass), and
        //   (2) redirect generated CONSUMER call-sites hydra.lib.<sub>.<fn> -> hydra.<lang>.lib.<sub>.<fn>
        //       so they resolve to the relocated impls (redirect; a no-op for Java, whose def-modules
        //       are capitalized classes that don't collide with the lowercase impl subpackages).
        // Without this, self-host (java-to-java, java-to-python, ...) emits impls whose name() refers
        // to hydra.lib.Math_ etc. but never emits those def-modules -> "cannot find symbol" /
        // "PrimitiveDefinition object is not callable". See project_473_self_host_lib_pass_gap.
        // The lib pass runs now (alongside the main pass output); the redirect runs LAST (after the
        // test + ext-for-tests passes below also write into outMain/<target> and outTest/<target>),
        // so every generated consumer file gets its hydra.lib.* impl call-sites redirected.
        if (!target.equals("haskell")) {
            runLibPass(target, outMain + File.separator + target, allMainMods, modsToGenerate);
        }

        stepTime = System.currentTimeMillis() - stepStart;

        Map<String, String> extMap = new java.util.HashMap<>();
        extMap.put("haskell", ".hs"); extMap.put("java", ".java"); extMap.put("python", ".py");
        extMap.put("scala", ".scala");
        extMap.put("typescript", ".ts");
        extMap.put("clojure", ".clj"); extMap.put("scheme", ".scm");
        extMap.put("common-lisp", ".lisp"); extMap.put("emacs-lisp", ".el");
        String ext = extMap.getOrDefault(target, "");
        long mainFileCount = 0;
        try {
            mainFileCount = Files.walk(Paths.get(outMain))
                    .filter(p -> p.toString().endsWith(ext) && !p.getFileName().toString().equals("__init__.py"))
                    .count();
        } catch (Exception e) {
            // ignore
        }

        System.out.println("  Generated " + mainFileCount + " files.");
        System.out.println("  Time: " + formatTime(stepTime));
        System.out.println();

        // Optionally load and generate test modules. Test modules live under
        // hydra-kernel/src/test/json/, and the kernel's main manifest lists them.
        long testFileCount = 0;
        if (includeTests) {
            String kernelMainDir = packageMainDir(distJsonRoot, "hydra-kernel");
            String testJsonDir = distJsonRoot + File.separator + "hydra-kernel"
                    + File.separator + "src" + File.separator + "test" + File.separator + "json";
            System.out.println("Loading test modules from JSON...");
            System.out.println("  Source: " + testJsonDir);

            stepStart = System.currentTimeMillis();
            List<ModuleName> testNamespaces = Generation.readManifestField(kernelMainDir, "testModules");
            List<Module> testMods = Generation.loadModulesFromJson(testJsonDir,
                    schemaMap, testNamespaces);
            // #546/#547: also load hydra-build's OWN test modules (hydra.test.build.*),
            // which the kernel test suite imports (Option A). They live in the hydra-build
            // package's test tree, not hydra-kernel's. Without this, the kernel testSuite's
            // references to them are unresolved on cross-host generation.
            String buildMainDir = packageMainDir(distJsonRoot, "hydra-build");
            String buildTestJsonDir = distJsonRoot + File.separator + "hydra-build"
                    + File.separator + "src" + File.separator + "test" + File.separator + "json";
            List<ModuleName> buildTestNs = Generation.readManifestField(buildMainDir, "testModules");
            if (buildTestNs != null && !buildTestNs.isEmpty()) {
                testMods.addAll(Generation.loadModulesFromJson(buildTestJsonDir, schemaMap, buildTestNs));
            }
            stepTime = System.currentTimeMillis() - stepStart;

            int testBindings = 0;
            for (Module m : testMods) testBindings += m.definitions.size();
            System.out.println("  Loaded " + testMods.size() + " test modules (" + testBindings + " bindings).");
            System.out.println("  Time: " + formatTime(stepTime));
            System.out.println();

            List<Module> allUniverse = new ArrayList<>(fullMods);
            allUniverse.addAll(testMods);

            // Filter skip-emit test namespaces (e.g. hydra.test.testEnv): these are
            // type-only stubs in the DSL whose hand-written per-language counterparts
            // are the source of truth. Emitting them would overwrite hand-written code
            // that registers primitives for the test graph.
            // Mirrors testSkipEmitModuleNames in Hydra.Sources.Test.All.
            java.util.Set<String> testSkipEmit = new java.util.HashSet<>();
            testSkipEmit.add("hydra.test.testEnv");
            List<Module> testModsFiltered = new ArrayList<>();
            for (Module m : testMods) {
                if (!testSkipEmit.contains(m.name.value)) {
                    testModsFiltered.add(m);
                }
            }
            testMods = testModsFiltered;

            String outTest = outDir + File.separator + "src/test";
            System.out.println("Mapping test modules to " + targetCap + "...");
            System.out.println("  Universe: " + allUniverse.size() + " modules");
            System.out.println("  Generating: " + testMods.size() + " test modules");
            System.out.println("  Output: " + outTest);
            System.out.println();

            stepStart = System.currentTimeMillis();

            try {
                switch (target) {
                    case "haskell":
                        GenerationTargets.writeHaskell(outTest + "/haskell", allUniverse, testMods);
                        break;
                    case "java":
                        GenerationTargets.writeJava(outTest + "/java", allUniverse, testMods);
                        break;
                    case "python":
                        GenerationTargets.writePython(outTest + "/python", allUniverse, testMods);
                        break;
                    case "scala":
                        GenerationTargets.writeScala(outTest + "/scala", allUniverse, testMods);
                        break;
                    case "typescript":
                        GenerationTargets.writeTypeScript(outTest + "/typescript", allUniverse, testMods);
                        break;
                    case "clojure":
                        GenerationTargets.writeLispDialect(outTest + "/clojure", "clojure", "clj", allUniverse, testMods);
                        break;
                    case "scheme":
                        GenerationTargets.writeLispDialect(outTest + "/scheme", "scheme", "scm", allUniverse, testMods);
                        break;
                    case "common-lisp":
                        GenerationTargets.writeLispDialect(outTest + "/common-lisp", "commonLisp", "lisp", allUniverse, testMods);
                        break;
                    case "emacs-lisp":
                        GenerationTargets.writeLispDialect(outTest + "/emacs-lisp", "emacsLisp", "el", allUniverse, testMods);
                        break;
                }
            } catch (Exception e) {
                System.out.println("  WARNING: Test generation failed: " + e.getMessage());
            }

            stepTime = System.currentTimeMillis() - stepStart;

            try {
                testFileCount = Files.walk(Paths.get(outTest))
                        .filter(p -> p.toString().endsWith(ext) && !p.getFileName().toString().equals("__init__.py"))
                        .count();
            } catch (Exception e) {
                // ignore
            }

            System.out.println("  Generated " + testFileCount + " test files.");
            System.out.println("  Time: " + formatTime(stepTime));
            System.out.println();
        }

        // #473 redirect — run LAST, over every generated dir (main + test), so consumer call-sites
        // written by any pass (main, lib, test, ext-for-tests) have their hydra.lib.* impl references
        // rewritten to hydra.<lang>.lib.*. See the lib-pass note above and project_473_self_host_lib_pass_gap.
        if (!target.equals("haskell")) {
            redirectLibCalls(repoRoot, target, outMain + File.separator + target);
            redirectTestEnv(target, outMain + File.separator + target);
            if (includeTests) {
                redirectLibCalls(repoRoot, target, outDir + File.separator + "src/test" + File.separator + target);
                redirectTestEnv(target, outDir + File.separator + "src/test" + File.separator + target);
            }
        }

        long totalTime = System.currentTimeMillis() - totalStart;

        System.out.println("==========================================");
        System.out.println("Done: " + mainFileCount + " main"
                + (includeTests ? " + " + testFileCount + " test" : "")
                + " files");
        System.out.println("  Output: " + outDir);
        System.out.println("  Total time: " + formatTime(totalTime));
        System.out.println("==========================================");
    }

    /** Return the JSON directory for a package's main modules. */
    private static String packageMainDir(String root, String pkg) {
        return root + File.separator + pkg + File.separator + "src"
                + File.separator + "main" + File.separator + "json";
    }

    /** Read a manifest field, returning empty if the manifest or field is missing. */
    private static List<ModuleName> readManifestFieldOrEmpty(String pkgDir, String fieldName)
            throws Exception {
        File manifestFile = new File(pkgDir + File.separator + "manifest.json");
        if (!manifestFile.exists()) return new ArrayList<>();
        try {
            return Generation.readManifestField(pkgDir, fieldName);
        } catch (RuntimeException e) {
            return new ArrayList<>();
        }
    }

    /** Load a package's mainModules from its manifest. */
    private static List<Module> loadPackageMain(String root, String pkg,
            Map<hydra.core.Name, hydra.core.Type> schemaMap) throws Exception {
        String pkgDir = packageMainDir(root, pkg);
        List<ModuleName> allNs = readManifestFieldOrEmpty(pkgDir, "mainModules");
        if (allNs.isEmpty()) return new ArrayList<>();
        System.out.println("  " + pkg + ": " + allNs.size() + " modules from " + pkgDir);
        return Generation.loadModulesFromJson(pkgDir, schemaMap, allNs);
    }

    /** Map a legacy --json-dir value ending in <pkg>/src/main/json to the dist/json root.
     *  If the path does not match the expected shape, return unchanged. */
    private static String legacyJsonDirToRoot(String path) {
        String sep = File.separator;
        String trimmed = path.endsWith(sep) ? path.substring(0, path.length() - sep.length()) : path;
        String[] parts = trimmed.split(java.util.regex.Pattern.quote(sep));
        int n = parts.length;
        if (n >= 4 && "src".equals(parts[n - 3]) && "main".equals(parts[n - 2]) && "json".equals(parts[n - 1])) {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < n - 4; i++) {
                if (i > 0) sb.append(sep);
                sb.append(parts[i]);
            }
            String result = sb.toString();
            return result.isEmpty() ? "." : result;
        }
        return path;
    }

    private static String formatTime(long millis) {
        if (millis < 1000) {
            return millis + "ms";
        } else if (millis < 60000) {
            return String.format("%.1fs", millis / 1000.0);
        } else {
            long mins = millis / 60000;
            double secs = (millis % 60000) / 1000.0;
            return String.format("%dm %.1fs", mins, secs);
        }
    }

    // The hydra.lib.* sub-namespaces whose primitives get def-modules + impl relocation (#473).
    // #569/#568: this is the FALLBACK used only if the target's overlay lib directory can't be
    // read from disk (e.g. a packaged/relocated dist without the source tree present). The
    // primary signal is libSubsForTarget()'s filesystem existence check below, which is
    // correct-by-construction for any future hydra.lib.<sub> whether or not it has a relocated
    // overlay impl (see hydra.lib.defaults, #549/#565/#568) and needs no by-name maintenance.
    private static final List<String> LIB_SUBS_FALLBACK = Arrays.asList(
            "chars", "effects", "eithers", "equality", "files", "hashing", "lists", "literals",
            "logic", "maps", "math", "optionals", "pairs", "regex", "sets", "strings", "system", "text");

    // Per-target overlay-lib-directory relative path segment, keyed by the flat-namespace
    // convention each dialect's overlay tree uses (dashes/camelCase collapse to lower_snake
    // for the two-word dialects). Mirrors the langSeg values used by redirectLibCalls.
    private static String overlayDirSegment(String target) {
        switch (target) {
            case "common-lisp": return "common_lisp";
            case "emacs-lisp":  return "emacs_lisp";
            default:            return target; // java, python, scala, typescript, clojure, scheme
        }
    }

    /**
     * #568 structural fix: derive the redirectable hydra.lib.<sub> sub-namespaces for TARGET by
     * checking which overlay/<target>/hydra-kernel/.../hydra/overlay/<seg>/lib/ files actually
     * exist, rather than maintaining a hand-written allowlist per target. A sub only needs
     * redirecting if the target host ships a native impl for it; existence on disk IS that
     * signal. Falls back to LIB_SUBS_FALLBACK if the overlay source tree isn't reachable
     * (repoRoot guess wrong, or a packaged/relocated invocation) so behavior degrades to the old
     * static-list approach rather than silently skipping the redirect.
     */
    private static List<String> libSubsForTarget(String repoRoot, String target) {
        String seg = overlayDirSegment(target);
        java.nio.file.Path libDir = Paths.get(repoRoot, "overlay", target, "hydra-kernel", "src", "main",
                target, "hydra", "overlay", seg, "lib");
        if (!Files.isDirectory(libDir)) return LIB_SUBS_FALLBACK;
        List<String> subs = new ArrayList<>();
        File[] entries = libDir.toFile().listFiles();
        if (entries == null) return LIB_SUBS_FALLBACK;
        for (File f : entries) {
            String name = f.isDirectory() ? f.getName() : f.getName().replaceFirst("\\.[^.]+$", "");
            // Exclude the hand-written registry file/dir (Libraries.*, libraries.*) and any
            // other non-sub file (e.g. Python's __init__.py, Java's PrimitiveType.java) —
            // real sub-namespace entries are lowercase and match a hydra.lib.<sub> module name.
            if (name.equalsIgnoreCase("Libraries") || name.equals("__init__") || name.equals("PrimitiveType")) continue;
            subs.add(name);
        }
        return subs.isEmpty() ? LIB_SUBS_FALLBACK : subs;
    }

    private static boolean isLibModule(Module m) {
        return m.name.value.startsWith("hydra.lib.");
    }

    /**
     * #473 lib pass: emit the hydra.lib.* PrimitiveDefinition def-modules from their LOWERED form.
     * The lib modules are filtered from modsToGenerate, lowered via Codegen.lowerPrimitiveDefinitions,
     * and emitted with a lib-only-lowered universe (consumer modules in the universe stay un-lowered so
     * a lib default-implementation referencing another primitive resolves to the primitive, not a
     * lowered binding). Mirrors genForDirLib in bootstrap-from-json/Main.hs.
     */
    private static void runLibPass(String target, String langDir,
                                   List<Module> allMainMods, List<Module> modsToGenerate) {
        List<Module> libMods = new ArrayList<>();
        for (Module m : modsToGenerate) {
            if (isLibModule(m)) libMods.add(Codegen.lowerPrimitiveDefinitions(m));
        }
        if (libMods.isEmpty()) return;

        List<Module> libUniverse = new ArrayList<>();
        for (Module m : allMainMods) {
            libUniverse.add(isLibModule(m) ? Codegen.lowerPrimitiveDefinitions(m) : m);
        }

        System.out.println("Lib pass: emitting " + libMods.size()
                + " hydra.lib.* definition modules to " + target + "...");
        switch (target) {
            case "java":        GenerationTargets.writeJava(langDir, libUniverse, libMods); break;
            case "python":      GenerationTargets.writePython(langDir, libUniverse, libMods); break;
            case "scala":       GenerationTargets.writeScala(langDir, libUniverse, libMods); break;
            case "typescript":  GenerationTargets.writeTypeScript(langDir, libUniverse, libMods); break;
            case "clojure":     GenerationTargets.writeLispDialect(langDir, "clojure", "clj", libUniverse, libMods); break;
            case "scheme":      GenerationTargets.writeLispDialect(langDir, "scheme", "scm", libUniverse, libMods); break;
            case "common-lisp": GenerationTargets.writeLispDialect(langDir, "commonLisp", "lisp", libUniverse, libMods); break;
            case "emacs-lisp":  GenerationTargets.writeLispDialect(langDir, "emacsLisp", "el", libUniverse, libMods); break;
            default: /* haskell handled by caller guard */ break;
        }
    }

    /**
     * #473/#568/#569 redirect: rewrite generated CONSUMER call-sites so they resolve to the
     * relocated native hydra.overlay.<lang>.lib.* impls instead of the hydra.lib.* def-modules
     * (PrimitiveDefinition data, not callable). Dispatches per target's actual reference shape,
     * mirroring redirectFor / redirectSchemeFor / redirectLispFlat in bootstrap-from-json/Main.hs:
     *   - dotted (python/scala/clojure): hydra.lib.<sub> -> hydra.overlay.<lang>.lib.<sub>
     *   - scheme (R7RS sexp libraries):  (hydra lib <sub>) -> (hydra overlay scheme lib <sub>)
     *   - common-lisp/emacs-lisp (flat): hydra_lib_<sub>_ -> hydra_overlay_<lang>_lib_<sub>_,
     *     and the def-module :use token is dropped from consumer defpackage clauses.
     *   - java/typescript/haskell: no-op. Java's def-modules are capitalized classes that don't
     *     collide with lowercase impl subpackages; TypeScript's coder resolves lib references via
     *     a generated local import alias (see importsToText's overlay path remap), never emitting
     *     raw dotted "hydra.lib.<sub>" text at call sites, so there is nothing to redirect here.
     * #568: the sub-list comes from libSubsForTarget()'s overlay-directory existence check, not a
     * hand-maintained allowlist, so a future hydra.lib.<sub> routes correctly with no driver change
     * (hydra.lib.defaults has no overlay counterpart and is therefore never redirected, replacing
     * the old by-name exclusion).
     */
    private static void redirectLibCalls(String repoRoot, String target, String langDir) {
        if (target.equals("java") || target.equals("typescript") || target.equals("haskell")) return;
        java.nio.file.Path root = Paths.get(langDir);
        if (!Files.isDirectory(root)) return;
        List<String> subs = libSubsForTarget(repoRoot, target);
        try {
            List<java.nio.file.Path> files = new ArrayList<>();
            Files.walk(root).filter(Files::isRegularFile).forEach(files::add);
            switch (target) {
                case "python":
                case "scala":
                case "clojure":
                    redirectDottedFiles(files, subs, "overlay." + target);
                    break;
                case "scheme":
                    redirectSchemeFiles(files, subs);
                    break;
                case "common-lisp":
                    redirectLispFlatFiles(files, subs, "common_lisp");
                    break;
                case "emacs-lisp":
                    redirectLispFlatFiles(files, subs, "emacs_lisp");
                    break;
                default:
                    break;
            }
        } catch (java.io.IOException e) {
            throw new RuntimeException("Lib-call redirect failed under " + langDir, e);
        }
    }

    /** Files under hydra/lib/ are the lib-pass def-modules; the overlay Libraries registry
     *  (overlay/<lang>/.../Libraries.<ext>, copied into the generated tree as
     *  hydra/overlay/<lang>/Libraries.<ext>) deliberately imports BOTH the relocated impl and the
     *  def-module (aliased, for `def_X.fn.name`). Neither must be redirected. #569 Defect B fix:
     *  the previous guard checked "sources/libraries." (lowercase, wrong directory), which never
     *  matched the actual generated path "hydra/overlay/<lang>/Libraries.<ext>" (capitalized) —
     *  so the registry's def-module import got wrongly redirected onto the impl, breaking `.name`
     *  member access (e.g. Scala: "value name is not a member of Int => Int", 242 errors). */
    private static boolean isLibDefOrRegistryFile(String pSlash) {
        // The overlay Libraries registry sits directly under hydra/overlay/<lang>/ for scheme/clojure
        // (Libraries.scm/.clj) but one level deeper under hydra/overlay/<lang>/lib/ for the flat Lisp
        // dialects (common_lisp/lib/libraries.lisp, emacs_lisp/lib/libraries.el). Both must be protected
        // from the lib-call redirect: the registry passes canonical hydra_lib_<sub>_<fn> symbols to
        // (prim-name ...) to derive the registered primitive NAME, which must stay canonical — rewriting
        // it to hydra_overlay_<lang>_lib_... mis-registers the primitive and breaks name lookup (the
        // #444 common-lisp/emacs-lisp test-graph TYPE-ERROR). The registry may sit at any depth under
        // hydra/overlay/<lang>/: directly (scheme/clojure Libraries.scm/.clj), under lib/ (flat Lisp
        // dialects), or under sources/ (python overlay/python/sources/libraries.py). Match the registry
        // regardless of intermediate segments so every host's registry is protected — python's
        // sources/libraries.py was previously unguarded, so java→python corrupted its
        // def_<sub>.<fn>.name references onto the impl, breaking pytest collection with
        // "'function' object has no attribute 'name'".
        return pSlash.contains("/hydra/lib/")
            || pSlash.matches(".*/hydra/overlay/[^/]+/(.*/)?[Ll]ibraries\\.[^/]+$");
    }

    /** Dotted-language redirect (python/scala/clojure): hydra.lib.<sub> -> hydra.<langSeg>.lib.<sub>,
     *  protecting quoted primitive-NAME string literals via a sentinel. Mirrors redirectFor /
     *  redirectDotted in bootstrap-from-json/Main.hs and heads/scala/.../Bootstrap.scala. */
    private static void redirectDottedFiles(List<java.nio.file.Path> files, List<String> subs, String langSeg)
            throws java.io.IOException {
        final String sentinel = "@@HYDRA_LIB_NAME@@";
        for (java.nio.file.Path p : files) {
            String pSlash = p.toString().replace(File.separatorChar, '/');
            if (isLibDefOrRegistryFile(pSlash)) continue;
            String s = new String(Files.readAllBytes(p), java.nio.charset.StandardCharsets.UTF_8);
            if (!s.contains("hydra.lib.")) continue;
            String out = s.replace("\"hydra.lib.", "\"" + sentinel);
            for (String sub : subs) {
                out = out.replace("hydra.lib." + sub + ".",  "hydra." + langSeg + ".lib." + sub + ".");
                out = out.replace("hydra.lib." + sub + ";",  "hydra." + langSeg + ".lib." + sub + ";");
                out = out.replace("hydra.lib." + sub + "\n", "hydra." + langSeg + ".lib." + sub + "\n");
                out = out.replace("hydra.lib." + sub + " ",  "hydra." + langSeg + ".lib." + sub + " ");
                out = out.replace("hydra.lib import " + sub, "hydra." + langSeg + ".lib import " + sub);
            }
            out = out.replace(sentinel, "hydra.lib.");
            if (!out.equals(s)) {
                Files.write(p, out.getBytes(java.nio.charset.StandardCharsets.UTF_8));
            }
        }
    }

    /** Scheme (R7RS) redirect: library/import headers use the space-separated form
     *  `(hydra lib <sub>)`, not dotted. Redirect to `(hydra overlay scheme lib <sub>)`. Call sites
     *  use the flattened identifier hydra_lib_<sub>_<fn> (unchanged — resolved via the renamed
     *  import); primitive NAME strings are dotted "hydra.lib..." (untouched by this rewrite).
     *  Mirrors redirectSchemeFor in bootstrap-from-json/Main.hs. */
    private static void redirectSchemeFiles(List<java.nio.file.Path> files, List<String> subs)
            throws java.io.IOException {
        for (java.nio.file.Path p : files) {
            String pSlash = p.toString().replace(File.separatorChar, '/');
            if (isLibDefOrRegistryFile(pSlash)) continue;
            String s = new String(Files.readAllBytes(p), java.nio.charset.StandardCharsets.UTF_8);
            if (!s.contains("(hydra lib ")) continue;
            String out = s;
            for (String sub : subs) {
                out = out.replace("(hydra lib " + sub + ")", "(hydra overlay scheme lib " + sub + ")");
            }
            if (!out.equals(s)) {
                Files.write(p, out.getBytes(java.nio.charset.StandardCharsets.UTF_8));
            }
        }
    }

    /** Common Lisp / Emacs Lisp redirect: both are flat-namespace dialects where native primitive
     *  impls are plain `defvar hydra_overlay_<lang>_lib_<sub>_<fn>` symbols (no per-module
     *  package), and generated consumers emit a defpackage `(:use ... :hydra.lib.<sub> ...)`
     *  clause per referenced lib. Two-part fix, mirroring redirectLispFlat in
     *  bootstrap-from-json/Main.hs: (1) rename consumer CALL sites
     *  hydra_lib_<sub>_ -> hydra_overlay_<langSeg>_lib_<sub>_ so calls hit the relocated impls, and
     *  (2) drop the ":hydra.lib.<sub>" token from consumer defpackage (:use ...) clauses, so
     *  consumers no longer import the real (post-lib-pass) def-module package over the impl.
     *  Primitive NAME strings are dotted "hydra.lib..." and untouched by the underscore rename. */
    private static void redirectLispFlatFiles(List<java.nio.file.Path> files, List<String> subs, String langSeg)
            throws java.io.IOException {
        for (java.nio.file.Path p : files) {
            String pSlash = p.toString().replace(File.separatorChar, '/');
            if (isLibDefOrRegistryFile(pSlash)) continue;
            String s = new String(Files.readAllBytes(p), java.nio.charset.StandardCharsets.UTF_8);
            if (!s.contains("hydra_lib_") && !s.contains(":hydra.lib.")) continue;
            String out = s;
            for (String sub : subs) {
                out = out.replace("hydra_lib_" + sub + "_", "hydra_overlay_" + langSeg + "_lib_" + sub + "_");
            }
            for (String sub : subs) {
                out = out.replace(" :hydra.lib." + sub, "");
            }
            if (!out.equals(s)) {
                Files.write(p, out.getBytes(java.nio.charset.StandardCharsets.UTF_8));
            }
        }
    }

    /**
     * #444/#456-class test-env redirect for the Lisp targets. The hand-written test environment
     * hydra.test.testEnv is skip-emitted from generated output (see testSkipEmit) and supplied by
     * overlay/&lt;lang&gt;/ under the renamed namespace hydra.overlay.&lt;lang&gt;.test.testEnv (#501).
     * Generated test modules still reference it by its canonical name, so redirect the code
     * reference (NOT quoted primitive-name strings, which never contain "test.testEnv") to the
     * overlay namespace for the dialects whose tests resolve testEnv BY MODULE REFERENCE:
     *   - clojure: dotted `hydra.test.testEnv` -&gt; `hydra.overlay.clojure.test.testEnv`
     *   - scheme:  R7RS library form `(hydra test testEnv)` -&gt; `(hydra overlay scheme test testEnv)`,
     *              plus the dotted form for any residual dotted references
     * common-lisp / emacs-lisp load the hand-written test_env explicitly via their run-tests loader
     * (see run-tests.lisp / run-tests.el), so they need no code redirect here — their loader path is
     * fixed separately. Mirrors redirectClojureTestEnv / redirectSchemeTestEnv in
     * heads/haskell/src/exec/bootstrap-from-json/Main.hs. This is the Java-host parity fix for the
     * #444/#456 host-X-can't-produce-Lisp-target bug class.
     */
    private static void redirectTestEnv(String target, String langDir) {
        if (!target.equals("clojure") && !target.equals("scheme")) return;
        java.nio.file.Path root = Paths.get(langDir);
        if (!Files.isDirectory(root)) return;
        try {
            List<java.nio.file.Path> files = new ArrayList<>();
            Files.walk(root).filter(Files::isRegularFile).forEach(files::add);
            for (java.nio.file.Path p : files) {
                String s = new String(Files.readAllBytes(p), java.nio.charset.StandardCharsets.UTF_8);
                String out = s;
                if (target.equals("scheme")) {
                    out = out.replace("(hydra test testEnv)", "(hydra overlay scheme test testEnv)");
                    out = out.replace("hydra.test.testEnv", "hydra.overlay.scheme.test.testEnv");
                } else { // clojure
                    out = out.replace("hydra.test.testEnv", "hydra.overlay.clojure.test.testEnv");
                }
                if (!out.equals(s)) {
                    Files.write(p, out.getBytes(java.nio.charset.StandardCharsets.UTF_8));
                }
            }
        } catch (java.io.IOException e) {
            throw new RuntimeException("Test-env redirect failed under " + langDir, e);
        }
    }
}
