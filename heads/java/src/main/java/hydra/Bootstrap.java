package hydra;

import hydra.packaging.Module;
import hydra.packaging.Namespace;

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

        Set<String> kernelNsSet = new HashSet<>();
        for (Module m : kernelMods) kernelNsSet.add(m.namespace.value);

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
                if (kernelNsSet.contains(m.namespace.value)) filteredGen.add(m);
            }
            modsToGenerate = filteredGen;
            List<Module> filteredAll = new ArrayList<>();
            for (Module m : allMainMods) {
                if (kernelNsSet.contains(m.namespace.value)) filteredAll.add(m);
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

        // Generate main modules
        String outMain = outDir + File.separator + "src/main";
        System.out.println("Mapping " + modsToGenerate.size() + " modules to " + targetCap + "...");
        System.out.println("  Universe: " + allMainMods.size() + " modules");
        System.out.println("  Output: " + outMain);
        System.out.println();

        stepStart = System.currentTimeMillis();

        switch (target) {
            case "haskell":
                Generation.writeHaskell(outMain + "/haskell", allMainMods, modsToGenerate);
                break;
            case "java":
                Generation.writeJava(outMain + "/java", allMainMods, modsToGenerate);
                break;
            case "python":
                Generation.writePython(outMain + "/python", allMainMods, modsToGenerate);
                break;
            case "clojure":
                Generation.writeLispDialect(outMain + "/clojure", "clojure", "clj", allMainMods, modsToGenerate);
                break;
            case "scheme":
                Generation.writeLispDialect(outMain + "/scheme", "scheme", "scm", allMainMods, modsToGenerate);
                break;
            case "common-lisp":
                Generation.writeLispDialect(outMain + "/common-lisp", "commonLisp", "lisp", allMainMods, modsToGenerate);
                break;
            case "emacs-lisp":
                Generation.writeLispDialect(outMain + "/emacs-lisp", "emacsLisp", "el", allMainMods, modsToGenerate);
                break;
            default:
                System.out.println("Unknown target: " + target);
                System.exit(1);
        }

        stepTime = System.currentTimeMillis() - stepStart;

        Map<String, String> extMap = new java.util.HashMap<>();
        extMap.put("haskell", ".hs"); extMap.put("java", ".java"); extMap.put("python", ".py");
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
            List<Namespace> testNamespaces = Generation.readManifestField(kernelMainDir, "testModules");
            List<Module> testMods = Generation.loadModulesFromJson(testJsonDir,
                    schemaMap, testNamespaces);
            stepTime = System.currentTimeMillis() - stepStart;

            int testBindings = 0;
            for (Module m : testMods) testBindings += m.definitions.size();
            System.out.println("  Loaded " + testMods.size() + " test modules (" + testBindings + " bindings).");
            System.out.println("  Time: " + formatTime(stepTime));
            System.out.println();

            List<Module> allUniverse = new ArrayList<>(fullMods);
            allUniverse.addAll(testMods);

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
                        Generation.writeHaskell(outTest + "/haskell", allUniverse, testMods);
                        break;
                    case "java":
                        Generation.writeJava(outTest + "/java", allUniverse, testMods);
                        break;
                    case "python":
                        Generation.writePython(outTest + "/python", allUniverse, testMods);
                        break;
                    case "clojure":
                        Generation.writeLispDialect(outTest + "/clojure", "clojure", "clj", allUniverse, testMods);
                        break;
                    case "scheme":
                        Generation.writeLispDialect(outTest + "/scheme", "scheme", "scm", allUniverse, testMods);
                        break;
                    case "common-lisp":
                        Generation.writeLispDialect(outTest + "/common-lisp", "commonLisp", "lisp", allUniverse, testMods);
                        break;
                    case "emacs-lisp":
                        Generation.writeLispDialect(outTest + "/emacs-lisp", "emacsLisp", "el", allUniverse, testMods);
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
    private static List<Namespace> readManifestFieldOrEmpty(String pkgDir, String fieldName)
            throws Exception {
        File manifestFile = new File(pkgDir + File.separator + "manifest.json");
        if (!manifestFile.exists()) return new ArrayList<>();
        try {
            return Generation.readManifestField(pkgDir, fieldName);
        } catch (RuntimeException e) {
            return new ArrayList<>();
        }
    }

    /** Load a package's mainModules + evalLibModules from its manifest. */
    private static List<Module> loadPackageMain(String root, String pkg,
            Map<hydra.core.Name, hydra.core.Type> schemaMap) throws Exception {
        String pkgDir = packageMainDir(root, pkg);
        List<Namespace> mainNs = readManifestFieldOrEmpty(pkgDir, "mainModules");
        List<Namespace> evalNs = readManifestFieldOrEmpty(pkgDir, "evalLibModules");
        List<Namespace> allNs = new ArrayList<>(mainNs);
        allNs.addAll(evalNs);
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
}
