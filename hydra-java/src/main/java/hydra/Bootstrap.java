package hydra;

import hydra.module.Module;
import hydra.module.Namespace;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


/**
 * Bootstrapping entry point: loads Hydra modules from JSON and generates
 * code for a target language. Demonstrates that Java can independently
 * regenerate Hydra from a language-independent JSON representation.
 */
public class Bootstrap {

    public static void main(String[] args) throws Exception {
        long totalStart = System.currentTimeMillis();

        String target = null;
        String jsonDir = null;
        String outBase = "/tmp/hydra-bootstrapping-demo";
        boolean typesOnly = false;
        boolean kernelOnly = false;

        for (int i = 0; i < args.length; i++) {
            switch (args[i]) {
                case "--target":
                    if (i + 1 < args.length) target = args[++i];
                    break;
                case "--json-dir":
                    if (i + 1 < args.length) jsonDir = args[++i];
                    break;
                case "--output":
                    if (i + 1 < args.length) outBase = args[++i];
                    break;
                case "--types-only":
                    typesOnly = true;
                    break;
                case "--kernel-only":
                    kernelOnly = true;
                    break;
            }
        }

        if (target == null || jsonDir == null) {
            System.out.println("Usage: java hydra.Bootstrap --target <haskell|java|python> --json-dir <path> [--output <dir>] [--types-only] [--kernel-only]");
            System.exit(1);
        }

        String outDir = outBase + File.separator + "java-to-" + target;

        System.out.println("==========================================");
        System.out.println("Bootstrapping Hydra to " + target + " from JSON (via Java)");
        System.out.println("==========================================");
        System.out.println("  Host language:   Java");
        System.out.println("  Target language: " + target);
        System.out.println("  JSON directory:  " + jsonDir);
        System.out.println("  Output directory:" + outDir);
        if (typesOnly) System.out.println("  Filter:          types only");
        if (kernelOnly) System.out.println("  Filter:          kernel only (excluding hydra.ext.*)");
        System.out.println("==========================================");
        System.out.println();

        // Count JSON input files
        long jsonFileCount = 0;
        try {
            jsonFileCount = Files.walk(Paths.get(jsonDir))
                    .filter(p -> p.toString().endsWith(".json"))
                    .count();
        } catch (Exception e) {
            // ignore
        }
        System.out.println("Step 1: Loading modules from JSON...");
        System.out.println("  Source: " + jsonDir);
        System.out.println("  JSON input files: " + jsonFileCount);

        long stepStart = System.currentTimeMillis();
        List<Module> kernelModules = Collections.emptyList();
        // Preserve TypeSchemes (stripTypeSchemes=false) so that the pipeline
        // can skip pre-adaptation inference. Without types, inference fails on
        // hoisted case-statement bindings (_hoist_*) that lack type annotations.
        List<Module> rawMods = Generation.loadAllModulesFromJsonDirWith(false, jsonDir, kernelModules);
        long stepTime = System.currentTimeMillis() - stepStart;

        int totalBindings = 0;
        for (Module m : rawMods) {
            totalBindings += m.elements.size();
        }
        System.out.println("  Loaded " + rawMods.size() + " modules (" + totalBindings + " bindings).");
        System.out.println("  Time: " + formatTime(stepTime));
        System.out.println();

        // Main modules keep their type annotations from JSON (no stripping).
        // This allows the pipeline to skip pre-adaptation inference (Step 3 in
        // dataGraphToDefinitions), which is a major performance win.
        List<Module> allMods = rawMods;

        // Step 2: Filter modules
        List<Module> modsToGenerate = allMods;
        if (kernelOnly) {
            int before = modsToGenerate.size();
            modsToGenerate = Generation.filterKernelModules(modsToGenerate);
            allMods = Generation.filterKernelModules(allMods);
            System.out.println("Step 2: Filtering to kernel modules...");
            System.out.println("  Before: " + before + " modules");
            System.out.println("  After:  " + modsToGenerate.size() + " kernel modules (excluded "
                    + (before - modsToGenerate.size()) + " ext modules)");
            System.out.println();
        }
        if (typesOnly) {
            int before = modsToGenerate.size();
            modsToGenerate = Generation.filterTypeModules(modsToGenerate);
            System.out.println("Step 2b: Filtering to type modules...");
            System.out.println("  Before: " + before + " modules");
            System.out.println("  After:  " + modsToGenerate.size() + " type modules (excluded "
                    + (before - modsToGenerate.size()) + " non-type modules)");
            System.out.println();
        }

        // Debug: dump term structures for a specific module
        String dumpModule = System.getProperty("debug.dump");
        if (dumpModule != null) {
            for (Module mod : allMods) {
                if (mod.namespace.value.equals(dumpModule)) {
                    System.out.println("=== Dumping terms for " + dumpModule + " ===");
                    for (hydra.core.Binding b : mod.elements) {
                        System.out.println("--- " + b.name.value + " ---");
                        System.out.println(hydra.show.core.Core.term(b.term));
                        System.out.println();
                    }
                    System.out.println("=== End dump ===");
                }
            }
            return;
        }

        // List modules to generate
        System.out.println("Step 3: Generating " + target + " code...");
        System.out.println("  Universe: " + allMods.size() + " modules");
        System.out.println("  Generating: " + modsToGenerate.size() + " modules:");
        for (Module m : modsToGenerate) {
            int bindings = m.elements.size();
            System.out.println("    - " + m.namespace.value + " (" + bindings + " bindings)");
        }
        System.out.println("  Output: " + outDir);
        System.out.println();

        String outMain = outDir + File.separator + "src/gen-main";
        stepStart = System.currentTimeMillis();

        switch (target) {
            case "haskell":
                Generation.writeHaskell(outMain + "/haskell", allMods, modsToGenerate);
                break;
            case "java":
                Generation.writeJava(outMain + "/java", allMods, modsToGenerate);
                break;
            case "python":
                Generation.writePython(outMain + "/python", allMods, modsToGenerate);
                break;
            default:
                System.out.println("Unknown target: " + target);
                System.exit(1);
        }

        stepTime = System.currentTimeMillis() - stepStart;

        // Count main output files
        long mainFileCount = 0;
        String ext = target.equals("java") ? ".java" : target.equals("python") ? ".py" : ".hs";
        try {
            mainFileCount = Files.walk(Paths.get(outMain))
                    .filter(p -> p.toString().endsWith(ext))
                    .count();
        } catch (Exception e) {
            // ignore
        }

        System.out.println("  Generated " + mainFileCount + " main " + target + " files.");
        System.out.println("  Time: " + formatTime(stepTime));
        System.out.println();

        // Step 4: Load and generate test modules
        String testJsonDir = jsonDir.replace("gen-main/json", "gen-test/json");
        System.out.println("Step 4: Loading test modules from JSON...");
        System.out.println("  Source: " + testJsonDir);

        stepStart = System.currentTimeMillis();
        // Load test modules WITHOUT stripping TypeSchemes (stripTypeSchemes=false).
        // Test modules need their types preserved so inference can be skipped.
        List<Module> testMods = Generation.loadAllModulesFromJsonDirWith(false, testJsonDir, allMods);
        stepTime = System.currentTimeMillis() - stepStart;

        int testBindings = 0;
        for (Module m : testMods) {
            testBindings += m.elements.size();
        }
        System.out.println("  Loaded " + testMods.size() + " test modules (" + testBindings + " bindings).");
        System.out.println("  Time: " + formatTime(stepTime));
        System.out.println();

        List<Module> allUniverse = new ArrayList<>(allMods);
        allUniverse.addAll(testMods);

        String outTest = outDir + File.separator + "src/gen-test";
        System.out.println("Step 5: Generating " + target + " test code...");
        System.out.println("  Universe: " + allUniverse.size() + " modules");
        System.out.println("  Generating: " + testMods.size() + " test modules");
        System.out.println("  Output: " + outTest);
        System.out.println();

        stepStart = System.currentTimeMillis();

        boolean testGenOk = true;
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
            }
        } catch (Exception e) {
            testGenOk = false;
            System.out.println("  WARNING: Test generation failed: " + e.getMessage());
            System.out.println("  (Main code generation succeeded; test generation is a known issue)");
        }

        stepTime = System.currentTimeMillis() - stepStart;

        long testFileCount = 0;
        try {
            testFileCount = Files.walk(Paths.get(outTest))
                    .filter(p -> p.toString().endsWith(ext))
                    .count();
        } catch (Exception e) {
            // ignore
        }

        System.out.println("  Generated " + testFileCount + " test " + target + " files.");
        System.out.println("  Time: " + formatTime(stepTime));
        System.out.println();

        long totalTime = System.currentTimeMillis() - totalStart;

        System.out.println("==========================================");
        System.out.println("Bootstrap complete: java-to-" + target);
        System.out.println("==========================================");
        System.out.println("  Modules loaded:    " + rawMods.size() + " main + " + testMods.size() + " test");
        System.out.println("  Modules generated: " + modsToGenerate.size() + " main + " + testMods.size() + " test");
        System.out.println("  Output files:      " + mainFileCount + " main + " + testFileCount + " test");
        System.out.println("  Output directory:  " + outDir);
        System.out.println("  Total time:        " + formatTime(totalTime));
        System.out.println("==========================================");
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
