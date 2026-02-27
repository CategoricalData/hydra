package hydra;

import hydra.module.Module;
import hydra.module.Namespace;

import java.io.File;
import java.nio.file.Files;
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
 *
 * Usage:
 * <pre>{@code
 *   java hydra.Bootstrap --target <haskell|java|python> --json-dir <path> [OPTIONS]
 *
 * Options:
 *   --output <dir>         Output base directory (default: /tmp/hydra-bootstrapping-demo)
 *   --include-coders       Also load and generate ext coder modules
 *   --include-tests        Also load and generate kernel test modules
 *   --ext-json-dir <dir>   Directory containing ext JSON modules (for --include-coders)
 *   --kernel-only          Only generate kernel modules (exclude hydra.ext.*)
 *   --types-only           Only generate type-defining modules
 * }</pre>
 */
public class Bootstrap {

    public static void main(String[] args) throws Exception {
        long totalStart = System.currentTimeMillis();

        String target = null;
        String jsonDir = null;
        String extJsonDir = null;
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
                    if (i + 1 < args.length) jsonDir = args[++i];
                    break;
                case "--ext-json-dir":
                    if (i + 1 < args.length) extJsonDir = args[++i];
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

        if (target == null || jsonDir == null) {
            System.out.println("Usage: java hydra.Bootstrap --target <haskell|java|python> --json-dir <path> [OPTIONS]");
            System.out.println();
            System.out.println("Options:");
            System.out.println("  --output <dir>         Output base directory");
            System.out.println("  --include-coders       Also load and generate ext coder modules");
            System.out.println("  --include-tests        Also load and generate kernel test modules");
            System.out.println("  --ext-json-dir <dir>   Directory containing ext JSON modules (for --include-coders)");
            System.out.println("  --kernel-only          Only generate kernel modules (exclude hydra.ext.*)");
            System.out.println("  --types-only           Only generate type-defining modules");
            System.exit(1);
        }

        if (includeCoders && extJsonDir == null) {
            System.out.println("Error: --include-coders requires --ext-json-dir");
            System.exit(1);
        }

        String targetCap = target.substring(0, 1).toUpperCase() + target.substring(1);
        String outDir = outBase + File.separator + "java-to-" + target;

        System.out.println("==========================================");
        System.out.println("Mapping JSON to " + targetCap + " (via Java host)");
        System.out.println("==========================================");
        System.out.println("  Host language:   Java");
        System.out.println("  Target language: " + targetCap);
        System.out.println("  JSON directory:  " + jsonDir);
        System.out.println("  Output:          " + outDir);
        System.out.println("  Include coders:  " + includeCoders);
        System.out.println("  Include tests:   " + includeTests);
        if (typesOnly) System.out.println("  Filter:          types only");
        if (kernelOnly) System.out.println("  Filter:          kernel only");
        System.out.println("==========================================");
        System.out.println();

        // Step 1: Load main + eval lib modules from JSON
        System.out.println("Step 1: Loading main modules from JSON...");
        System.out.println("  Source: " + jsonDir);

        long stepStart = System.currentTimeMillis();
        List<Namespace> mainNamespaces = Generation.readManifestField(jsonDir, "mainModules");
        List<Namespace> evalLibNamespaces = Generation.readManifestField(jsonDir, "evalLibModules");
        List<Namespace> allKernelNamespaces = new ArrayList<>(mainNamespaces);
        allKernelNamespaces.addAll(evalLibNamespaces);
        java.util.Map<hydra.core.Name, hydra.core.Type> schemaMap = Generation.bootstrapSchemaMap();
        List<Module> mainMods = Generation.loadModulesFromJson(false, jsonDir,
                schemaMap, allKernelNamespaces);
        long stepTime = System.currentTimeMillis() - stepStart;

        int totalBindings = 0;
        for (Module m : mainMods) {
            totalBindings += m.elements.size();
        }
        System.out.println("  Loaded " + mainMods.size() + " modules (" + totalBindings + " bindings).");
        System.out.println("  Time: " + formatTime(stepTime));
        System.out.println();

        // Step 2: Optionally load ext coder modules
        List<Module> coderMods = new ArrayList<>();
        if (includeCoders) {
            System.out.println("Step 2: Loading hydra-ext coder modules from JSON...");
            List<Namespace> coderNamespaces = Generation.readManifestField(extJsonDir, "hydraCoderModules");
            // Filter out modules already loaded as part of kernel
            Set<String> kernelNsSet = new HashSet<>();
            for (Namespace ns : allKernelNamespaces) {
                kernelNsSet.add(ns.value);
            }
            List<Namespace> extCoderNamespaces = new ArrayList<>();
            for (Namespace ns : coderNamespaces) {
                if (!kernelNsSet.contains(ns.value)) {
                    extCoderNamespaces.add(ns);
                }
            }
            stepStart = System.currentTimeMillis();
            coderMods = Generation.loadModulesFromJson(false, extJsonDir,
                    schemaMap, extCoderNamespaces);
            stepTime = System.currentTimeMillis() - stepStart;
            System.out.println("  Loaded " + coderMods.size() + " modules.");
            System.out.println("  Time: " + formatTime(stepTime));
            System.out.println();
        } else {
            System.out.println("Step 2: Skipping ext coder modules");
            System.out.println();
        }

        List<Module> allMainMods = new ArrayList<>(mainMods);
        allMainMods.addAll(coderMods);

        // Apply filters
        List<Module> modsToGenerate = allMainMods;
        if (kernelOnly) {
            int before = modsToGenerate.size();
            modsToGenerate = Generation.filterKernelModules(modsToGenerate);
            allMainMods = Generation.filterKernelModules(allMainMods);
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
        List<Module> fullMods = new ArrayList<>(mainMods);
        fullMods.addAll(coderMods);

        // Generate main modules
        String outMain = outDir + File.separator + "src/gen-main";
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
            default:
                System.out.println("Unknown target: " + target);
                System.exit(1);
        }

        stepTime = System.currentTimeMillis() - stepStart;

        String ext = target.equals("java") ? ".java" : target.equals("python") ? ".py" : ".hs";
        long mainFileCount = 0;
        try {
            mainFileCount = Files.walk(Paths.get(outMain))
                    .filter(p -> p.toString().endsWith(ext))
                    .count();
        } catch (Exception e) {
            // ignore
        }

        System.out.println("  Generated " + mainFileCount + " files.");
        System.out.println("  Time: " + formatTime(stepTime));
        System.out.println();

        // Optionally load and generate test modules
        long testFileCount = 0;
        if (includeTests) {
            String testJsonDir = jsonDir.replace("gen-main/json", "gen-test/json");
            System.out.println("Loading test modules from JSON...");
            System.out.println("  Source: " + testJsonDir);

            stepStart = System.currentTimeMillis();
            List<Namespace> testNamespaces = Generation.readManifestField(jsonDir, "testModules");
            List<Module> testMods = Generation.loadModulesFromJson(false, testJsonDir,
                    schemaMap, testNamespaces);
            stepTime = System.currentTimeMillis() - stepStart;

            int testBindings = 0;
            for (Module m : testMods) {
                testBindings += m.elements.size();
            }
            System.out.println("  Loaded " + testMods.size() + " test modules (" + testBindings + " bindings).");
            System.out.println("  Time: " + formatTime(stepTime));
            System.out.println();

            List<Module> allUniverse = new ArrayList<>(fullMods);
            allUniverse.addAll(testMods);

            String outTest = outDir + File.separator + "src/gen-test";
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
                }
            } catch (Exception e) {
                System.out.println("  WARNING: Test generation failed: " + e.getMessage());
            }

            stepTime = System.currentTimeMillis() - stepStart;

            try {
                testFileCount = Files.walk(Paths.get(outTest))
                        .filter(p -> p.toString().endsWith(ext))
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
