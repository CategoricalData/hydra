package hydra;

import hydra.context.Context;
import hydra.core.Name;
import hydra.core.Type;
import hydra.errors.Error_;
import hydra.graph.Graph;
import hydra.packaging.Module;
import hydra.packaging.ModuleName;
import hydra.util.Either;

import java.io.File;
import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Generate {@code dist/json/hydra-java/} from the Java DSL sources in
 * {@code packages/hydra-java/src/main/java/hydra/sources/java/}, mirroring the
 * Python self-host demo for issue #344.
 *
 * <p>The driver:</p>
 * <ol>
 *   <li>Loads the kernel universe from {@code dist/json/hydra-kernel/}.</li>
 *   <li>Loads the Java DSL source modules' {@code module_} static fields via
 *       reflection.</li>
 *   <li>Runs {@link Codegen#inferModulesGiven} over (universe + sources).</li>
 *   <li>Builds a schema_map from the inferred graph.</li>
 *   <li>Encodes each module to JSON via {@link Codegen#moduleToJson}.</li>
 *   <li>Writes each output to
 *       {@code dist/json/hydra-java/src/main/json/hydra/java/&lt;module&gt;.json}.</li>
 * </ol>
 *
 * <p>Usage (run via {@code bin/generate-hydra-java-from-java.sh}):</p>
 * <pre>
 *   java -cp ... hydra.JavaSelfHostDemo \
 *     --out-root dist/json/hydra-java/src/main/json \
 *     --hydra-root /path/to/worktree
 * </pre>
 */
public class JavaSelfHostDemo {

    /** Static-field name on each source class that returns the source module. */
    private static final String MODULE_FIELD = "module_";

    /**
     * Java DSL source modules under {@code hydra.sources.java.*}. The
     * fully-qualified class names exposing a static {@code module_} field of
     * type {@link Module}. Order mirrors the Haskell {@code mainModules} list
     * in {@code Hydra.Sources.Java.Manifest}.
     */
    private static final List<String> SOURCE_CLASS_NAMES = java.util.Arrays.asList(
        "hydra.sources.java.Coder",
        "hydra.sources.java.Environment",
        "hydra.sources.java.Language",
        "hydra.sources.java.Names",
        "hydra.sources.java.Serde",
        "hydra.sources.java.Syntax",
        "hydra.sources.java.Testing",
        "hydra.sources.java.Utils"
    );

    public static void main(String[] args) throws Exception {
        String hydraRoot = null;
        String outRoot = null;
        for (int i = 0; i < args.length; i++) {
            String a = args[i];
            if ("--hydra-root".equals(a) && i + 1 < args.length) {
                hydraRoot = args[++i];
            } else if ("--out-root".equals(a) && i + 1 < args.length) {
                outRoot = args[++i];
            } else if ("--help".equals(a) || "-h".equals(a)) {
                System.out.println("Usage: java hydra.JavaSelfHostDemo "
                    + "--hydra-root DIR --out-root DIR");
                System.exit(0);
            }
        }
        if (hydraRoot == null) {
            System.err.println("--hydra-root is required");
            System.exit(2);
        }
        if (outRoot == null) {
            outRoot = hydraRoot + "/dist/json/hydra-java/src/main/json";
        }

        String kernelJson = hydraRoot + "/dist/json/hydra-kernel/src/main/json";

        // 1. Load the kernel universe.
        long t0 = System.nanoTime();
        System.err.println("Loading universe from " + kernelJson + " ...");
        Map<Name, Type> bootstrapSchema = Generation.bootstrapSchemaMap();
        List<ModuleName> mainModulesNs = Generation.readManifestField(kernelJson, "mainModules");
        List<Module> universe = Generation.loadModulesFromJson(kernelJson, bootstrapSchema, mainModulesNs);
        double tUniverse = (System.nanoTime() - t0) / 1e9;
        System.err.println("  loaded " + universe.size() + " kernel modules ("
            + String.format("%.1f", tUniverse) + "s)");

        // 2. Discover and load Java DSL source modules.
        t0 = System.nanoTime();
        System.err.println("Loading Java DSL source modules ...");
        List<Module> sources = new ArrayList<>();
        for (String className : SOURCE_CLASS_NAMES) {
            try {
                Class<?> cls = Class.forName(className);
                Object val = cls.getDeclaredField(MODULE_FIELD).get(null);
                if (!(val instanceof Module)) {
                    System.err.println("  WARNING: " + className + "." + MODULE_FIELD
                        + " is " + val.getClass().getName() + ", expected Module; skipping");
                    continue;
                }
                Module m = (Module) val;
                sources.add(m);
                int nDefs = m.definitions.size();
                System.err.println("    " + m.name.value + ": " + nDefs + " definitions");
            } catch (ClassNotFoundException e) {
                System.err.println("  MISSING: " + className
                    + " (skipping; expected when port is incomplete)");
            }
        }
        double tImport = (System.nanoTime() - t0) / 1e9;
        if (sources.isEmpty()) {
            System.err.println("");
            System.err.println("ERROR: no Java DSL source modules found.");
            System.err.println("Expected classes under hydra.sources.java.* — see");
            System.err.println("packages/hydra-java/src/main/java/hydra/sources/java/.");
            System.err.println("Until those are ported from the Haskell DSL sources at");
            System.err.println("packages/hydra-java/src/main/haskell/Hydra/Sources/Java/,");
            System.err.println("this self-host demo cannot generate any output.");
            System.exit(3);
        }
        System.err.println("  imported " + sources.size() + " java source modules ("
            + String.format("%.1f", tImport) + "s)");

        // 3. Inference over universe + sources.
        // Bindings arrive pre-annotated from module_to_source_module, so inference
        // is a no-op pass-through for the source modules; we still run it to keep
        // the pipeline shape identical to other hosts.
        t0 = System.nanoTime();
        System.err.println("Inferring " + sources.size() + " java source modules ...");
        Context ctx = new Context(
            Collections.emptyList(),
            Collections.emptyList(),
            Collections.emptyMap());
        Graph bsGraph = Generation.bootstrapGraph();
        List<Module> universePlusSources = new ArrayList<>(universe);
        universePlusSources.addAll(sources);
        Either<Error_, List<Module>> result =
            Codegen.inferModulesGiven(ctx, bsGraph, universePlusSources, sources);
        double tInfer = (System.nanoTime() - t0) / 1e9;
        if (result instanceof Either.Left) {
            Error_ err = ((Either.Left<Error_, List<Module>>) result).value;
            System.err.println("  INFERENCE FAILED: " + err);
            try {
                String shown = hydra.show.Errors.error(err);
                System.err.println("  details: " + shown);
            } catch (Throwable t) {
                System.err.println("  (no detail formatter available: " + t.getMessage() + ")");
            }
            System.exit(4);
        }
        List<Module> inferred =
            ((Either.Right<Error_, List<Module>>) result).value;
        System.err.println("  inferred (" + String.format("%.1f", tInfer) + "s)");

        // 4. Build graph + schema_map.
        t0 = System.nanoTime();
        System.err.println("Building graph + schema_map ...");
        List<Module> graphInput = new ArrayList<>(universe);
        graphInput.addAll(inferred);
        Graph graph = Codegen.modulesToGraph(bsGraph, graphInput, inferred);
        Map<Name, Type> schemaMap = Codegen.buildSchemaMap(graph);
        double tGraph = (System.nanoTime() - t0) / 1e9;
        System.err.println("  built (" + String.format("%.1f", tGraph) + "s; schema_map has "
            + schemaMap.size() + " entries)");

        // 5. Encode each module + write to disk.
        t0 = System.nanoTime();
        System.err.println("Writing JSON to " + outRoot + " ...");
        Files.createDirectories(Paths.get(outRoot));
        int nWritten = 0, nUnchanged = 0;
        for (Module m : inferred) {
            Either<Error_, String> encoded = Codegen.moduleToJson(schemaMap, m);
            if (encoded instanceof Either.Left) {
                Error_ err = ((Either.Left<Error_, String>) encoded).value;
                System.err.println("  ENCODE FAILED for " + m.name.value + ": " + err);
                System.exit(5);
            }
            String jsonStr = ((Either.Right<Error_, String>) encoded).value;
            String namespacePath = m.name.value.replace('.', '/');
            Path filePath = Paths.get(outRoot, namespacePath + ".json");
            Files.createDirectories(filePath.getParent());
            String newContent = jsonStr + "\n";
            boolean skip = false;
            if (Files.exists(filePath)) {
                String oldContent = new String(Files.readAllBytes(filePath));
                if (oldContent.equals(newContent)) {
                    skip = true;
                    nUnchanged++;
                    System.err.println("  unchanged: " + filePath);
                }
            }
            if (!skip) {
                try (PrintWriter pw = new PrintWriter(filePath.toFile())) {
                    pw.print(newContent);
                }
                nWritten++;
                System.err.println("  wrote: " + filePath);
            }
        }
        double tWrite = (System.nanoTime() - t0) / 1e9;
        System.err.println("  wrote " + nWritten + " files (" + nUnchanged + " unchanged) ("
            + String.format("%.1f", tWrite) + "s)");

        // 6. Summary.
        double tTotal = tUniverse + tImport + tInfer + tGraph + tWrite;
        System.err.println("");
        System.err.println("Summary:");
        System.err.println("  universe load: " + String.format("%6.1f", tUniverse) + "s");
        System.err.println("  source load:   " + String.format("%6.1f", tImport) + "s");
        System.err.println("  inference:     " + String.format("%6.1f", tInfer) + "s");
        System.err.println("  graph+schema:  " + String.format("%6.1f", tGraph) + "s");
        System.err.println("  json write:    " + String.format("%6.1f", tWrite) + "s");
        System.err.println("  total:         " + String.format("%6.1f", tTotal) + "s");
    }
}
