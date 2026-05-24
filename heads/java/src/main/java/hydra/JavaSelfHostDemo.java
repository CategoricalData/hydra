package hydra;

import hydra.core.Name;
import hydra.core.Type;
import hydra.packaging.Module;
import hydra.packaging.ModuleName;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
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

        // Resolve the dist/json root from --out-root. Canonical layout has the
        // four-segment ".../hydra-java/src/main/json" tail; strip it to recover
        // the dist-json root that the per-package driver expects. Non-canonical
        // paths (e.g. /tmp targets for --compare) are treated verbatim.
        Path outRootPath = Paths.get(outRoot).toAbsolutePath().normalize();
        String[] tail = {"hydra-java", "src", "main", "json"};
        String distJsonRoot;
        boolean canonicalLayout = true;
        if (outRootPath.getNameCount() >= tail.length) {
            for (int k = 0; k < tail.length; k++) {
                if (!tail[k].equals(outRootPath.getName(
                        outRootPath.getNameCount() - tail.length + k).toString())) {
                    canonicalLayout = false;
                    break;
                }
            }
        } else {
            canonicalLayout = false;
        }
        if (canonicalLayout) {
            distJsonRoot = outRootPath.subpath(0, outRootPath.getNameCount() - tail.length)
                .toString();
            if (outRootPath.isAbsolute()) {
                distJsonRoot = "/" + distJsonRoot;
            }
        } else {
            distJsonRoot = outRootPath.toString();
        }
        System.err.println("Output dist/json root: " + distJsonRoot);

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

        // 3. Per-package iterative inference + JSON write (mirrors the
        // Haskell-side inferAndWriteByPackage). For today's single-package
        // demo (hydra-java sources only) this is effectively a one-iteration
        // loop, but the driver shape is in place for future multi-package
        // self-hosts.
        t0 = System.nanoTime();
        System.err.println("Per-package inference + write ...");
        List<Module> universePlusSources = new ArrayList<>(universe);
        universePlusSources.addAll(sources);
        try {
            Generation.inferAndWriteByPackage(
                hydraRoot, distJsonRoot, universePlusSources, sources, universe);
        } catch (RuntimeException ex) {
            System.err.println("  FAILED: " + ex.getMessage());
            Throwable cause = ex.getCause();
            if (cause != null) {
                System.err.println("  cause: " + cause);
            }
            System.exit(4);
        }
        double tPkg = (System.nanoTime() - t0) / 1e9;
        System.err.println("  done (" + String.format("%.1f", tPkg) + "s)");

        // 4. Summary.
        double tTotal = tUniverse + tImport + tPkg;
        System.err.println("");
        System.err.println("Summary:");
        System.err.println("  universe load:        " + String.format("%6.1f", tUniverse) + "s");
        System.err.println("  source load:          " + String.format("%6.1f", tImport) + "s");
        System.err.println("  per-package + write:  " + String.format("%6.1f", tPkg) + "s");
        System.err.println("  total:                " + String.format("%6.1f", tTotal) + "s");
    }
}
