package hydra;

import hydra.core.Name;
import hydra.core.Type;
import hydra.overlay.java.build.Generation;
import hydra.packaging.Module;
import hydra.packaging.ModuleName;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Update {@code dist/json/hydra-java/} from the Java DSL sources in
 * {@code packages/hydra-java/src/main/java/hydra/sources/java/}, mirroring the
 * Python {@code update-python-json.py} driver. Originally introduced for issue
 * #344 as a "self-host demo"; now the canonical Java DSL → JSON step in the
 * regular sync pipeline (Phase 5).
 *
 * <p>The driver:</p>
 * <ol>
 *   <li>Loads the kernel universe from {@code dist/json/hydra-kernel/}.</li>
 *   <li>Loads the Java/JVM DSL source modules from the canonical package
 *       manifests ({@code hydra.sources.jvm.Manifest.mainModules} then
 *       {@code hydra.sources.java.Manifest.mainModules}).</li>
 *   <li>Runs {@link Codegen#inferModulesGiven} over (universe + sources).</li>
 *   <li>Builds a schema_map from the inferred graph.</li>
 *   <li>Encodes each module to JSON via {@link Codegen#moduleToJson}.</li>
 *   <li>Writes each output to
 *       {@code dist/json/hydra-java/src/main/json/hydra/java/&lt;module&gt;.json}.</li>
 * </ol>
 *
 * <p>Usage (run via {@code bin/update-java-json.sh}):</p>
 * <pre>
 *   java -cp ... hydra.UpdateJavaJson \
 *     --out-root dist/json/hydra-java/src/main/json \
 *     --hydra-root /path/to/worktree
 * </pre>
 */
public class UpdateJavaJson {

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
                System.out.println("Usage: java hydra.UpdateJavaJson "
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

        // 2. Load the Java/JVM DSL source modules from the canonical package
        // manifests (#559 Step B): hydra-jvm's manifest first (no deps on
        // hydra-java), then hydra-java's. Previously this driver hand-coded the
        // source-class list and loaded each module via reflection; those lists
        // duplicated the authoritative Manifest.mainModules declarations and
        // drifted whenever a package's source set changed. Consuming the
        // manifests directly removes the duplicate + the reflection.
        t0 = System.nanoTime();
        System.err.println("Loading Java DSL source modules ...");
        List<Module> sources = new ArrayList<>();
        sources.addAll(hydra.sources.jvm.Manifest.mainModules);
        sources.addAll(hydra.sources.java.Manifest.mainModules);
        for (Module m : sources) {
            System.err.println("    " + m.name.value + ": " + m.definitions.size() + " definitions");
        }
        double tImport = (System.nanoTime() - t0) / 1e9;
        if (sources.isEmpty()) {
            System.err.println("");
            System.err.println("ERROR: no Java DSL source modules found.");
            System.err.println("Expected classes under hydra.sources.java.* — see");
            System.err.println("packages/hydra-java/src/main/java/hydra/sources/java/.");
            System.err.println("Until those are ported from the Haskell DSL sources at");
            System.err.println("packages/hydra-java/src/main/haskell/Hydra/Sources/Java/,");
            System.err.println("this driver cannot generate any output.");
            System.exit(3);
        }
        System.err.println("  imported " + sources.size() + " java source modules ("
            + String.format("%.1f", tImport) + "s)");

        // 3. Per-package iterative inference + JSON write (mirrors the
        // Haskell-side inferAndWriteByPackage). For today's single-package
        // run (hydra-java sources only) this is effectively a one-iteration
        // loop, but the driver shape is in place for future multi-package
        // native-coder updates.
        t0 = System.nanoTime();
        System.err.println("Per-package inference + write ...");
        List<Module> universePlusSources = new ArrayList<>(universe);
        universePlusSources.addAll(sources);
        try {
            Generation.inferAndWriteByPackage(
                hydraRoot, distJsonRoot, universePlusSources, sources, universe);
            // #370/#346: also synthesize the DSL-wrapper modules
            // (hydra.dsl.java.{environment,syntax}) that the legacy Haskell
            // update-json-main DSL pass used to write. Now that hydra-java is
            // single-writer (no Haskell DSL fallback), the native driver owns its
            // full emission set. The DSL-type source modules are the type-defining
            // ones: hydra.java.environment and hydra.java.syntax.
            //
            // #556: hydra.java.language and hydra.jvm.serde are demand-curated in
            // too (mirroring the kernel's dslTermModules, #467) — they're the
            // source modules behind the 5 inline var("hydra.java.language..."/
            // "hydra.jvm.serde...") string refs this issue retires. Unlike the
            // type-only modules above, these are TERM-only, so their term
            // definitions need a TermSignature before Dsls.generateRefBindings will
            // emit anything for them — see reloadTermSignatureSources below.
            List<Module> dslTypeMods = new ArrayList<>();
            for (Module m : sources) {
                String ns = m.name.value;
                if (ns.equals("hydra.java.environment") || ns.equals("hydra.java.syntax")
                        || ns.equals("hydra.java.language") || ns.equals("hydra.jvm.serde")) {
                    dslTypeMods.add(m);
                }
            }
            // #467/#556: the raw dslTypeMods entries have termDefinitionSignature =
            // None (inference never runs on derived modules); reload the
            // term-bearing ones from their just-written dist/json counterparts,
            // which carry inferAndWriteByPackage's inferred signatures.
            List<Module> dslSourcesForSynthesis = Generation.reloadTermSignatureSources(
                distJsonRoot, universePlusSources, dslTypeMods);
            List<Module> writtenDsl = Generation.synthesizeAndWriteDslModules(
                distJsonRoot, universePlusSources, dslSourcesForSynthesis);
            System.err.println("  DSL wrappers: " + writtenDsl.size() + " module(s) written");

            // #511: synthesize term encode/decode wrappers (hydra.encode.* /
            // hydra.decode.*) for native type modules that must be read from /
            // written to JSON as typed values. hydra.gradle is the build-config
            // type whose overlay build.json is read via the canonical
            // Decode.fromJson → Term → hydra.decode.gradle path (exactly as
            // hydra.packaging.Module is read via hydra.decode.packaging). The
            // native Java path previously synthesized only DSL wrappers; this adds
            // the encode/decode synthesis it lacked.
            List<Module> encTypeMods = new ArrayList<>();
            for (Module m : sources) {
                if (m.name.value.equals("hydra.gradle")) {
                    encTypeMods.add(m);
                }
            }
            List<Module> writtenCoders = Generation.synthesizeAndWriteEncodeDecodeModules(
                distJsonRoot, universePlusSources, encTypeMods);
            System.err.println("  encode/decode wrappers: " + writtenCoders.size() + " module(s) written");

            // #511: write the native packages' manifest.json source-driven (rich
            // schema, partitioned by Generation.groupByPackage, #560). Previously
            // these manifests were static/stale, so a new module (hydra.gradle)
            // never appeared and routing silently fell back to hydra-kernel.
            // mainModules = the loaded source modules (includes hydra.gradle);
            // mainDslModules = the DSL-type source modules; mainEncodingModules =
            // the encode/decode source modules (hydra.gradle).
            Generation.writePackageManifests(
                distJsonRoot, sources, dslTypeMods, encTypeMods);
        } catch (RuntimeException | java.io.IOException ex) {
            System.err.println("  FAILED: " + ex.getMessage());
            Throwable cause = (ex instanceof RuntimeException) ? ex.getCause() : ex;
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
