package hydra;

import hydra.typing.InferenceContext;
import hydra.core.Name;
import hydra.core.Term;
import hydra.errors.Error_;
import hydra.graph.Graph;
import hydra.packaging.Definition;
import hydra.packaging.Module;
import hydra.packaging.ModuleDependency;
import hydra.packaging.ModuleName;
import hydra.packaging.TermDefinition;
import hydra.util.Either;
import hydra.util.Maybe;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * Cross-host inference benchmark — Java runner.
 *
 * Loads the synthetic {@code hydra.bench.inferenceScaling} workload from the
 * kernel JSON, takes prefixes of the chained walker definitions, and times
 * {@link hydra.Codegen#inferModulesGiven} on each prefix. Emits a JSON array
 * describing {@code {host, n, elapsed_seconds, ok}} per prefix size.
 *
 * Usage:
 * <pre>
 *   java -cp ... hydra.BenchInference [--sizes 10,25,50,100] [--out path/to/result.json]
 * </pre>
 *
 * Invoked by {@code bin/run-inference-bench.sh}, which dispatches to per-host
 * runners and aggregates results.
 */
public class BenchInference {

    private static final String DEFAULT_BENCH_NS = "hydra.bench.linearChain";
    private static final List<Integer> DEFAULT_SIZES =
        Arrays.asList(0, 10, 25, 50, 100);

    public static void main(String[] args) throws Exception {
        // Parse args
        List<Integer> sizes = DEFAULT_SIZES;
        String benchNs = DEFAULT_BENCH_NS;
        String outPath = null;
        for (int i = 0; i < args.length; i++) {
            String a = args[i];
            if ("--sizes".equals(a) && i + 1 < args.length) {
                sizes = parseSizes(args[++i]);
            } else if ("--namespace".equals(a) && i + 1 < args.length) {
                benchNs = args[++i];
            } else if ("--out".equals(a) && i + 1 < args.length) {
                outPath = args[++i];
            }
        }
        Collections.sort(sizes);

        // Resolve worktree root from the location of this class's source.
        // The bench wrapper script sets HYDRA_ROOT explicitly; honor it if set.
        String worktreeRoot = System.getenv("HYDRA_ROOT");
        if (worktreeRoot == null || worktreeRoot.isEmpty()) {
            // Fallback: assume working directory is the worktree root.
            worktreeRoot = Paths.get("").toAbsolutePath().toString();
        }
        String kernelMainDir = worktreeRoot
                + File.separator + "dist" + File.separator + "json"
                + File.separator + "hydra-kernel" + File.separator + "src"
                + File.separator + "main" + File.separator + "json";
        String benchMainDir = worktreeRoot
                + File.separator + "dist" + File.separator + "json"
                + File.separator + "hydra-bench" + File.separator + "src"
                + File.separator + "main" + File.separator + "json";

        System.err.println("Loading universe from " + kernelMainDir + " ...");
        long t0 = System.currentTimeMillis();
        // Seed the schema map with the bootstrap schema (kernel types) so module
        // decoding can resolve type-variable references.
        Map<Name, hydra.core.Type> schemaMap = Generation.bootstrapSchemaMap();
        List<ModuleName> mainNs = Generation.readManifestField(kernelMainDir, "mainModules");
        List<Module> universe = new ArrayList<>(
                Generation.loadModulesFromJson(kernelMainDir, schemaMap, mainNs));
        // hydra-bench is opt-in: populated by bin/sync-bench.sh, absent on a
        // default sync. If present, append its modules so the workload resolves.
        if (new java.io.File(benchMainDir).isDirectory()) {
            List<ModuleName> benchNsList = Generation.readManifestField(benchMainDir, "mainModules");
            universe.addAll(Generation.loadModulesFromJson(benchMainDir, schemaMap, benchNsList));
        }
        long loadMs = System.currentTimeMillis() - t0;
        System.err.println("  loaded " + universe.size() + " modules ("
                + String.format("%.1fs", loadMs / 1000.0) + ")");

        // Find the bench module.
        Module benchMod = null;
        for (Module m : universe) {
            if (benchNs.equals(m.name.value)) {
                benchMod = m;
                break;
            }
        }
        if (benchMod == null) {
            System.err.println("ERROR: bench module " + benchNs
                    + " not found. Run bin/sync-bench.sh to regenerate the hydra-bench package.");
            System.exit(2);
        }
        int avail = benchMod.definitions.size();
        System.err.println("Bench workload " + benchNs + ": " + avail + " definitions available");

        // Time each requested prefix size.
        List<BenchResult> results = new ArrayList<>();
        for (int n : sizes) {
            if (n > avail) {
                System.err.println("  skipping n=" + n + " (only " + avail + " defs available)");
                continue;
            }
            Module target = makeSyntheticModule(benchMod, n);
            BenchResult r = timeInference(universe, target, n, benchNs);
            String status = r.ok ? "OK" : "FAIL: " + r.error;
            System.err.println(String.format("  n=%3d: %6.2fs %s",
                    n, r.elapsedSeconds, status));
            results.add(r);
        }

        // Emit JSON.
        String json = renderJson(results);
        if (outPath != null) {
            try (PrintWriter pw = new PrintWriter(outPath)) {
                pw.print(json);
            }
            System.err.println("Wrote " + outPath);
        } else {
            System.out.print(json);
        }

        boolean allOk = true;
        for (BenchResult r : results) {
            if (!r.ok) { allOk = false; break; }
        }
        System.exit(allOk ? 0 : 1);
    }

    private static List<Integer> parseSizes(String s) {
        TreeSet<Integer> out = new TreeSet<>();
        for (String t : s.split(",")) {
            String trimmed = t.trim();
            if (!trimmed.isEmpty()) {
                out.add(Integer.parseInt(trimmed));
            }
        }
        return new ArrayList<>(out);
    }

    /**
     * Build a synthetic module with the first n walker defs renamed into
     * "z.bench.scaling". The bench module stays in the universe so
     * walker(k-1) lookups continue to resolve.
     */
    private static Module makeSyntheticModule(Module benchMod, int n) {
        ModuleName targetNs = new ModuleName("z.bench.scaling");
        List<Definition> defs = benchMod.definitions;
        List<Definition> renamed = new ArrayList<>(n);
        for (int i = 0; i < Math.min(n, defs.size()); i++) {
            Definition d = defs.get(i);
            if (d instanceof Definition.Term) {
                TermDefinition td = ((Definition.Term) d).value;
                String local = td.name.value.substring(td.name.value.lastIndexOf('.') + 1);
                Name newName = new Name(targetNs.value + "." + local);
                renamed.add(new Definition.Term(
                        new TermDefinition(newName, hydra.util.Maybe.nothing(), td.signature, td.body)));
            } else {
                renamed.add(d);
            }
        }
        // Inject the bench module's namespace as an explicit dependency
        // so walker_(k-1) lookups resolve via the universe.
        List<ModuleDependency> deps = new ArrayList<>();
        deps.add(new ModuleDependency(benchMod.name, hydra.util.Maybe.<hydra.packaging.PackageName>nothing()));
        deps.addAll(benchMod.dependencies);
        return new Module(targetNs, benchMod.metadata, deps, renamed);
    }

    private static BenchResult timeInference(List<Module> universe, Module target, int n, String benchNs) {
        // Build a context — empty is fine for this synthetic workload.
        InferenceContext cx = new InferenceContext(0, new java.util.ArrayList<>());
        Graph bsGraph = Generation.bootstrapGraph();

        List<Module> universeWithTarget = new ArrayList<>(universe);
        universeWithTarget.add(target);
        List<Module> targets = Collections.singletonList(target);

        long t0 = System.nanoTime();
        Either<Error_, List<Module>> result;
        boolean ok;
        String err = "";
        try {
            result = hydra.Codegen.inferModulesGiven(cx, bsGraph, universeWithTarget, targets);
            if (result instanceof Either.Right) {
                ok = true;
            } else if (result instanceof Either.Left) {
                ok = false;
                Error_ e = ((Either.Left<Error_, List<Module>>) result).value;
                err = truncate(e.toString(), 200);
            } else {
                ok = false;
                err = "unexpected Either: " + result.getClass().getName();
            }
        } catch (RuntimeException e) {
            ok = false;
            err = truncate(e.getMessage() == null ? e.toString() : e.getMessage(), 200);
        }
        long elapsedNs = System.nanoTime() - t0;
        double elapsedSec = elapsedNs / 1e9;
        return new BenchResult("java", benchNs, n, elapsedSec, ok, err);
    }

    private static String renderJson(List<BenchResult> results) {
        StringBuilder sb = new StringBuilder();
        sb.append("[\n");
        for (int i = 0; i < results.size(); i++) {
            BenchResult r = results.get(i);
            sb.append("  {\n");
            sb.append("    \"host\": \"").append(r.host).append("\",\n");
            sb.append("    \"namespace\": \"").append(r.namespace).append("\",\n");
            sb.append("    \"n\": ").append(r.n).append(",\n");
            sb.append("    \"elapsed_seconds\": ").append(String.format("%.6f", r.elapsedSeconds)).append(",\n");
            sb.append("    \"ok\": ").append(r.ok ? "true" : "false").append(",\n");
            sb.append("    \"error\": ").append(r.ok ? "null" : "\"" + escapeJson(r.error) + "\"").append("\n");
            sb.append("  }");
            if (i < results.size() - 1) sb.append(",");
            sb.append("\n");
        }
        sb.append("]\n");
        return sb.toString();
    }

    private static String escapeJson(String s) {
        if (s == null) return "";
        StringBuilder sb = new StringBuilder(s.length() + 8);
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            switch (c) {
                case '"': sb.append("\\\""); break;
                case '\\': sb.append("\\\\"); break;
                case '\n': sb.append("\\n"); break;
                case '\r': sb.append("\\r"); break;
                case '\t': sb.append("\\t"); break;
                default:
                    if (c < 0x20) {
                        sb.append(String.format("\\u%04x", (int) c));
                    } else {
                        sb.append(c);
                    }
            }
        }
        return sb.toString();
    }

    private static String truncate(String s, int max) {
        if (s == null) return "";
        return s.length() <= max ? s : s.substring(0, max);
    }

    private static class BenchResult {
        final String host;
        final String namespace;
        final int n;
        final double elapsedSeconds;
        final boolean ok;
        final String error;

        BenchResult(String host, String namespace, int n, double elapsedSeconds, boolean ok, String error) {
            this.host = host;
            this.namespace = namespace;
            this.n = n;
            this.elapsedSeconds = elapsedSeconds;
            this.ok = ok;
            this.error = error;
        }
    }
}
