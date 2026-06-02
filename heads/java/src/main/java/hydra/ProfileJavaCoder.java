package hydra;

import hydra.typing.InferenceContext;
import hydra.core.Name;
import hydra.core.Term;
import hydra.errors.Error_;
import hydra.graph.Graph;
import hydra.packaging.Definition;
import hydra.packaging.Module;
import hydra.packaging.ModuleName;
import hydra.packaging.TermDefinition;
import hydra.util.Either;
import hydra.util.Maybe;

import java.io.File;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Profile inference scaling on prefix subsets of hydra.java.coder.
 *
 * Strips type schemes from the target module, then runs inference on the
 * first N defs (for N in a sweep) and prints wall-clock per N. Used to
 * isolate the >100x slowdown vs the synthetic linearChain benchmark.
 *
 * Usage: java hydra.ProfileJavaCoder [--sizes 1,5,10,20,50] [--namespace hydra.java.coder]
 */
public class ProfileJavaCoder {

    public static void main(String[] args) throws Exception {
        String ns = "hydra.java.coder";
        int[] sizes = {1, 5, 10, 20, 50, 100, 198};
        for (int i = 0; i < args.length; i++) {
            String a = args[i];
            if ("--namespace".equals(a) && i + 1 < args.length) ns = args[++i];
            else if ("--sizes".equals(a) && i + 1 < args.length) {
                String[] parts = args[++i].split(",");
                sizes = new int[parts.length];
                for (int j = 0; j < parts.length; j++) sizes[j] = Integer.parseInt(parts[j].trim());
            }
        }

        String worktreeRoot = System.getenv("HYDRA_ROOT");
        if (worktreeRoot == null || worktreeRoot.isEmpty()) {
            worktreeRoot = Paths.get("").toAbsolutePath().toString();
        }
        String kernelDir = worktreeRoot + "/dist/json/hydra-kernel/src/main/json";
        String javaDir   = worktreeRoot + "/dist/json/hydra-java/src/main/json";

        System.err.println("Loading kernel from " + kernelDir + " ...");
        Map<Name, hydra.core.Type> schemaMap = Generation.bootstrapSchemaMap();
        List<ModuleName> kernelNs = Generation.readManifestField(kernelDir, "mainModules");
        List<Module> universe = new ArrayList<>(
            Generation.loadModulesFromJson(kernelDir, schemaMap, kernelNs));
        System.err.println("  loaded " + universe.size() + " kernel modules");

        System.err.println("Loading hydra-java from " + javaDir + " ...");
        List<ModuleName> javaNs = Generation.readManifestField(javaDir, "mainModules");
        List<Module> javaMods = new ArrayList<>(
            Generation.loadModulesFromJson(javaDir, schemaMap, javaNs));
        for (Module m : javaMods) universe.add(m);
        System.err.println("  loaded " + javaMods.size() + " hydra-java modules");

        Module target = null;
        for (Module m : javaMods) {
            if (ns.equals(m.name.value)) { target = m; break; }
        }
        if (target == null) { System.err.println("Module " + ns + " not found"); System.exit(2); }
        System.err.println("Target " + ns + ": " + target.definitions.size() + " defs total");

        // Strip type schemes ONLY in the target module. Other hydra.java.*
        // modules stay typed (Serde, Names, etc. — their bindings already
        // carry inferred schemes from a prior typed JSON). The cost we want
        // to measure is inference for the target alone.
        Module strippedTarget = null;
        List<Module> universeStripped = new ArrayList<>();
        for (Module m : universe) {
            if (m.name.value.equals(ns)) {
                List<Definition> stripped = new ArrayList<>();
                for (Definition d : m.definitions) {
                    if (d instanceof Definition.Term) {
                        TermDefinition td = ((Definition.Term) d).value;
                        stripped.add(new Definition.Term(
                            new TermDefinition(td.name, hydra.util.Maybe.nothing(), td.term, Maybe.<hydra.typing.TermSignature>nothing())));
                    } else stripped.add(d);
                }
                strippedTarget = new Module(m.name, m.metadata, m.dependencies, stripped);
                universeStripped.add(strippedTarget);
            } else {
                universeStripped.add(m);
            }
        }

        InferenceContext ctx = new InferenceContext(0, new java.util.ArrayList<>());
        Graph bsGraph = Generation.bootstrapGraph();

        System.err.println("Profiling prefix sizes ...");
        for (int n : sizes) {
            n = Math.min(n, strippedTarget.definitions.size());
            List<Definition> prefix = new ArrayList<>(strippedTarget.definitions.subList(0, n));
            Module prefixMod = new Module(strippedTarget.name, strippedTarget.metadata,
                strippedTarget.dependencies, prefix);
            // Universe = stripped, with target replaced by prefix.
            List<Module> universeForCall = new ArrayList<>();
            for (Module m : universeStripped) {
                universeForCall.add(m.name.value.equals(ns) ? prefixMod : m);
            }
            long t0 = System.currentTimeMillis();
            Either<Error_, List<Module>> result =
                Codegen.inferModulesGiven(ctx, bsGraph, universeForCall,
                    Collections.singletonList(prefixMod));
            long elapsedMs = System.currentTimeMillis() - t0;
            boolean ok = result instanceof Either.Right;
            String status = ok ? "OK" : ("FAIL: " + summarize(result));
            System.err.println(String.format("  n=%3d: %8.2fs  %s", n, elapsedMs / 1000.0, status));
        }
    }

    private static String summarize(Either<Error_, List<Module>> r) {
        if (r instanceof Either.Left) {
            Error_ e = ((Either.Left<Error_, List<Module>>) r).value;
            String s = e.toString();
            return s.length() > 120 ? s.substring(0, 120) + "..." : s;
        }
        return "";
    }
}
