package hydra;

import hydra.core.Name;
import hydra.overlay.java.build.Generation;
import hydra.packaging.Module;
import hydra.packaging.ModuleName;

import java.io.File;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Regenerates docs/specification/{primitives,types}/*.md from the kernel via
 * hydra.codegen.generateModuleDoc + hydra.print.markdown.document (#723).
 *
 * Each page maps to one or more kernel module namespaces (PAGE_MODULES below).
 * Most primitives/ pages are 1:1 with a hydra.lib.<name> module; three pages
 * are permanently out of per-module scope (equality.md/ordering.md are
 * type-class-scoped, not single-module; functions.md now has a backing module
 * but is not yet wired into this table — see the branch plan). Some types/
 * pages combine a type module with its paired hydra.error.<name> module.
 *
 * Usage:
 *   java hydra.RegenerateSpec [--module <namespace>]...
 *
 * With no --module flags, regenerates every page listed in PAGE_MODULES. One
 * or more --module flags scope regeneration to pages whose module list
 * intersects the given namespaces.
 *
 * Invoked by bin/regenerate-spec.sh, which resolves the classpath and fails
 * loudly on a non-zero exit or a missing "Wrote N page(s)" confirmation line.
 */
public class RegenerateSpec {

    /** page path (relative to docs/specification/) -> backing module namespaces. */
    private static final Map<String, List<String>> PAGE_MODULES = buildPageModules();

    private static Map<String, List<String>> buildPageModules() {
        Map<String, List<String>> m = new LinkedHashMap<>();
        // primitives/ — 1:1 with hydra.lib.<name>, except equality/ordering/functions
        // (documented exclusions; see docs/specification/primitives/*.md IOU headers).
        for (String name : Arrays.asList(
                "chars", "effects", "eithers", "files", "hashing", "lists", "literals",
                "logic", "maps", "math", "optionals", "pairs", "regex", "sets", "strings",
                "system", "text")) {
            m.put("primitives/" + name + ".md", Arrays.asList("hydra.lib." + name));
        }
        // types/ — combines a type module with its paired hydra.error.<name> module
        // where one exists.
        m.put("types/files.md", Arrays.asList("hydra.file", "hydra.error.file"));
        m.put("types/system.md", Arrays.asList("hydra.system", "hydra.error.system"));
        m.put("types/time.md", Arrays.asList("hydra.time"));
        m.put("types/util.md", Arrays.asList("hydra.util"));
        return m;
    }

    public static void main(String[] args) throws Exception {
        List<String> moduleFilters = new ArrayList<>();
        for (int i = 0; i < args.length; i++) {
            if ("--module".equals(args[i]) && i + 1 < args.length) {
                moduleFilters.add(args[++i]);
            } else {
                System.err.println("Usage: hydra.RegenerateSpec [--module <namespace>]...");
                System.exit(1);
            }
        }

        String worktreeRoot = System.getenv("HYDRA_ROOT");
        if (worktreeRoot == null || worktreeRoot.isEmpty()) {
            worktreeRoot = Paths.get("").toAbsolutePath().toString();
        }
        String kernelMainDir = worktreeRoot
                + File.separator + "dist" + File.separator + "json"
                + File.separator + "hydra-kernel" + File.separator + "src"
                + File.separator + "main" + File.separator + "json";

        System.err.println("Loading universe from " + kernelMainDir + " ...");
        Map<Name, hydra.core.Type> schemaMap = Generation.bootstrapSchemaMap();
        List<ModuleName> mainNs = Generation.readManifestField(kernelMainDir, "mainModules");
        List<Module> universe = Generation.loadModulesFromJson(kernelMainDir, schemaMap, mainNs);
        System.err.println("  loaded " + universe.size() + " modules");

        Map<String, Module> byNamespace = new LinkedHashMap<>();
        for (Module m : universe) {
            byNamespace.put(m.name.value, m);
        }

        int written = 0;
        for (Map.Entry<String, List<String>> entry : PAGE_MODULES.entrySet()) {
            String page = entry.getKey();
            List<String> namespaces = entry.getValue();
            if (!moduleFilters.isEmpty() && !containsAny(namespaces, moduleFilters)) {
                continue;
            }

            List<hydra.core.Term> sectionBlocks = new ArrayList<>();
            for (String ns : namespaces) {
                Module mod = byNamespace.get(ns);
                if (mod == null) {
                    System.err.println("ERROR: module " + ns + " (backing page " + page
                            + ") not found in the loaded universe.");
                    System.exit(2);
                }
                hydra.core.Term doc = hydra.Codegen.generateModuleDoc(mod);
                sectionBlocks.add(doc);
            }

            String rendered = renderPage(page, sectionBlocks);
            java.nio.file.Path outPath = Paths.get(worktreeRoot, "docs", "specification", page);
            Files.createDirectories(outPath.getParent());
            try (PrintWriter pw = new PrintWriter(outPath.toFile())) {
                pw.print(rendered);
            }
            written++;
            System.err.println("Wrote " + written + " page: " + page);
        }

        if (written == 0) {
            System.err.println("ERROR: no pages matched the given --module filter(s): " + moduleFilters);
            System.exit(2);
        }
        System.err.println("Wrote " + written + " page(s).");
    }

    /**
     * Renders one or more generateModuleDoc Document terms as a single page: the
     * first module's Document supplies the title, and every module's content
     * blocks (each already including its own generated-file notice) are
     * concatenated in namespace order.
     */
    private static String renderPage(String page, List<hydra.core.Term> docs) {
        StringBuilder sb = new StringBuilder();
        for (hydra.core.Term doc : docs) {
            sb.append(hydra.print.Markdown.document(doc));
        }
        return sb.toString();
    }

    private static boolean containsAny(List<String> namespaces, List<String> filters) {
        for (String ns : namespaces) {
            if (filters.contains(ns)) {
                return true;
            }
        }
        return false;
    }
}
