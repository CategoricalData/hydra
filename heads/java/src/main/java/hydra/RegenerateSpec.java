package hydra;

import hydra.build.overlay.java.Generation;
import hydra.core.markdown.Document;
import hydra.core.model.Name;
import hydra.core.packaging.Module;
import hydra.core.packaging.ModuleName;

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
 * hydra.core.Codegen.generateModuleDoc + hydra.core.print.Markdown.document (#723).
 *
 * Each page maps to one or more kernel module namespaces (PAGE_MODULES below).
 * Most primitives/ pages are 1:1 with a hydra.core.lib.<name> module; equality.md
 * and ordering.md are permanently out of per-module scope (they catalog primitives
 * by type-class membership across many source files, not by a single module).
 * Some types/ pages combine a type module with its paired hydra.core.error.<name>
 * module.
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

    // Matches CLAUDE.md's documented convention for generated source files verbatim, since no
    // distinct wording has been specified for generated prose pages. Prepended here, outside the
    // Markdown-AST, rather than folded into generateModuleDoc's Document content: hand-authored
    // pages put this notice BEFORE the title, but hydra.core.print.markdown's document printer
    // always emits "H1 title followed by its blocks" (its own documented, general-purpose
    // contract) -- so a Block.raw notice inside Document.content would land after the title.
    private static final String GENERATED_FILE_NOTICE =
            "<!-- Note: this is an automatically generated file. Do not edit. -->\n\n";

    /** page path (relative to docs/specification/) -> backing module namespaces. */
    private static final Map<String, List<String>> PAGE_MODULES = buildPageModules();

    private static Map<String, List<String>> buildPageModules() {
        Map<String, List<String>> m = new LinkedHashMap<>();
        // primitives/ — 1:1 with hydra.core.lib.<name>, except equality/ordering
        // (documented exclusions; see docs/specification/primitives/*.md IOU headers
        // and .claude/commands/regenerate-spec.md).
        for (String name : Arrays.asList(
                "chars", "effects", "eithers", "files", "functions", "hashing", "lists",
                "literals", "logic", "maps", "math", "optionals", "pairs", "regex", "sets",
                "strings", "system", "text")) {
            m.put("primitives/" + name + ".md", Arrays.asList("hydra.core.lib." + name));
        }
        // types/ — combines a type module with its paired hydra.core.error.<name> module
        // where one exists.
        m.put("types/files.md", Arrays.asList("hydra.core.file", "hydra.core.error.file"));
        m.put("types/system.md", Arrays.asList("hydra.core.system", "hydra.core.error.system"));
        m.put("types/time.md", Arrays.asList("hydra.core.time"));
        m.put("types/util.md", Arrays.asList("hydra.core.util"));
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
        Map<Name, hydra.core.model.Type> schemaMap = Generation.bootstrapSchemaMap();
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

            List<Document> docs = new ArrayList<>();
            for (String ns : namespaces) {
                Module mod = byNamespace.get(ns);
                if (mod == null) {
                    System.err.println("ERROR: module " + ns + " (backing page " + page
                            + ") not found in the loaded universe.");
                    System.exit(2);
                }
                docs.add(hydra.core.Codegen.generateModuleDoc(mod));
            }

            String rendered = renderPage(docs);
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
     * Renders one or more generateModuleDoc Documents as a single page: a single
     * generated-file notice up front, then each module's Document rendered in
     * namespace order and concatenated.
     */
    private static String renderPage(List<Document> docs) {
        StringBuilder sb = new StringBuilder(GENERATED_FILE_NOTICE);
        for (int i = 0; i < docs.size(); i++) {
            if (i > 0) {
                sb.append("\n\n");
            }
            sb.append(hydra.core.print.Markdown.document(docs.get(i)));
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
