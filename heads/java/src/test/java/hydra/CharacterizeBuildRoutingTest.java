package hydra;

import hydra.build.Routing;
import hydra.overlay.java.util.Either;
import hydra.overlay.java.util.Pair;
import hydra.packaging.Module;
import hydra.packaging.ModuleName;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Temporary characterization test for #416 Phase 2 pilot: compares the generated
 * {@code hydra.build.Routing} against the legacy {@code PACKAGE_PREFIXES} table in
 * {@link Generation}, over every namespace declared in the current package manifests.
 *
 * <p>Not part of the permanent test suite -- a one-off parity check for the pilot.
 */
public class CharacterizeBuildRoutingTest {

    /**
     * Known, expected divergences between the legacy {@code PACKAGE_PREFIXES} table and the
     * manifest-derived generated module, confirmed 2026-07-02: the legacy table is missing
     * prefix entries for these namespaces and silently falls back to {@code "hydra-kernel"},
     * while the generated module correctly routes them per the manifest. Verified latent (not
     * live): the Java driver is hard-scoped to its own package's sources, so these namespaces
     * never actually reach {@code namespaceToPackage} in production. New entries here should be
     * scrutinized, not just added -- they represent a real gap in the legacy table.
     */
    private static final Map<String, String> EXPECTED_DIVERGENCES = new TreeMap<>();
    static {
        EXPECTED_DIVERGENCES.put("hydra.decode.neo4j.model", "hydra-pg");
        EXPECTED_DIVERGENCES.put("hydra.encode.neo4j.model", "hydra-pg");
        EXPECTED_DIVERGENCES.put("hydra.error.neo4j", "hydra-pg");
        EXPECTED_DIVERGENCES.put("hydra.neo4j.model", "hydra-pg");
        EXPECTED_DIVERGENCES.put("hydra.neo4j.pg", "hydra-pg");
        EXPECTED_DIVERGENCES.put("hydra.validate.neo4j", "hydra-pg");
        // #546: the build modules were extracted from hydra-kernel into hydra-build.
        // The legacy PACKAGE_PREFIXES table has no hydra.build. entry, so it falls back
        // to hydra-kernel, while the manifest correctly routes them to hydra-build.
        EXPECTED_DIVERGENCES.put("hydra.build.modules", "hydra-build");
        EXPECTED_DIVERGENCES.put("hydra.build.reconcile", "hydra-build");
        EXPECTED_DIVERGENCES.put("hydra.build.routing", "hydra-build");
        EXPECTED_DIVERGENCES.put("hydra.test.build.modules", "hydra-build");
        EXPECTED_DIVERGENCES.put("hydra.test.build.reconcile", "hydra-build");
        EXPECTED_DIVERGENCES.put("hydra.test.build.routing", "hydra-build");
        // #497: hydra.show.* -> hydra.print.* rename. The legacy PACKAGE_PREFIXES table's
        // hydra-pg entry is still keyed "hydra.show.error.pg" (pinned to the PUBLISHED
        // hydra-java host's pre-rename namespace; see Generation.java's own note on that
        // entry), so it has no prefix match for the new "hydra.print.error.pg" and falls
        // back to hydra-kernel, while the manifest correctly routes it to hydra-pg.
        EXPECTED_DIVERGENCES.put("hydra.print.error.pg", "hydra-pg");
    }

    // Minimal hand-rolled extraction for manifest.json's flat {"package": "...",
    // "mainModules": [...], "testModules": [...]} shape. Not a general JSON parser --
    // deliberately narrow since this is throwaway characterization code.
    private static String extractStringField(String json, String field) {
        Matcher m = Pattern.compile("\"" + field + "\"\\s*:\\s*\"([^\"]*)\"").matcher(json);
        if (!m.find()) {
            throw new IllegalStateException("field not found: " + field);
        }
        return m.group(1);
    }

    private static List<String> extractStringArrayField(String json, String field) {
        Matcher arrayMatcher = Pattern.compile("\"" + field + "\"\\s*:\\s*\\[([^\\]]*)]").matcher(json);
        if (!arrayMatcher.find()) {
            throw new IllegalStateException("array field not found: " + field);
        }
        String body = arrayMatcher.group(1);
        List<String> result = new ArrayList<>();
        Matcher elementMatcher = Pattern.compile("\"([^\"]*)\"").matcher(body);
        while (elementMatcher.find()) {
            result.add(elementMatcher.group(1));
        }
        return result;
    }

    private static List<Pair<String, List<ModuleName>>> loadManifests(Path distJsonRoot) throws IOException {
        List<Pair<String, List<ModuleName>>> pkgs = new ArrayList<>();
        try (var stream = Files.newDirectoryStream(distJsonRoot)) {
            List<Path> pkgDirs = new ArrayList<>();
            stream.forEach(pkgDirs::add);
            pkgDirs.sort((a, b) -> a.getFileName().toString().compareTo(b.getFileName().toString()));
            for (Path pkgDir : pkgDirs) {
                Path manifestPath = pkgDir.resolve("src/main/json/manifest.json");
                if (!Files.isRegularFile(manifestPath)) {
                    continue;
                }
                String json = Files.readString(manifestPath);
                String pkgName = extractStringField(json, "package");
                // derivedMainModules is not a manifest JSON field; derived names (hydra.dsl.*,
                // hydra.encode.*, hydra.decode.*, hydra.sources.*) are synthesized by
                // Routing.buildRoutingMap itself via derivedNames, so mainModules + testModules
                // is the correct (and complete) declared-module input here.
                Set<String> declared = new LinkedHashSet<>();
                declared.addAll(extractStringArrayField(json, "mainModules"));
                declared.addAll(extractStringArrayField(json, "testModules"));
                List<ModuleName> moduleNames = new ArrayList<>();
                for (String s : declared) {
                    moduleNames.add(new ModuleName(s));
                }
                pkgs.add(new Pair<>(pkgName, moduleNames));
            }
        }
        return pkgs;
    }

    @Test
    public void characterizeAgainstLegacyTable() throws IOException {
        Path root = Paths.get("").toAbsolutePath().getParent().getParent();
        Path distJsonRoot = root.resolve("dist/json");
        assertTrue(Files.isDirectory(distJsonRoot), "dist/json not found at " + distJsonRoot);

        List<Pair<String, List<ModuleName>>> pkgs = loadManifests(distJsonRoot);
        Set<String> allNamespaces = new TreeSet<>();
        for (Pair<String, List<ModuleName>> p : pkgs) {
            for (ModuleName m : p.second) {
                allNamespaces.add(m.value);
            }
        }
        System.out.println("Loaded " + pkgs.size() + " packages, " + allNamespaces.size()
            + " distinct declared namespaces.");

        Map<ModuleName, String> rm = Routing.buildRoutingMap(pkgs);

        int agree = 0;
        Set<String> expectedDivergencesSeen = new TreeSet<>();
        List<String> unexpectedDisagreements = new ArrayList<>();
        List<String> failLoudHits = new ArrayList<>();
        for (String ns : allNamespaces) {
            ModuleName moduleName = new ModuleName(ns);
            String legacyPkg = Generation.namespaceToPackage(moduleName);
            Either<hydra.errors.Error_, String> generated = Routing.namespaceToPackageIn(rm, moduleName);
            if (generated.isRight()) {
                String generatedPkg = ((Either.Right<hydra.errors.Error_, String>) generated).value;
                if (generatedPkg.equals(legacyPkg)) {
                    agree++;
                } else if (generatedPkg.equals(EXPECTED_DIVERGENCES.get(ns))) {
                    expectedDivergencesSeen.add(ns);
                } else {
                    unexpectedDisagreements.add(ns + ": legacy=" + legacyPkg + " generated=" + generatedPkg);
                }
            } else {
                // A declared namespace that the generated module fails loud on is always a bug
                // (every declared namespace must route somewhere) -- never expected.
                failLoudHits.add(ns + ": legacy fallback answer=" + legacyPkg);
            }
        }

        System.out.println("Agreement: " + agree + "/" + allNamespaces.size());
        System.out.println("Expected divergences seen: " + expectedDivergencesSeen.size() + "/"
            + EXPECTED_DIVERGENCES.size());
        expectedDivergencesSeen.forEach(ns -> System.out.println("  " + ns + " -> " + EXPECTED_DIVERGENCES.get(ns)
            + " (expected)"));

        Set<String> missingExpected = new TreeSet<>(EXPECTED_DIVERGENCES.keySet());
        missingExpected.removeAll(expectedDivergencesSeen);
        if (!missingExpected.isEmpty()) {
            System.out.println(missingExpected.size() + " previously-known divergences NOT reproduced "
                + "this run (legacy table may have been fixed, or namespace removed from manifests -- "
                + "update EXPECTED_DIVERGENCES if so):");
            missingExpected.forEach(ns -> System.out.println("  " + ns));
        }

        if (!unexpectedDisagreements.isEmpty()) {
            System.out.println(unexpectedDisagreements.size() + " UNEXPECTED DISAGREEMENTS (new "
                + "divergence, not in EXPECTED_DIVERGENCES -- investigate before adding to the known set):");
            unexpectedDisagreements.forEach(d -> System.out.println("  " + d));
        }
        if (!failLoudHits.isEmpty()) {
            System.out.println(failLoudHits.size()
                + " namespaces where the generated module fails loud on a DECLARED namespace "
                + "-- always a bug, never expected:");
            failLoudHits.forEach(h -> System.out.println("  " + h));
        }
        if (unexpectedDisagreements.isEmpty() && failLoudHits.isEmpty() && missingExpected.isEmpty()) {
            System.out.println("All divergences from the legacy table are accounted for by the known set; "
                + "no unexpected disagreements, no fail-loud hits on declared namespaces.");
        }

        assertTrue(unexpectedDisagreements.isEmpty() && failLoudHits.isEmpty() && missingExpected.isEmpty(),
            "Generated routing has " + unexpectedDisagreements.size() + " unexpected disagreements, "
                + failLoudHits.size() + " fail-loud hits on declared namespaces, and "
                + missingExpected.size() + " previously-known divergences not reproduced"
                + " -- see stdout for details.");
    }
}
