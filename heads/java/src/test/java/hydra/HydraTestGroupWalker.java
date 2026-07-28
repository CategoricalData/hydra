package hydra;

import hydra.testing.*;

import org.junit.jupiter.api.DynamicContainer;
import org.junit.jupiter.api.DynamicNode;
import org.junit.jupiter.api.DynamicTest;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;

/**
 * Generic JUnit walker over a Hydra {@link TestGroup} tree, shared by every per-package
 * test-suite runner (hydra-kernel's TestSuiteRunner, hydra-build's BuildTestSuiteRunner,
 * and any future package's runner — #547). Handles universal and effectful test cases,
 * skip tags, timeouts, and optional benchmark JSON output. Does not build any test graph
 * or primitive environment — that is package-specific and stays in each subclass/caller.
 */
public final class HydraTestGroupWalker {

    private final String benchmarkOutput;
    private final String languageTag;
    private final Map<String, Long> benchmarkTimers = new ConcurrentHashMap<>();
    private final Map<String, Double> benchmarkResults = new ConcurrentHashMap<>();
    private static final Duration TEST_TIMEOUT = Duration.ofSeconds(10);

    // Canonical root directory for effectful (file I/O) test cases. Must match the testDir
    // constant in Hydra.Sources.Test.Lib.Files and the effectfulTestDir in the Haskell
    // runner. Hard-coded *nix path for now (configurable later, #494).
    private static final String EFFECTFUL_TEST_DIR = "/tmp/hydra-testing";

    public HydraTestGroupWalker(String languageTag, String benchmarkOutput) {
        this.languageTag = languageTag;
        this.benchmarkOutput = benchmarkOutput;
    }

    /** Walk the given root group, registering a benchmark shutdown hook if configured. */
    public Stream<DynamicNode> walk(TestGroup root, double initMs) {
        if (benchmarkOutput != null) {
            benchmarkResults.put(root.name + "/_initialization", initMs);
            Runtime.getRuntime().addShutdownHook(new Thread(() -> writeBenchmarkJson(benchmarkOutput, root)));
        }
        return collectTests(root, root.name);
    }

    private static boolean shouldSkip(TestCaseWithMetadata tc) {
        Tag disabledTag = new Tag("disabled");
        Tag disabledForPythonTag = new Tag("disabledForPython");
        return tc.tags.contains(disabledTag) || tc.tags.contains(disabledForPythonTag);
    }

    private Stream<DynamicNode> collectTests(TestGroup group, String hydraPath) {
        List<DynamicNode> nodes = new ArrayList<>();

        if (benchmarkOutput != null) {
            final String path = hydraPath;
            nodes.add(DynamicTest.dynamicTest("000_TIMER_START", () -> benchmarkTimers.put(path, System.nanoTime())));
        }

        for (TestCaseWithMetadata tc : group.cases) {
            String name = tc.name + tc.description.map(d -> ": " + d).orElse("");
            if (shouldSkip(tc)) {
                continue;
            }
            DynamicTest test = runTestCase(name, tc);
            if (test != null) {
                nodes.add(test);
            }
        }

        for (TestGroup subgroup : group.subgroups) {
            String subName = subgroup.name + subgroup.description.map(d -> " (" + d + ")").orElse("");
            String subPath = hydraPath + "/" + subgroup.name;
            nodes.add(DynamicContainer.dynamicContainer(subName, collectTests(subgroup, subPath)));
        }

        if (benchmarkOutput != null) {
            final String path = hydraPath;
            nodes.add(DynamicTest.dynamicTest("999_TIMER_END", () -> {
                Long startTime = benchmarkTimers.get(path);
                if (startTime != null) {
                    double elapsedMs = (System.nanoTime() - startTime) / 1_000_000.0;
                    benchmarkResults.put(path, elapsedMs);
                }
            }));
        }

        return nodes.stream();
    }

    private DynamicTest runTestCase(String name, TestCaseWithMetadata tc) {
        return tc.case_.accept(new TestCase.Visitor<>() {
            @Override
            public DynamicTest visit(TestCase.Universal instance) {
                UniversalTestCase utc = instance.value;
                return withTimeout(name, () -> assertEquals(utc.expected.apply(null), utc.actual.apply(null)));
            }

            @Override
            public DynamicTest visit(TestCase.Effectful instance) {
                EffectfulTestCase eutc = instance.value;
                return withTimeout(name, () -> {
                    prepareEffectfulTempDir();
                    assertEquals(eutc.expected.apply(null), eutc.actual.apply(null));
                });
            }
        });
    }

    // Prepare a guaranteed-empty canonical temp directory before an effectful test case.
    private static void prepareEffectfulTempDir() throws IOException {
        Path dir = Paths.get(EFFECTFUL_TEST_DIR);
        if (Files.exists(dir)) {
            try (Stream<Path> walk = Files.walk(dir)) {
                walk.sorted(java.util.Comparator.reverseOrder())
                    .forEach(p -> {
                        try {
                            Files.delete(p);
                        } catch (IOException e) {
                            throw new RuntimeException("Failed to delete " + p, e);
                        }
                    });
            }
        }
        Files.createDirectories(dir);
    }

    private static DynamicTest withTimeout(String name, org.junit.jupiter.api.function.Executable executable) {
        return DynamicTest.dynamicTest(name, () -> assertTimeoutPreemptively(TEST_TIMEOUT, executable));
    }

    // ---- Benchmark output ----

    private void writeBenchmarkJson(String outputPath, TestGroup root) {
        try {
            String json = buildBenchmarkJson(root);
            try (FileWriter writer = new FileWriter(outputPath)) {
                writer.write(json);
            }
            System.out.println("Benchmark results written to " + outputPath);
        } catch (IOException e) {
            System.err.println("Failed to write benchmark JSON: " + e.getMessage());
        }
    }

    private String buildBenchmarkJson(TestGroup root) {
        StringBuilder sb = new StringBuilder();
        sb.append("{\n");

        sb.append("  \"metadata\": {\n");
        sb.append("    \"timestamp\": \"").append(Instant.now().atOffset(ZoneOffset.UTC)
            .format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'"))).append("\",\n");
        sb.append("    \"language\": \"").append(languageTag).append("\",\n");
        sb.append("    \"branch\": ").append(jsonString(gitOutput("git", "rev-parse", "--abbrev-ref", "HEAD"))).append(",\n");
        sb.append("    \"commit\": ").append(jsonString(gitOutput("git", "rev-parse", "--short", "HEAD"))).append(",\n");
        sb.append("    \"commitMessage\": ").append(jsonString(gitOutput("git", "log", "-1", "--format=%s"))).append("\n");
        sb.append("  },\n");

        String rootPath = root.name;
        sb.append("  \"groups\": [\n");
        List<TestGroup> subgroups = root.subgroups;
        int totalPassed = 0, totalFailed = 0, totalSkipped = 0;
        double totalTimeMs = 0;

        double initTime = benchmarkResults.getOrDefault(rootPath + "/_initialization", 0.0);
        if (initTime > 0) {
            totalTimeMs += initTime;
            sb.append("    {\n");
            sb.append("      \"failed\": 0,\n");
            sb.append("      \"passed\": 0,\n");
            sb.append("      \"path\": ").append(jsonString(rootPath + "/_initialization")).append(",\n");
            sb.append("      \"skipped\": 0,\n");
            sb.append("      \"totalTimeMs\": ").append(round1(initTime)).append("}");
            if (!subgroups.isEmpty()) sb.append(",");
            sb.append("\n");
        }

        for (int i = 0; i < subgroups.size(); i++) {
            TestGroup group = subgroups.get(i);
            String groupPath = rootPath + "/" + group.name;
            int[] counts = countTests(group);
            double groupTime = benchmarkResults.getOrDefault(groupPath, 0.0);
            totalPassed += counts[0];
            totalFailed += counts[1];
            totalSkipped += counts[2];
            totalTimeMs += groupTime;

            sb.append("    {\n");
            sb.append("      \"failed\": ").append(counts[1]).append(",\n");
            sb.append("      \"passed\": ").append(counts[0]).append(",\n");
            sb.append("      \"path\": ").append(jsonString(groupPath)).append(",\n");
            sb.append("      \"skipped\": ").append(counts[2]).append(",\n");

            if (!group.subgroups.isEmpty()) {
                sb.append("      \"subgroups\": [\n");
                for (int j = 0; j < group.subgroups.size(); j++) {
                    TestGroup sub = group.subgroups.get(j);
                    String subPath = groupPath + "/" + sub.name;
                    int[] subCounts = countTests(sub);
                    double subTime = benchmarkResults.getOrDefault(subPath, 0.0);

                    sb.append("        {\n");
                    sb.append("          \"failed\": ").append(subCounts[1]).append(",\n");
                    sb.append("          \"passed\": ").append(subCounts[0]).append(",\n");
                    sb.append("          \"path\": ").append(jsonString(subPath)).append(",\n");
                    sb.append("          \"skipped\": ").append(subCounts[2]).append(",\n");
                    sb.append("          \"totalTimeMs\": ").append(round1(subTime)).append("}");
                    if (j < group.subgroups.size() - 1) sb.append(",");
                    sb.append("\n");
                }
                sb.append("      ],\n");
            }

            sb.append("      \"totalTimeMs\": ").append(round1(groupTime)).append("}");
            if (i < subgroups.size() - 1) sb.append(",");
            sb.append("\n");
        }
        sb.append("  ],\n");

        sb.append("  \"summary\": {\n");
        sb.append("    \"totalPassed\": ").append(totalPassed).append(",\n");
        sb.append("    \"totalFailed\": ").append(totalFailed).append(",\n");
        sb.append("    \"totalSkipped\": ").append(totalSkipped).append(",\n");
        sb.append("    \"totalTimeMs\": ").append(round1(totalTimeMs)).append("\n");
        sb.append("  }\n");

        sb.append("}\n");
        return sb.toString();
    }

    /** Count [passed, failed, skipped] tests in a group (recursive). */
    private static int[] countTests(TestGroup group) {
        int runnable = 0;
        int skipped = 0;
        for (TestCaseWithMetadata tc : group.cases) {
            if (shouldSkip(tc)) {
                skipped++;
            } else {
                runnable++;
            }
        }
        for (TestGroup sub : group.subgroups) {
            int[] subCounts = countTests(sub);
            runnable += subCounts[0];
            skipped += subCounts[2];
        }
        return new int[]{runnable, 0, skipped};
    }

    private static String round1(double value) {
        return String.format("%.1f", value);
    }

    private static String jsonString(String value) {
        if (value == null) return "\"\"";
        return "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "").trim() + "\"";
    }

    private static String gitOutput(String... command) {
        try {
            ProcessBuilder pb = new ProcessBuilder(command);
            pb.redirectErrorStream(true);
            Process p = pb.start();
            String output = new String(p.getInputStream().readAllBytes()).trim();
            p.waitFor();
            return output;
        } catch (Exception e) {
            return "unknown";
        }
    }
}
