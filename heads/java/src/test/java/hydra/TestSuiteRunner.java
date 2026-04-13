package hydra;

import hydra.core.*;
import hydra.graph.Graph;
import hydra.graph.Primitive;
import hydra.test.TestSuite;
import hydra.test.TestGraph;
import hydra.testing.*;
import hydra.lib.Libraries;
import hydra.tools.PrimitiveFunction;
import hydra.util.ConsList;
import hydra.util.Maybe;

import org.junit.jupiter.api.DynamicContainer;
import org.junit.jupiter.api.DynamicNode;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;

import java.io.FileWriter;
import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

import static hydra.dsl.Terms.*;
import static org.junit.jupiter.api.Assertions.*;


/**
 * Java executor for Hydra's language-agnostic test suite.
 *
 * All test cases are now UniversalTestCase instances (string comparison).
 * Legacy per-type handlers have been removed.
 */
public class TestSuiteRunner {

    // Benchmark output support
    private static final String BENCHMARK_OUTPUT = System.getenv("HYDRA_BENCHMARK_OUTPUT");
    private static final Map<String, Long> benchmarkTimers = new ConcurrentHashMap<>();
    private static final Map<String, Double> benchmarkResults = new ConcurrentHashMap<>();
    private static TestGroup rootTestGroup;

    // Cached test infrastructure
    private static Graph testGraph;

    private static synchronized Graph getTestGraph() {
        if (testGraph == null) {
            testGraph = buildTestGraph();
        }
        return testGraph;
    }

    private static hydra.context.Context emptyContext() {
        return new hydra.context.Context(
            hydra.util.ConsList.empty(),
            hydra.util.ConsList.empty(),
            hydra.util.PersistentMap.empty());
    }

    /**
     * Build the test graph with schema, test data, and primitives.
     * Mirrors the Haskell testGraph in TestUtils.hs.
     *
     * This is public because TestEnv.java delegates to it.
     */
    public static Graph buildTestGraph() {
        // Build primitives map
        hydra.util.PersistentMap<Name, Primitive> primitives = hydra.util.PersistentMap.empty();
        for (PrimitiveFunction prim : Libraries.standardPrimitives()) {
            primitives = primitives.insert(prim.name(), prim.toNative());
        }

        // Build schema types from test types + kernel types
        Map<Name, Type> testTypes = TestGraph.testTypes();
        Map<Name, Type> kernelTypes = buildKernelTypes();
        Map<Name, Type> allTypes = new HashMap<>(kernelTypes);
        allTypes.putAll(testTypes); // test types override kernel types if any overlap
        hydra.util.PersistentMap<Name, TypeScheme> schemaTypes = hydra.util.PersistentMap.empty();
        for (Map.Entry<Name, Type> entry : allTypes.entrySet()) {
            schemaTypes = schemaTypes.insert(entry.getKey(), hydra.Resolution.typeToTypeScheme(entry.getValue()));
        }

        // Build bound terms map from test terms + primitive bridges + kernel constants
        Map<Name, Term> boundTerms = new HashMap<>();

        // Primitives are resolved via graphPrimitives, not boundTerms.
        // No need to bridge them as term bindings.

        // Add non-primitive kernel constants needed by annotation source module
        boundTerms.put(new Name("hydra.constants.key_classes"),
            new Term.Wrap(new hydra.core.WrappedTerm(new Name("hydra.core.Name"),
                new Term.Literal(new hydra.core.Literal.String_("classes")))));
        boundTerms.put(new Name("hydra.constants.key_description"),
            new Term.Wrap(new hydra.core.WrappedTerm(new Name("hydra.core.Name"),
                new Term.Literal(new hydra.core.Literal.String_("description")))));
        boundTerms.put(new Name("hydra.constants.key_type"),
            new Term.Wrap(new hydra.core.WrappedTerm(new Name("hydra.core.Name"),
                new Term.Literal(new hydra.core.Literal.String_("type")))));
        boundTerms.put(new Name("hydra.constants.key_debugId"),
            new Term.Wrap(new hydra.core.WrappedTerm(new Name("hydra.core.Name"),
                new Term.Literal(new hydra.core.Literal.String_("debugId")))));
        boundTerms.put(new Name("hydra.constants.key_firstClassType"),
            new Term.Wrap(new hydra.core.WrappedTerm(new Name("hydra.core.Name"),
                new Term.Literal(new hydra.core.Literal.String_("firstClassType")))));

        // Add kernel constant term bindings (hand-written since generated sources exceed JVM method size limits)
        List<Binding> constantBindings = new ArrayList<>();
        addConstantBindings(constantBindings);
        for (Binding b : constantBindings) {
            boundTerms.put(b.name, b.term);
        }

        // Add kernel annotation/rewriting term bindings
        List<Binding> annotationBindings = new ArrayList<>();
        addAnnotationsBindings(annotationBindings);
        for (Binding b : annotationBindings) {
            boundTerms.put(b.name, b.term);
        }

        // Add test term bindings
        Map<Name, Term> testTerms = TestGraph.testTerms();
        boundTerms.putAll(testTerms);

        // Add type element terms to boundTerms (encoded types)
        for (Map.Entry<Name, Type> entry : allTypes.entrySet()) {
            boundTerms.put(entry.getKey(), hydra.encode.Core.type(entry.getValue()));
        }

        hydra.util.PersistentMap<Name, Term> persistentBoundTerms = hydra.util.PersistentMap.empty();
        for (Map.Entry<Name, Term> entry : boundTerms.entrySet()) {
            persistentBoundTerms = persistentBoundTerms.insert(entry.getKey(), entry.getValue());
        }

        return new Graph(
            persistentBoundTerms,
            hydra.util.PersistentMap.empty(), // boundTypes (TypeSchemes for term bindings — not populated for test graph)
            hydra.util.PersistentMap.empty(), // classConstraints
            hydra.util.PersistentSet.empty(), // lambdaVariables
            hydra.util.PersistentMap.empty(), // metadata
            primitives,
            schemaTypes,
            hydra.util.PersistentSet.empty()  // typeVariables
        );
    }

    private static void addConstantBinding(List<Binding> bindings, String name, Term value) {
        bindings.add(new Binding(new Name(name), value, Maybe.nothing()));
    }

    /**
     * Add term-level bindings for graph constants needed by tests.
     * These are hand-written because the generated source modules exceed JVM method size limits.
     */
    private static void addConstantBindings(List<Binding> bindings) {
        addConstantBinding(bindings, "hydra.lexical.emptyGraph",
            record("hydra.graph.Graph",
                field("boundTerms", new Term.Map(hydra.util.PersistentMap.empty())),
                field("boundTypes", new Term.Map(hydra.util.PersistentMap.empty())),
                field("classConstraints", new Term.Map(hydra.util.PersistentMap.empty())),
                field("lambdaVariables", new Term.Set(hydra.util.PersistentSet.empty())),
                field("metadata", new Term.Map(hydra.util.PersistentMap.empty())),
                field("primitives", new Term.Map(hydra.util.PersistentMap.empty())),
                field("schemaTypes", new Term.Map(hydra.util.PersistentMap.empty())),
                field("typeVariables", new Term.Set(hydra.util.PersistentSet.empty()))));
    }

    /**
     * Add term-level bindings for annotation and rewriting functions needed by tests.
     * These are hand-written because the generated source modules exceed JVM method size limits.
     */
    private static void addAnnotationsBindings(List<Binding> bindings) {
        addConstantBinding(bindings, "hydra.rewriting.deannotateTerm",
            lambda("t",
                apply(
                    match("hydra.core.Term", Maybe.just(var("t")),
                        field("annotated", lambda("at",
                            apply(var("hydra.rewriting.deannotateTerm"),
                                apply(project("hydra.core.AnnotatedTerm", "body"), var("at")))))),
                    var("t"))));

        addConstantBinding(bindings, "hydra.annotations.termAnnotationInternal",
            lambda("term",
                let_("toPairs",
                    lambda("rest", "t",
                        apply(
                            match("hydra.core.Term",
                                Maybe.just(var("rest")),
                                field("annotated", lambda("at",
                                    apply(apply(var("toPairs"),
                                        apply(apply(primitive("hydra.lib.lists.cons"),
                                            apply(primitive("hydra.lib.maps.toList"),
                                                apply(project("hydra.core.AnnotatedTerm", "annotation"), var("at")))),
                                            var("rest"))),
                                        apply(project("hydra.core.AnnotatedTerm", "body"), var("at")))))),
                            var("t"))),
                    apply(primitive("hydra.lib.maps.fromList"),
                        apply(primitive("hydra.lib.lists.concat"),
                            apply(apply(var("toPairs"), list()), var("term")))))));

        addConstantBinding(bindings, "hydra.annotations.setAnnotation",
            lambda("key",
                lambda("val",
                    lambda("m",
                        apply(apply(apply(primitive("hydra.lib.maybes.maybe"),
                            apply(apply(primitive("hydra.lib.maps.delete"), var("key")), var("m"))),
                            lambda("v",
                                apply(apply(apply(primitive("hydra.lib.maps.insert"),
                                    var("key")), var("v")), var("m")))),
                            var("val"))))));

        addConstantBinding(bindings, "hydra.annotations.setTermAnnotation",
            lambda("key",
                lambda("val",
                    lambda("term",
                        let_("stripped", apply(var("hydra.rewriting.deannotateTerm"), var("term")),
                            let_("anns",
                                apply(apply(apply(var("hydra.annotations.setAnnotation"), var("key")), var("val")),
                                    apply(var("hydra.annotations.termAnnotationInternal"), var("term"))),
                                apply(apply(apply(primitive("hydra.lib.logic.ifElse"),
                                    apply(primitive("hydra.lib.maps.null"), var("anns"))),
                                    var("stripped")),
                                    inject("hydra.core.Term", "annotated",
                                        record("hydra.core.AnnotatedTerm",
                                            field("body", var("stripped")),
                                            field("annotation", var("anns")))))))))));

        addConstantBinding(bindings, "hydra.annotations.setTermDescription",
            lambda("d",
                apply(apply(var("hydra.annotations.setTermAnnotation"),
                    var("hydra.constants.key_description")),
                    apply(apply(primitive("hydra.lib.maybes.map"),
                        lambda("s",
                            inject("hydra.core.Term", "literal",
                                inject("hydra.core.Literal", "string", var("s"))))),
                        var("d")))));

        addConstantBinding(bindings, "hydra.annotations.getDescription",
            lambda("cx",
                lambda("g",
                    lambda("anns",
                        apply(apply(apply(primitive("hydra.lib.maybes.maybe"),
                            right(nothing())),
                            lambda("descTerm",
                                apply(
                                    match("hydra.core.Term", Maybe.just(
                                        left(record("hydra.context.InContext",
                                            field("object", inject("hydra.errors.Error", field("other", wrap("hydra.errors.OtherError", string("Expected string literal"))))),
                                            field("context", var("cx"))))),
                                        field("literal", lambda("lit",
                                            apply(
                                                match("hydra.core.Literal", Maybe.just(
                                                    left(record("hydra.context.InContext",
                                                        field("object", inject("hydra.errors.Error", field("other", wrap("hydra.errors.OtherError", string("Expected string literal"))))),
                                                        field("context", var("cx"))))),
                                                    field("string", lambda("s", right(just(var("s")))))),
                                                var("lit"))))),
                                    var("descTerm")))),
                            apply(apply(primitive("hydra.lib.maps.lookup"),
                                var("hydra.constants.key_description")),
                                var("anns")))))));

        addConstantBinding(bindings, "hydra.annotations.getTermDescription",
            lambda("cx",
                lambda("g",
                    lambda("term",
                        let_("peel",
                            lambda("t",
                                apply(
                                    match("hydra.core.Term", Maybe.just(var("t")),
                                        field("typeLambda", lambda("tl",
                                            apply(var("peel"),
                                                apply(project("hydra.core.TypeLambda", "body"), var("tl"))))),
                                        field("typeApplication", lambda("ta",
                                            apply(var("peel"),
                                                apply(project("hydra.core.TypeApplicationTerm", "body"), var("ta")))))),
                                    var("t"))),
                            apply(apply(apply(var("hydra.annotations.getDescription"), var("cx")), var("g")),
                                apply(var("hydra.annotations.termAnnotationInternal"),
                                    apply(var("peel"), var("term")))))))));

        addConstantBinding(bindings, "hydra.annotations.getTermAnnotation",
            lambda("key",
                lambda("term",
                    apply(apply(primitive("hydra.lib.maps.lookup"), var("key")),
                        apply(var("hydra.annotations.termAnnotationInternal"), var("term"))))));
    }

    /**
     * Build kernel type definitions needed by inference/checking tests.
     * These types are normally provided by kernelTypesModules in Haskell.
     */
    private static Map<Name, Type> buildKernelTypes() {
        Map<Name, Type> types = new HashMap<>();

        // CoderDirection: enum with encode, decode
        types.put(new Name("hydra.coders.CoderDirection"),
            new Type.Union(ConsList.of(
                new FieldType(new Name("encode"), new Type.Unit()),
                new FieldType(new Name("decode"), new Type.Unit()))));

        Name contextName = new Name("hydra.context.Context");
        Name inContextName = new Name("hydra.context.InContext");
        Name errorName = new Name("hydra.errors.Error");
        Type inContextError = new Type.Application(new ApplicationType(
            new Type.Variable(inContextName),
            new Type.Variable(errorName)));
        java.util.function.Function<Type, Type> eitherInContextError = v ->
            new Type.Either(new EitherType(inContextError, v));

        // Coder: forall v1 v2. {encode: ..., decode: ...}
        Type encodeType = new Type.Function(new FunctionType(
            new Type.Variable(contextName),
            new Type.Function(new FunctionType(
                new Type.Variable(new Name("v1")),
                eitherInContextError.apply(new Type.Variable(new Name("v2")))))));
        Type decodeType = new Type.Function(new FunctionType(
            new Type.Variable(contextName),
            new Type.Function(new FunctionType(
                new Type.Variable(new Name("v2")),
                eitherInContextError.apply(new Type.Variable(new Name("v1")))))));
        Type coderBody = new Type.Record(ConsList.of(
            new FieldType(new Name("encode"), encodeType),
            new FieldType(new Name("decode"), decodeType)));
        types.put(new Name("hydra.coders.Coder"),
            new Type.Forall(new ForallType(new Name("v1"),
                new Type.Forall(new ForallType(new Name("v2"), coderBody)))));

        // Context
        types.put(contextName,
            new Type.Record(ConsList.of(
                new FieldType(new Name("trace"), new Type.List(new Type.Literal(new LiteralType.String_()))),
                new FieldType(new Name("messages"), new Type.List(new Type.Literal(new LiteralType.String_()))),
                new FieldType(new Name("other"), new Type.Map(new MapType(
                    new Type.Variable(new Name("hydra.core.Name")),
                    new Type.Variable(new Name("hydra.core.Term"))))))));

        // InContext
        types.put(inContextName,
            new Type.Forall(new ForallType(new Name("e"),
                new Type.Record(ConsList.of(
                    new FieldType(new Name("object"), new Type.Variable(new Name("e"))),
                    new FieldType(new Name("context"), new Type.Variable(contextName)))))));

        // Error types
        Name otherErrorName = new Name("hydra.errors.OtherError");
        types.put(otherErrorName,
            new Type.Wrap(new Type.Literal(new LiteralType.String_())));
        types.put(errorName,
            new Type.Union(ConsList.of(
                new FieldType(new Name("other"), new Type.Variable(otherErrorName)))));

        // Type (hydra.core.Type)
        Name typeName = new Name("hydra.core.Type");
        types.put(typeName,
            new Type.Union(ConsList.of(
                new FieldType(new Name("annotated"), new Type.Variable(new Name("annotatedType"))),
                new FieldType(new Name("application"), new Type.Variable(new Name("applicationElim"))),
                new FieldType(new Name("either"), new Type.Variable(new Name("eitherType"))),
                new FieldType(new Name("forall"), new Type.Variable(new Name("forallType"))),
                new FieldType(new Name("function"), new Type.Variable(new Name("functionType"))),
                new FieldType(new Name("list"), new Type.Variable(typeName)),
                new FieldType(new Name("literal"), new Type.Variable(new Name("literalType"))),
                new FieldType(new Name("map"), new Type.Variable(new Name("mapType"))),
                new FieldType(new Name("maybe"), new Type.Variable(typeName)),
                new FieldType(new Name("pair"), new Type.Variable(new Name("pairType"))),
                new FieldType(new Name("record"), new Type.Variable(new Name("rowType"))),
                new FieldType(new Name("set"), new Type.Variable(typeName)),
                new FieldType(new Name("union"), new Type.Variable(new Name("rowType"))),
                new FieldType(new Name("unit"), new Type.Unit()),
                new FieldType(new Name("variable"), new Type.Variable(new Name("name"))),
                new FieldType(new Name("wrap"), new Type.Variable(new Name("wrappedType"))))));

        // Name
        types.put(new Name("hydra.core.Name"),
            new Type.Wrap(new Type.Literal(new LiteralType.String_())));

        // ForallType
        types.put(new Name("hydra.core.ForallType"),
            new Type.Record(ConsList.of(
                new FieldType(new Name("parameter"), new Type.Variable(new Name("hydra.core.Name"))),
                new FieldType(new Name("body"), new Type.Variable(typeName)))));

        // Comparison
        types.put(new Name("hydra.util.Comparison"),
            new Type.Union(ConsList.of(
                new FieldType(new Name("lessThan"), new Type.Unit()),
                new FieldType(new Name("equalTo"), new Type.Unit()),
                new FieldType(new Name("greaterThan"), new Type.Unit()))));

        // CaseConvention
        types.put(new Name("hydra.util.CaseConvention"),
            new Type.Union(ConsList.of(
                new FieldType(new Name("camel"), new Type.Unit()),
                new FieldType(new Name("pascal"), new Type.Unit()),
                new FieldType(new Name("lowerSnake"), new Type.Unit()),
                new FieldType(new Name("upperSnake"), new Type.Unit()))));

        // Precision
        types.put(new Name("hydra.util.Precision"),
            new Type.Union(ConsList.of(
                new FieldType(new Name("arbitrary"), new Type.Unit()),
                new FieldType(new Name("bits"), new Type.Literal(new LiteralType.Integer_(new IntegerType.Int32()))))));

        return types;
    }

    private static boolean shouldSkip(TestCaseWithMetadata tc) {
        Tag disabledTag = new Tag("disabled");
        Tag disabledForPythonTag = new Tag("disabledForPython");
        // Note: disabledForPython tests are also skipped in Java because the same beta-reduction
        // term explosion occurs (e.g. deeply nested withTrace/mutateTrace). The Haskell evaluator
        // handles these efficiently via lazy evaluation, but Java's eager reducer cannot.
        return tc.tags.contains(disabledTag)
            || tc.tags.contains(disabledForPythonTag);
    }

    @TestFactory
    Stream<DynamicNode> kernelTests() {
        TestGroup allTests = TestSuite.allTests();
        rootTestGroup = allTests;

        // Eagerly initialize test infrastructure and measure time.
        // This ensures startup cost is not attributed to the first test group.
        long initStart = System.nanoTime();
        getTestGraph();
        double initMs = (System.nanoTime() - initStart) / 1_000_000.0;
        if (BENCHMARK_OUTPUT != null) {
            benchmarkResults.put(allTests.name + "/_initialization", initMs);
            Runtime.getRuntime().addShutdownHook(new Thread(() -> writeBenchmarkJson(BENCHMARK_OUTPUT, allTests)));
        }

        return collectTests(allTests, allTests.name);
    }

    private static Stream<DynamicNode> collectTests(TestGroup group, String hydraPath) {
        List<DynamicNode> nodes = new ArrayList<>();

        // Timer start sentinel
        if (BENCHMARK_OUTPUT != null) {
            final String path = hydraPath;
            nodes.add(DynamicTest.dynamicTest("000_TIMER_START", () -> {
                benchmarkTimers.put(path, System.nanoTime());
            }));
        }

        // Test cases
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

        // Subgroups
        for (TestGroup subgroup : group.subgroups) {
            String subName = subgroup.name + subgroup.description.map(d -> " (" + d + ")").orElse("");
            String subPath = hydraPath + "/" + subgroup.name;
            nodes.add(DynamicContainer.dynamicContainer(subName, collectTests(subgroup, subPath)));
        }

        // Timer stop sentinel
        if (BENCHMARK_OUTPUT != null) {
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

    private static final Duration TEST_TIMEOUT = Duration.ofSeconds(10);

    private static DynamicTest runTestCase(String name, TestCaseWithMetadata tc) {
        return tc.case_.accept(new TestCase.Visitor<>() {
            @Override
            public DynamicTest visit(TestCase.Universal instance) {
                UniversalTestCase utc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(utc.expected, utc.actual));
            }
        });
    }

    private static DynamicTest withTimeout(String name, org.junit.jupiter.api.function.Executable executable) {
        return DynamicTest.dynamicTest(name, () -> assertTimeoutPreemptively(TEST_TIMEOUT, executable));
    }

    // ---- Benchmark output ----

    private static void writeBenchmarkJson(String outputPath, TestGroup root) {
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

    private static String buildBenchmarkJson(TestGroup root) {
        StringBuilder sb = new StringBuilder();
        sb.append("{\n");

        // Metadata
        sb.append("  \"metadata\": {\n");
        sb.append("    \"timestamp\": \"").append(Instant.now().atOffset(ZoneOffset.UTC)
            .format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'"))).append("\",\n");
        sb.append("    \"language\": \"java\",\n");
        sb.append("    \"branch\": ").append(jsonString(gitOutput("git", "rev-parse", "--abbrev-ref", "HEAD"))).append(",\n");
        sb.append("    \"commit\": ").append(jsonString(gitOutput("git", "rev-parse", "--short", "HEAD"))).append(",\n");
        sb.append("    \"commitMessage\": ").append(jsonString(gitOutput("git", "log", "-1", "--format=%s"))).append("\n");
        sb.append("  },\n");

        // Groups (children of root)
        String rootPath = root.name;
        sb.append("  \"groups\": [\n");
        List<TestGroup> subgroups = root.subgroups;
        int totalPassed = 0, totalFailed = 0, totalSkipped = 0;
        double totalTimeMs = 0;

        // Add initialization group if present
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

            // Subgroups
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

        // Summary
        sb.append("  \"summary\": {\n");
        sb.append("    \"totalPassed\": ").append(totalPassed).append(",\n");
        sb.append("    \"totalFailed\": ").append(totalFailed).append(",\n");
        sb.append("    \"totalSkipped\": ").append(totalSkipped).append(",\n");
        sb.append("    \"totalTimeMs\": ").append(round1(totalTimeMs)).append("\n");
        sb.append("  }\n");

        sb.append("}\n");
        return sb.toString();
    }

    /**
     * Count [passed, failed, skipped] tests in a group (recursive).
     * "passed" = runnable (not skipped) tests; "failed" = 0 (we can't know at generation time).
     */
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
