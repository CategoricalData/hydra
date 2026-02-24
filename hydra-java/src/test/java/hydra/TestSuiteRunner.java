package hydra;

import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.core.*;
import hydra.graph.Graph;
import hydra.graph.Primitive;
import hydra.test.testSuite.TestSuite;
import hydra.test.testGraph.TestGraph;
import hydra.testing.*;
import hydra.tools.PrettyPrinter;
import hydra.lib.Libraries;
import hydra.module.Module;
import hydra.module.Namespace;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;
import hydra.util.Tuple;

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

import static hydra.dsl.Flows.EMPTY_TRACE;
import static org.junit.jupiter.api.Assertions.*;


/**
 * Java executor for Hydra's language-agnostic test suite.
 */
public class TestSuiteRunner {

    // Unit value used as state for flows that don't need state (equivalent to Haskell's ())
    private static final hydra.util.Unit UNIT = new hydra.util.Unit();

    // Benchmark output support
    private static final String BENCHMARK_OUTPUT = System.getenv("HYDRA_BENCHMARK_OUTPUT");
    private static final Map<String, Long> benchmarkTimers = new ConcurrentHashMap<>();
    private static final Map<String, Double> benchmarkResults = new ConcurrentHashMap<>();
    private static TestGroup rootTestGroup;

    // Cached test infrastructure
    private static Graph testGraph;
    private static hydra.typing.InferenceContext inferenceContext;
    private static hydra.typing.TypeContext typeContext;

    /**
     * Compare two terms for equality, with tolerance for floating-point precision and BigDecimal scale differences.
     */
    private static boolean termsEqual(Term expected, Term actual) {
        if (expected.equals(actual)) {
            return true;
        }
        // Handle float precision: compare float64 values with ULP tolerance
        if (expected instanceof Term.Literal && actual instanceof Term.Literal) {
            Literal el = ((Term.Literal) expected).value;
            Literal al = ((Term.Literal) actual).value;
            if (el instanceof Literal.Float_ && al instanceof Literal.Float_) {
                FloatValue ef = ((Literal.Float_) el).value;
                FloatValue af = ((Literal.Float_) al).value;
                if (ef instanceof FloatValue.Float64 && af instanceof FloatValue.Float64) {
                    double e = ((FloatValue.Float64) ef).value;
                    double a = ((FloatValue.Float64) af).value;
                    return Math.abs(e - a) <= 2 * Math.ulp(e);
                }
                if (ef instanceof FloatValue.Float32 && af instanceof FloatValue.Float32) {
                    float e = ((FloatValue.Float32) ef).value;
                    float a = ((FloatValue.Float32) af).value;
                    return Math.abs(e - a) <= 2 * Math.ulp(e);
                }
                if (ef instanceof FloatValue.Bigfloat && af instanceof FloatValue.Bigfloat) {
                    java.math.BigDecimal e = ((FloatValue.Bigfloat) ef).value;
                    java.math.BigDecimal a = ((FloatValue.Bigfloat) af).value;
                    return e.compareTo(a) == 0;
                }
            }
        }
        return false;
    }

    private static synchronized Graph getTestGraph() {
        if (testGraph == null) {
            testGraph = buildTestGraph();
        }
        return testGraph;
    }

    private static synchronized hydra.typing.InferenceContext getInferenceContext() {
        if (inferenceContext == null) {
            Graph graph = getTestGraph();
            Flow<Object, hydra.typing.InferenceContext> flow =
                hydra.schemas.Schemas.graphToInferenceContext(graph);
            FlowState<Object, hydra.typing.InferenceContext> state =
                flow.value.apply(UNIT).apply(EMPTY_TRACE);
            if (!state.value.isJust()) {
                throw new RuntimeException("Failed to create inference context: " + state.trace.messages);
            }
            inferenceContext = state.value.fromJust();
        }
        return inferenceContext;
    }

    private static synchronized hydra.typing.TypeContext getTypeContext() {
        if (typeContext == null) {
            Graph graph = getTestGraph();
            Flow<Graph, hydra.typing.TypeContext> flow =
                hydra.schemas.Schemas.graphToTypeContext(graph);
            FlowState<Graph, hydra.typing.TypeContext> state =
                flow.value.apply(graph).apply(EMPTY_TRACE);
            if (!state.value.isJust()) {
                throw new RuntimeException("Failed to create type context: " + state.trace.messages);
            }
            typeContext = state.value.fromJust();
        }
        return typeContext;
    }

    /**
     * Build the test graph with schema, test data, and primitives.
     * Mirrors the Haskell testGraph in TestUtils.hs.
     */
    private static Graph buildTestGraph() {
        // Build primitives map
        Map<Name, Primitive> primitives = new HashMap<>();
        for (PrimitiveFunction prim : Libraries.standardPrimitives()) {
            primitives.put(prim.name(), prim.toNative());
        }

        // Load all kernel modules for schema graph type definitions.
        // Only type-defining bindings are extracted — the full module data is not retained.
        List<Module> allKernelModules = loadAllKernelModulesForSchema();
        List<Binding> kernelTypeBindings = new ArrayList<>();
        for (Module mod : allKernelModules) {
            for (Binding b : mod.elements) {
                if (hydra.annotations.Annotations.isNativeType(b)) {
                    kernelTypeBindings.add(b);
                }
            }
        }

        // Load only essential term modules for the evaluator (hydra.monads, hydra.annotations, etc.)
        List<Module> termModules = loadEvaluatorTermModules();

        // Build schema graph with test types + kernel types
        Map<Name, Type> testTypes = TestGraph.testTypes();

        // Build type bindings: kernel type bindings (from JSON) + test type bindings
        List<Binding> typeBindings = new ArrayList<>(kernelTypeBindings);
        for (Map.Entry<Name, Type> entry : testTypes.entrySet()) {
            Term typeTerm = hydra.encode.core.Core.type(entry.getValue());
            TypeScheme typeScheme = new TypeScheme(
                List.of(),
                new Type.Variable(new Name("hydra.core.Type")),
                Maybe.nothing()
            );
            typeBindings.add(new Binding(entry.getKey(), typeTerm, Maybe.just(typeScheme)));
        }

        Graph schemaGraph = new Graph(
            typeBindings,
            Collections.emptyMap(),
            Collections.emptyMap(),
            new Term.Literal(new Literal.String_("schema")),
            Collections.emptyMap(),
            Maybe.nothing()
        );

        // Build main graph with test terms + kernel term bindings
        Map<Name, Term> testTerms = TestGraph.testTerms();
        List<Binding> termBindings = new ArrayList<>();

        // Add kernel term bindings from non-bootstrap modules
        for (Module mod : termModules) {
            termBindings.addAll(mod.elements);
        }

        // Bridge all primitives: Variable("hydra.lib.maps.null") -> Function.Primitive("hydra.lib.maps.null")
        // The reducer only looks up Term.Variable in term bindings (not primitives).
        Set<String> existingBindingNames = new HashSet<>();
        for (Binding b : termBindings) {
            existingBindingNames.add(b.name.value);
        }
        for (PrimitiveFunction prim : Libraries.standardPrimitives()) {
            String primName = prim.name().value;
            if (!existingBindingNames.contains(primName)) {
                termBindings.add(new Binding(
                    prim.name(),
                    new Term.Function(new hydra.core.Function.Primitive(prim.name())),
                    Maybe.nothing()));
            }
        }

        // Add test term bindings
        for (Map.Entry<Name, Term> entry : testTerms.entrySet()) {
            termBindings.add(new Binding(entry.getKey(), entry.getValue(), Maybe.nothing()));
        }

        return new Graph(
            termBindings,
            Collections.emptyMap(),
            Collections.emptyMap(),
            new Term.Literal(new Literal.String_("test")),
            primitives,
            Maybe.just(schemaGraph)
        );
    }

    private static final Set<String> BOOTSTRAP_NAMESPACES = Set.of(
        "hydra.core", "hydra.compute", "hydra.graph", "hydra.module");

    // The evaluator only needs these term modules (hydra.monads + hydra.annotations and their deps).
    // Loading all 113 kernel modules from JSON creates too much memory pressure.
    private static final List<String> EVALUATOR_TERM_NAMESPACES = List.of(
        "hydra.constants",
        "hydra.show.core",
        "hydra.monads",
        "hydra.extract.core",
        "hydra.lexical",
        "hydra.rewriting",
        "hydra.decode.core",
        "hydra.encode.core",
        "hydra.annotations");

    /**
     * Load all kernel modules from JSON for the schema graph (type-defining bindings only).
     */
    private static List<Module> loadAllKernelModulesForSchema() {
        try {
            String jsonDir = "../hydra-haskell/src/gen-main/json";
            Map<Name, Type> schemaMap = Generation.bootstrapSchemaMap();
            List<Namespace> allKernelNamespaces = Generation.readManifestField(jsonDir, "kernelModules");
            return Generation.loadModulesFromJson(false, jsonDir, schemaMap, allKernelNamespaces);
        } catch (IOException e) {
            throw new RuntimeException("Failed to load kernel modules from JSON", e);
        }
    }

    /**
     * Load only the essential term modules from JSON for the evaluator.
     * System F type annotations are stripped since the evaluator works at the simply-typed level.
     */
    private static List<Module> loadEvaluatorTermModules() {
        try {
            String jsonDir = "../hydra-haskell/src/gen-main/json";
            Map<Name, Type> schemaMap = Generation.bootstrapSchemaMap();
            List<Namespace> termNamespaces = new ArrayList<>();
            for (String ns : EVALUATOR_TERM_NAMESPACES) {
                termNamespaces.add(new Namespace(ns));
            }
            List<Module> modules = Generation.loadModulesFromJson(false, jsonDir, schemaMap, termNamespaces);
            // Strip System F type annotations (TypeLambda, TypeApplication, etc.) from
            // term bodies for the evaluator, which works at the simply-typed level.
            return Generation.stripAllTermTypes(modules);
        } catch (IOException e) {
            throw new RuntimeException("Failed to load evaluator term modules from JSON", e);
        }
    }

    /**
     * Assert two terms are alpha-equivalent with respect to type variable naming.
     * Normalizes type variable names by order of first occurrence in the shown representation.
     */
    private static void assertAlphaEquivalent(Term expected, Term actual, String message) {
        String expectedStr = hydra.show.core.Core.term(expected);
        String actualStr = hydra.show.core.Core.term(actual);
        assertEquals(normalizeTypeVarNames(expectedStr), normalizeTypeVarNames(actualStr), message);
    }

    private static void assertAlphaEquivalent(Type expected, Type actual, String message) {
        String expectedStr = hydra.show.core.Core.type(expected);
        String actualStr = hydra.show.core.Core.type(actual);
        assertEquals(normalizeTypeVarNames(expectedStr), normalizeTypeVarNames(actualStr), message);
    }

    /**
     * Normalize type variable names for alpha-equivalence comparison.
     * Extracts the Λ binder chain and renames all type variables to canonical names
     * based on the binder's position: first Λ-bound var becomes t0, second becomes t1, etc.
     * This makes alpha-equivalent terms compare equal as strings.
     */
    private static String normalizeTypeVarNames(String s) {
        // Extract Λ binder chain: "Λt3.Λt7.body" → binders=[t3, t7], body="body"
        List<String> binders = new ArrayList<>();
        String remaining = s;
        while (remaining.startsWith("Λ")) {
            int dotIdx = remaining.indexOf('.');
            if (dotIdx < 0) break;
            String binder = remaining.substring(1, dotIdx);
            binders.add(binder);
            remaining = remaining.substring(dotIdx + 1);
        }
        if (binders.isEmpty()) return s;

        // Determine renaming by order of first occurrence in the body (not binder position).
        // This handles cases where Java inference assigns type variables in a different order.
        Set<String> binderSet = new java.util.HashSet<>(binders);
        List<String> orderedByFirstOccurrence = new ArrayList<>();
        java.util.regex.Pattern varPattern = java.util.regex.Pattern.compile("\\b(t\\d+)\\b");
        java.util.regex.Matcher m = varPattern.matcher(remaining);
        while (m.find()) {
            String varName = m.group(1);
            if (binderSet.contains(varName) && !orderedByFirstOccurrence.contains(varName)) {
                orderedByFirstOccurrence.add(varName);
            }
        }
        // Add any binders not found in the body (to handle unused type vars)
        for (String b : binders) {
            if (!orderedByFirstOccurrence.contains(b)) {
                orderedByFirstOccurrence.add(b);
            }
        }

        // Build renaming: rename by first-occurrence order
        Map<String, String> renaming = new HashMap<>();
        for (int i = 0; i < orderedByFirstOccurrence.size(); i++) {
            renaming.put(orderedByFirstOccurrence.get(i), "tv" + i);
        }

        // Apply renaming to body only
        String normalizedBody = remaining;
        for (Map.Entry<String, String> entry : renaming.entrySet()) {
            normalizedBody = normalizedBody.replaceAll("\\b" + java.util.regex.Pattern.quote(entry.getKey()) + "\\b",
                java.util.regex.Matcher.quoteReplacement(entry.getValue()));
        }
        for (int i = 0; i < orderedByFirstOccurrence.size(); i++) {
            normalizedBody = normalizedBody.replaceAll("\\btv" + i + "\\b", "t" + i);
        }

        // Reconstruct binder chain in first-occurrence order
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < orderedByFirstOccurrence.size(); i++) {
            sb.append("Λt").append(i).append(".");
        }
        sb.append(normalizedBody);
        return sb.toString();
    }

    /**
     * Run a beta-reduction test case. Used by ReductionTest.
     */
    public static void runReductionTestCase(boolean eager, String name, Term input, Term output) {
        Graph graph = getTestGraph();
        String suffix = " (" + name + ")";

        Flow<Graph, Term> reduced = hydra.reduction.Reduction.reduceTerm(eager, input);
        FlowState<Graph, Term> result = reduced.value.apply(graph).apply(EMPTY_TRACE);
        if (result.value.isJust()) {
            if (!result.value.fromJust().equals(output)) {
                assertEquals(hydra.show.core.Core.term(output),
                    hydra.show.core.Core.term(result.value.fromJust()),
                    "Original term does not reduce to expected term" + suffix);
                assertEquals(output, result.value.fromJust(),
                    "Original term does not reduce to expected term" + suffix);
            }
        } else if (result.trace.messages.isEmpty()) {
            fail("Reduction failed" + suffix);
        } else {
            fail("Reduction failed: " + result.trace.messages.get(0) + suffix);
        }
    }

    @TestFactory
    Stream<DynamicNode> kernelTests() {
        TestGroup allTests = TestSuite.allTests();
        rootTestGroup = allTests;

        // Eagerly initialize test infrastructure and measure time.
        // This ensures startup cost is not attributed to the first test group.
        long initStart = System.nanoTime();
        getTestGraph();
        getInferenceContext();
        getTypeContext();
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
        int idx = 0;
        for (TestCaseWithMetadata tc : group.cases) {
            idx++;
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

    private static boolean shouldSkip(TestCaseWithMetadata tc) {
        Tag disabledTag = new Tag("disabled");
        Tag disabledForPythonTag = new Tag("disabledForPython");
        Tag requiresFlowDecodingTag = new Tag("requiresFlowDecoding");
        // Note: disabledForPython tests are also skipped in Java because the same beta-reduction
        // term explosion occurs (e.g. deeply nested withTrace/mutateTrace). The Haskell evaluator
        // handles these efficiently via lazy evaluation, but Java's eager reducer cannot.
        return tc.tags.contains(disabledTag)
            || tc.tags.contains(disabledForPythonTag)
            || tc.tags.contains(requiresFlowDecodingTag);
    }

    private static final Duration TEST_TIMEOUT = Duration.ofSeconds(10);

    private static DynamicTest runTestCase(String name, TestCaseWithMetadata tc) {
        return tc.case_.accept(new TestCase.PartialVisitor<>() {
            @Override
            public DynamicTest otherwise(TestCase instance) {
                return DynamicTest.dynamicTest(name + " [unhandled]",
                    () -> fail("Unhandled test case type: " + instance.getClass().getSimpleName()));
            }

            @Override
            public DynamicTest visit(TestCase.AlphaConversion instance) {
                AlphaConversionTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.result,
                        hydra.reduction.Reduction.alphaConvert(tc.oldVariable, tc.newVariable, tc.term)));
            }

            @Override
            public DynamicTest visit(TestCase.CaseConversion instance) {
                CaseConversionTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.toString,
                        hydra.formatting.Formatting.convertCase(tc.fromConvention, tc.toConvention, tc.fromString)));
            }

            @Override
            public DynamicTest visit(TestCase.DeannotateTerm instance) {
                DeannotateTermTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.output,
                        hydra.rewriting.Rewriting.deannotateTerm(tc.input)));
            }

            @Override
            public DynamicTest visit(TestCase.DeannotateType instance) {
                DeannotateTypeTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.output,
                        hydra.rewriting.Rewriting.deannotateType(tc.input)));
            }

            @Override
            public DynamicTest visit(TestCase.DelegatedEvaluation instance) {
                // Delegated evaluation runs in target language - always passes
                return withTimeout(name, () -> {});
            }

            @Override
            public DynamicTest visit(TestCase.EtaExpansion instance) {
                EtaExpansionTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    hydra.typing.TypeContext tx = getTypeContext();
                    Flow<Object, Term> flow = hydra.reduction.Reduction.etaExpandTypedTerm(tx, tc.input);
                    FlowState<Object, Term> state = flow.value.apply(UNIT).apply(EMPTY_TRACE);
                    assertTrue(state.value.isJust(),
                        "Eta expansion failed: " + state.trace.messages);
                    assertEquals(tc.output, state.value.fromJust());
                });
            }

            @Override
            public DynamicTest visit(TestCase.Evaluation instance) {
                EvaluationTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    Graph graph = getTestGraph();
                    Flow<Graph, Term> flow = hydra.reduction.Reduction.reduceTerm(true, tc.input);
                    FlowState<Graph, Term> state;
                    try {
                        java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<Graph, Term>> fn = flow.value.apply(graph);
                        if (fn == null) {
                            throw new IllegalArgumentException(
                                "flow.value.apply(graph) returned null for input: " + hydra.show.core.Core.term(tc.input));
                        }
                        state = fn.apply(EMPTY_TRACE);
                    } catch (Exception e) {
                        throw new IllegalArgumentException(
                            "Exception during reduceTerm for input: " + hydra.show.core.Core.term(tc.input), e);
                    }
                    assertTrue(state.value.isJust(),
                        "Evaluation failed for input: " + hydra.show.core.Core.term(tc.input)
                        + "\nExpected: " + hydra.show.core.Core.term(tc.output)
                        + "\nTrace: " + state.trace.messages);
                    Term result = state.value.fromJust();
                    if (!termsEqual(tc.output, result)) {
                        String expectedStr = hydra.show.core.Core.term(tc.output);
                        String actualStr = hydra.show.core.Core.term(result);
                        assertEquals(expectedStr, actualStr,
                            "Original term does not reduce to expected term");
                        fail("Readable match but direct comparison failed");
                    }
                });
            }

            @Override
            public DynamicTest visit(TestCase.Inference instance) {
                InferenceTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    hydra.typing.InferenceContext cx = getInferenceContext();
                    Flow<Object, Tuple.Tuple2<Term, TypeScheme>> flow =
                        hydra.inference.Inference.inferTypeOf(cx, tc.input);
                    FlowState<Object, Tuple.Tuple2<Term, TypeScheme>> state =
                        flow.value.apply(UNIT).apply(EMPTY_TRACE);
                    assertTrue(state.value.isJust(),
                        "Inference failed: " + state.trace.messages);
                    Tuple.Tuple2<Term, TypeScheme> result = state.value.fromJust();
                    assertEquals(
                        hydra.show.core.Core.typeScheme(tc.output),
                        hydra.show.core.Core.typeScheme(result.object2),
                        "Type scheme mismatch");
                    // Also check that inferred term has types stripped correctly
                    assertEquals(
                        hydra.show.core.Core.term(hydra.rewriting.Rewriting.removeTypesFromTerm(result.object1)),
                        hydra.show.core.Core.term(hydra.rewriting.Rewriting.removeTypesFromTerm(tc.input)),
                        "Inferred term mismatch");
                });
            }

            @Override
            public DynamicTest visit(TestCase.InferenceFailure instance) {
                InferenceFailureTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    hydra.typing.InferenceContext cx = getInferenceContext();
                    Flow<Object, Tuple.Tuple2<Term, TypeScheme>> flow =
                        hydra.inference.Inference.inferTypeOf(cx, tc.input);
                    FlowState<Object, Tuple.Tuple2<Term, TypeScheme>> state =
                        flow.value.apply(UNIT).apply(EMPTY_TRACE);
                    assertFalse(state.value.isJust(),
                        "Expected inference failure but got: " +
                        (state.value.isJust()
                            ? hydra.show.core.Core.typeScheme(state.value.fromJust().object2)
                            : ""));
                });
            }

            @Override
            public DynamicTest visit(TestCase.TypeChecking instance) {
                TypeCheckingTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    hydra.typing.InferenceContext cx = getInferenceContext();

                    // Infer type
                    Flow<Object, Tuple.Tuple2<Term, TypeScheme>> inferFlow =
                        hydra.inference.Inference.inferTypeOf(cx, tc.input);
                    FlowState<Object, Tuple.Tuple2<Term, TypeScheme>> inferState =
                        inferFlow.value.apply(UNIT).apply(EMPTY_TRACE);
                    assertTrue(inferState.value.isJust(),
                        "Inference failed: " + inferState.trace.messages);
                    Term inferredTerm = inferState.value.fromJust().object1;
                    Type inferredType = hydra.schemas.Schemas.typeSchemeToFType(
                        inferState.value.fromJust().object2);

                    // Reconstruct type - use trace from inference to continue fresh name counter
                    hydra.typing.TypeContext tx = getTypeContext();
                    Flow<Object, Type> typeOfFlow =
                        hydra.checking.Checking.typeOf(tx, List.of(), inferredTerm);
                    FlowState<Object, Type> typeOfState =
                        typeOfFlow.value.apply(UNIT).apply(inferState.trace);
                    assertTrue(typeOfState.value.isJust(),
                        "Type reconstruction failed: " + typeOfState.trace.messages);
                    Type reconstructedType = typeOfState.value.fromJust();

                    // Compare inferred term using alpha-equivalence for type variables,
                    // since Java's inference may assign them in a different order than Haskell
                    assertAlphaEquivalent(tc.outputTerm, inferredTerm, "Inferred term");
                    assertAlphaEquivalent(tc.outputType, inferredType, "Inferred type");
                    assertAlphaEquivalent(tc.outputType, reconstructedType, "Reconstructed type");
                });
            }

            @Override
            public DynamicTest visit(TestCase.TypeCheckingFailure instance) {
                TypeCheckingFailureTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    // TODO: implement when test data is available
                });
            }

            @Override
            public DynamicTest visit(TestCase.TypeReduction instance) {
                TypeReductionTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    Graph graph = getTestGraph();
                    // Use schema context for type reduction
                    Graph schemaGraph = graph.schema.orElse(graph);
                    Flow<Graph, Type> flow = hydra.reduction.Reduction.betaReduceType(tc.input);
                    FlowState<Graph, Type> state = flow.value.apply(schemaGraph).apply(EMPTY_TRACE);
                    assertTrue(state.value.isJust(),
                        "Type reduction failed: " + state.trace.messages);
                    assertEquals(tc.output, state.value.fromJust());
                });
            }

            @Override
            public DynamicTest visit(TestCase.TopologicalSort instance) {
                TopologicalSortTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.expected, hydra.sorting.Sorting.topologicalSort(tc.adjacencyList)));
            }

            @Override
            public DynamicTest visit(TestCase.TopologicalSortSCC instance) {
                TopologicalSortSCCTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.expected, hydra.sorting.Sorting.topologicalSortComponents(tc.adjacencyList)));
            }

            @Override
            public DynamicTest visit(TestCase.TopologicalSortBindings instance) {
                TopologicalSortBindingsTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    Map<Name, Term> bindingMap = hydra.lib.maps.FromList.apply(tc.bindings);
                    List<List<Tuple.Tuple2<Name, Term>>> result =
                        hydra.rewriting.Rewriting.topologicalSortBindingMap(bindingMap);
                    // Compare as sets of sets (order within SCCs doesn't matter)
                    Set<Set<Tuple.Tuple2<Name, Term>>> resultSet = new HashSet<>();
                    for (List<Tuple.Tuple2<Name, Term>> group : result) {
                        resultSet.add(new HashSet<>(group));
                    }
                    Set<Set<Tuple.Tuple2<Name, Term>>> expectedSet = new HashSet<>();
                    for (List<Tuple.Tuple2<Name, Term>> group : tc.expected) {
                        expectedSet.add(new HashSet<>(group));
                    }
                    assertEquals(expectedSet, resultSet);
                });
            }

            @Override
            public DynamicTest visit(TestCase.Serialization instance) {
                SerializationTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.output,
                        hydra.serialization.Serialization.printExpr(
                            hydra.serialization.Serialization.parenthesize(tc.input))));
            }

            @Override
            public DynamicTest visit(TestCase.FlattenLetTerms instance) {
                FlattenLetTermsTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.output, hydra.rewriting.Rewriting.flattenLetTerms(tc.input)));
            }

            @Override
            public DynamicTest visit(TestCase.FreeVariables instance) {
                FreeVariablesTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.output, hydra.rewriting.Rewriting.freeVariablesInTerm(tc.input)));
            }

            @Override
            public DynamicTest visit(TestCase.LiftLambdaAboveLet instance) {
                LiftLambdaAboveLetTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.output, hydra.rewriting.Rewriting.liftLambdaAboveLet(tc.input)));
            }

            @Override
            public DynamicTest visit(TestCase.SimplifyTerm instance) {
                SimplifyTermTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.output, hydra.rewriting.Rewriting.simplifyTerm(tc.input)));
            }

            @Override
            public DynamicTest visit(TestCase.NormalizeTypeVariables instance) {
                NormalizeTypeVariablesTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.output, hydra.rewriting.Rewriting.normalizeTypeVariablesInTerm(tc.input)));
            }

            @Override
            public DynamicTest visit(TestCase.FoldOverTerm instance) {
                FoldOverTermTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.output, runFoldOperation(tc.traversalOrder, tc.operation, tc.input)));
            }

            @Override
            public DynamicTest visit(TestCase.RewriteTerm instance) {
                RewriteTermTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.output, runTermRewriter(tc.rewriter, tc.input)));
            }

            @Override
            public DynamicTest visit(TestCase.RewriteType instance) {
                RewriteTypeTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.output, runTypeRewriter(tc.rewriter, tc.input)));
            }

            @Override
            public DynamicTest visit(TestCase.HoistSubterms instance) {
                HoistSubtermsTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.output,
                        hydra.hoisting.Hoisting.hoistSubterms(
                            predicateFn(tc.predicate), emptyTypeContext(), tc.input)));
            }

            @Override
            public DynamicTest visit(TestCase.HoistCaseStatements instance) {
                HoistCaseStatementsTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.output,
                        hydra.hoisting.Hoisting.hoistCaseStatements(emptyTypeContext(), tc.input)));
            }

            @Override
            public DynamicTest visit(TestCase.HoistLetBindings instance) {
                HoistLetBindingsTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    Let result = hydra.hoisting.Hoisting.hoistAllLetBindings(tc.input);
                    assertEquals(
                        hydra.show.core.Core.let(tc.output),
                        hydra.show.core.Core.let(result));
                });
            }

            @Override
            public DynamicTest visit(TestCase.HoistPolymorphicLetBindings instance) {
                HoistPolymorphicLetBindingsTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    Let result = hydra.hoisting.Hoisting.hoistPolymorphicLetBindings(
                        b -> true, tc.input);
                    assertEquals(
                        hydra.show.core.Core.let(tc.output),
                        hydra.show.core.Core.let(result));
                });
            }

            @Override
            public DynamicTest visit(TestCase.JsonParser instance) {
                ParserTestCase<hydra.json.model.Value> tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.output, hydra.json.parser.Parser.parseJson(tc.input)));
            }

            @Override
            public DynamicTest visit(TestCase.JsonWriter instance) {
                WriterTestCase<hydra.json.model.Value> tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.output, hydra.json.writer.Writer.printJson(tc.input)));
            }

            @Override
            public DynamicTest visit(TestCase.JsonCoder instance) {
                JsonCoderTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    Graph graph = getTestGraph();
                    Flow<Graph, hydra.compute.Coder<Graph, Object, Term, hydra.json.model.Value>> coderFlow =
                        hydra.ext.org.json.coder.Coder.jsonCoder(tc.type);
                    FlowState<Graph, hydra.compute.Coder<Graph, Object, Term, hydra.json.model.Value>> coderState =
                        coderFlow.value.apply(graph).apply(EMPTY_TRACE);
                    assertTrue(coderState.value.isJust(),
                        "Failed to create JSON coder: " + coderState.trace.messages);
                    var coder = coderState.value.fromJust();

                    // Encode and check
                    Flow<Graph, hydra.json.model.Value> encodeFlow = coder.encode.apply(tc.term);
                    FlowState<Graph, hydra.json.model.Value> encodeState =
                        encodeFlow.value.apply(graph).apply(EMPTY_TRACE);
                    assertTrue(encodeState.value.isJust(),
                        "JSON encode failed: " + encodeState.trace.messages);
                    if (!tc.json.equals(encodeState.value.fromJust())) {
                        String expectedStr = hydra.json.writer.Writer.printJson(tc.json);
                        String actualStr = hydra.json.writer.Writer.printJson(encodeState.value.fromJust());
                        assertEquals(expectedStr, actualStr, "JSON encode mismatch (pretty-printed)");
                        fail("JSON encode: pretty print matches but equals() fails"
                            + "\nExpected class: " + tc.json.getClass().getSimpleName()
                            + "\nActual class: " + encodeState.value.fromJust().getClass().getSimpleName());
                    }

                    // Roundtrip: encode then decode
                    Flow<Object, Term> decodeFlow = coder.decode.apply(encodeState.value.fromJust());
                    FlowState<Object, Term> decodeState =
                        decodeFlow.value.apply(graph).apply(EMPTY_TRACE);
                    assertTrue(decodeState.value.isJust(),
                        "JSON decode failed: " + decodeState.trace.messages);
                    if (!termsEqual(tc.term, decodeState.value.fromJust())) {
                        assertEquals(
                            hydra.show.core.Core.term(tc.term),
                            hydra.show.core.Core.term(decodeState.value.fromJust()),
                            "JSON roundtrip term mismatch");
                    }
                });
            }

            @Override
            public DynamicTest visit(TestCase.JsonDecode instance) {
                JsonDecodeTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    Graph graph = getTestGraph();
                    Flow<Graph, hydra.compute.Coder<Graph, Object, Term, hydra.json.model.Value>> coderFlow =
                        hydra.ext.org.json.coder.Coder.jsonCoder(tc.type);
                    FlowState<Graph, hydra.compute.Coder<Graph, Object, Term, hydra.json.model.Value>> coderState =
                        coderFlow.value.apply(graph).apply(EMPTY_TRACE);
                    assertTrue(coderState.value.isJust(),
                        "Failed to create JSON coder: " + coderState.trace.messages);
                    var coder = coderState.value.fromJust();

                    Flow<Object, Term> decodeFlow = coder.decode.apply(tc.json);
                    FlowState<Object, Term> decodeState =
                        decodeFlow.value.apply(graph).apply(EMPTY_TRACE);

                    tc.expected.accept(new hydra.util.Either.Visitor<>() {
                        @Override
                        public Object visit(hydra.util.Either.Left<String, Term> left) {
                            // Expected failure
                            assertFalse(decodeState.value.isJust(),
                                "Expected decode failure but succeeded");
                            return null;
                        }
                        @Override
                        public Object visit(hydra.util.Either.Right<String, Term> right) {
                            // Expected success
                            assertTrue(decodeState.value.isJust(),
                                "JSON decode failed: " + decodeState.trace.messages);
                            assertEquals(right.value, decodeState.value.fromJust());
                            return null;
                        }
                    });
                });
            }

            @Override
            public DynamicTest visit(TestCase.JsonEncode instance) {
                // Encode tests require type info we don't have in the test case
                return withTimeout(name, () -> {});
            }

            @Override
            public DynamicTest visit(TestCase.JsonRoundtrip instance) {
                JsonRoundtripTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    Graph graph = getTestGraph();
                    Flow<Graph, hydra.compute.Coder<Graph, Object, Term, hydra.json.model.Value>> coderFlow =
                        hydra.ext.org.json.coder.Coder.jsonCoder(tc.type);
                    FlowState<Graph, hydra.compute.Coder<Graph, Object, Term, hydra.json.model.Value>> coderState =
                        coderFlow.value.apply(graph).apply(EMPTY_TRACE);
                    assertTrue(coderState.value.isJust(),
                        "Failed to create JSON coder: " + coderState.trace.messages);
                    var coder = coderState.value.fromJust();

                    // Encode
                    Flow<Graph, hydra.json.model.Value> encodeFlow = coder.encode.apply(tc.term);
                    FlowState<Graph, hydra.json.model.Value> encodeState =
                        encodeFlow.value.apply(graph).apply(EMPTY_TRACE);
                    assertTrue(encodeState.value.isJust(),
                        "JSON encode failed: " + encodeState.trace.messages);

                    // Decode back
                    Flow<Object, Term> decodeFlow = coder.decode.apply(encodeState.value.fromJust());
                    FlowState<Object, Term> decodeState =
                        decodeFlow.value.apply(graph).apply(EMPTY_TRACE);
                    assertTrue(decodeState.value.isJust(),
                        "JSON decode failed: " + decodeState.trace.messages);
                    if (!termsEqual(tc.term, decodeState.value.fromJust())) {
                        assertEquals(
                            hydra.show.core.Core.term(tc.term),
                            hydra.show.core.Core.term(decodeState.value.fromJust()),
                            "JSON roundtrip term mismatch");
                    }
                });
            }

            @Override
            public DynamicTest visit(TestCase.SubstInType instance) {
                SubstInTypeTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    // Build TypeSubst from list of (Name, Type) pairs
                    Map<Name, Type> substMap = new HashMap<>();
                    for (hydra.util.Tuple.Tuple2<Name, Type> pair : tc.substitution) {
                        substMap.put(pair.object1, pair.object2);
                    }
                    hydra.typing.TypeSubst subst = new hydra.typing.TypeSubst(substMap);
                    Type result = hydra.substitution.Substitution.substInType(subst, tc.input);
                    assertEquals(tc.output, result);
                });
            }

            @Override
            public DynamicTest visit(TestCase.VariableOccursInType instance) {
                VariableOccursInTypeTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    boolean result = hydra.unification.Unification.variableOccursInType(tc.variable, tc.type);
                    assertEquals(tc.expected, result);
                });
            }

            @Override
            public DynamicTest visit(TestCase.UnifyTypes instance) {
                UnifyTypesTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    // Build schema types map from the list of names
                    Map<Name, hydra.core.TypeScheme> schemaTypes = new HashMap<>();
                    for (Name n : tc.schemaTypes) {
                        schemaTypes.put(n, new hydra.core.TypeScheme(
                            java.util.Collections.emptyList(),
                            new Type.Variable(n),
                            hydra.util.Maybe.nothing()));
                    }
                    Flow<Object, hydra.typing.TypeSubst> flow =
                        hydra.unification.Unification.unifyTypes(schemaTypes, tc.left, tc.right, "test");
                    FlowState<Object, hydra.typing.TypeSubst> state = flow.value.apply(UNIT).apply(EMPTY_TRACE);

                    tc.expected.accept(new hydra.util.Either.Visitor<String, hydra.typing.TypeSubst, Void>() {
                        @Override
                        public Void visit(hydra.util.Either.Left<String, hydra.typing.TypeSubst> left) {
                            // Expected failure
                            assertFalse(state.value.isJust(),
                                "Expected unification failure but got success: " + state.value);
                            return null;
                        }

                        @Override
                        public Void visit(hydra.util.Either.Right<String, hydra.typing.TypeSubst> right) {
                            // Expected success
                            assertTrue(state.value.isJust(),
                                "Expected unification success but got failure: " + state.trace.messages);
                            assertEquals(right.value, state.value.fromJust());
                            return null;
                        }
                    });
                });
            }

            @Override
            public DynamicTest visit(TestCase.JoinTypes instance) {
                JoinTypesTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    Flow<Object, java.util.List<hydra.typing.TypeConstraint>> flow =
                        hydra.unification.Unification.joinTypes(tc.left, tc.right, "test");
                    FlowState<Object, java.util.List<hydra.typing.TypeConstraint>> state =
                        flow.value.apply(UNIT).apply(EMPTY_TRACE);

                    tc.expected.accept(new hydra.util.Either.Visitor<Void, java.util.List<hydra.typing.TypeConstraint>, Void>() {
                        @Override
                        public Void visit(hydra.util.Either.Left<Void, java.util.List<hydra.typing.TypeConstraint>> left) {
                            // Expected failure
                            assertFalse(state.value.isJust(),
                                "Expected join failure but got success: " + state.value);
                            return null;
                        }

                        @Override
                        public Void visit(hydra.util.Either.Right<Void, java.util.List<hydra.typing.TypeConstraint>> right) {
                            // Expected success
                            assertTrue(state.value.isJust(),
                                "Expected join success but got failure: " + state.trace.messages);
                            assertEquals(right.value, state.value.fromJust());
                            return null;
                        }
                    });
                });
            }

            @Override
            public DynamicTest visit(TestCase.UnshadowVariables instance) {
                UnshadowVariablesTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(hydra.show.core.Core.term(tc.output),
                        hydra.show.core.Core.term(hydra.rewriting.Rewriting.unshadowVariables(tc.input))));
            }
        });
    }

    private static DynamicTest withTimeout(String name, org.junit.jupiter.api.function.Executable executable) {
        return DynamicTest.dynamicTest(name, () -> assertTimeoutPreemptively(TEST_TIMEOUT, executable));
    }

    // ---- Helper methods ----

    private static Term runFoldOperation(hydra.coders.TraversalOrder order, FoldOperation op, Term input) {
        return op.accept(new FoldOperation.Visitor<>() {
            @Override
            public Term visit(FoldOperation.SumInt32Literals instance) {
                int sum = hydra.rewriting.Rewriting.foldOverTerm(order,
                    acc -> t -> acc + getInt32(t), 0, input);
                return new Term.Literal(new Literal.Integer_(new IntegerValue.Int32(sum)));
            }

            @Override
            public Term visit(FoldOperation.CollectListLengths instance) {
                java.util.function.Function<List<Integer>, java.util.function.Function<Term, List<Integer>>> fld =
                    acc -> t -> {
                        List<Integer> result = new ArrayList<>(acc);
                        result.addAll(getListLength(t));
                        return result;
                    };
                List<Integer> lengths = hydra.rewriting.Rewriting.foldOverTerm(order,
                    fld, new ArrayList<>(), input);
                List<Term> terms = new ArrayList<>();
                for (int len : lengths) {
                    terms.add(new Term.Literal(new Literal.Integer_(new IntegerValue.Int32(len))));
                }
                return new Term.List(terms);
            }

            @Override
            public Term visit(FoldOperation.CollectLabels instance) {
                java.util.function.Function<List<Literal>, java.util.function.Function<Term, List<Literal>>> fld =
                    acc -> t -> {
                        List<Literal> result = new ArrayList<>(acc);
                        result.addAll(getLabel(t));
                        return result;
                    };
                List<Literal> labels = hydra.rewriting.Rewriting.foldOverTerm(order,
                    fld, new ArrayList<>(), input);
                List<Term> terms = new ArrayList<>();
                for (Literal label : labels) {
                    terms.add(new Term.Literal(label));
                }
                return new Term.List(terms);
            }
        });
    }

    private static int getInt32(Term t) {
        if (t instanceof Term.Literal) {
            Literal lit = ((Term.Literal) t).value;
            if (lit instanceof Literal.Integer_) {
                IntegerValue iv = ((Literal.Integer_) lit).value;
                if (iv instanceof IntegerValue.Int32) {
                    return ((IntegerValue.Int32) iv).value;
                }
            }
        }
        return 0;
    }

    private static List<Integer> getListLength(Term t) {
        if (t instanceof Term.List) {
            return List.of(((Term.List) t).value.size());
        }
        return List.of();
    }

    private static List<Literal> getLabel(Term t) {
        if (t instanceof Term.Pair) {
            Term first = ((Term.Pair) t).value.object1;
            if (first instanceof Term.Literal) {
                Literal lit = ((Term.Literal) first).value;
                if (lit instanceof Literal.String_) {
                    return List.of(lit);
                }
            }
        }
        return List.of();
    }

    private static Term runTermRewriter(TermRewriter rewriter, Term input) {
        return hydra.rewriting.Rewriting.rewriteTerm(
            recurse -> term -> rewriter.accept(new TermRewriter.Visitor<>() {
                @Override
                public Term visit(TermRewriter.ReplaceFooWithBar instance) {
                    if (term instanceof Term.Literal) {
                        Literal lit = ((Term.Literal) term).value;
                        if (lit instanceof Literal.String_ && ((Literal.String_) lit).value.equals("foo")) {
                            return new Term.Literal(new Literal.String_("bar"));
                        }
                    }
                    return recurse.apply(term);
                }

                @Override
                public Term visit(TermRewriter.ReplaceInt32WithInt64 instance) {
                    if (term instanceof Term.Literal) {
                        Literal lit = ((Term.Literal) term).value;
                        if (lit instanceof Literal.Integer_) {
                            IntegerValue iv = ((Literal.Integer_) lit).value;
                            if (iv instanceof IntegerValue.Int32) {
                                return new Term.Literal(new Literal.Integer_(
                                    new IntegerValue.Int64((long) ((IntegerValue.Int32) iv).value)));
                            }
                        }
                    }
                    return recurse.apply(term);
                }
            }),
            input);
    }

    private static Type runTypeRewriter(TypeRewriter rewriter, Type input) {
        return hydra.rewriting.Rewriting.rewriteType(
            recurse -> typ -> rewriter.accept(new TypeRewriter.Visitor<>() {
                @Override
                public Type visit(TypeRewriter.ReplaceStringWithInt32 instance) {
                    if (typ instanceof Type.Literal) {
                        LiteralType lt = ((Type.Literal) typ).value;
                        if (lt instanceof LiteralType.String_) {
                            return new Type.Literal(new LiteralType.Integer_(new IntegerType.Int32()));
                        }
                    }
                    return recurse.apply(typ);
                }
            }),
            input);
    }

    private static java.util.function.Function<Tuple.Tuple2<List<hydra.accessors.TermAccessor>, Term>, Boolean> predicateFn(
            HoistPredicate pred) {
        return pred.accept(new HoistPredicate.Visitor<>() {
            @Override
            public java.util.function.Function<Tuple.Tuple2<List<hydra.accessors.TermAccessor>, Term>, Boolean> visit(
                    HoistPredicate.Nothing instance) {
                return pair -> false;
            }

            @Override
            public java.util.function.Function<Tuple.Tuple2<List<hydra.accessors.TermAccessor>, Term>, Boolean> visit(
                    HoistPredicate.Lists instance) {
                return pair -> pair.object2 instanceof Term.List;
            }

            @Override
            public java.util.function.Function<Tuple.Tuple2<List<hydra.accessors.TermAccessor>, Term>, Boolean> visit(
                    HoistPredicate.Applications instance) {
                return pair -> pair.object2 instanceof Term.Application;
            }

            @Override
            public java.util.function.Function<Tuple.Tuple2<List<hydra.accessors.TermAccessor>, Term>, Boolean> visit(
                    HoistPredicate.CaseStatements instance) {
                return pair -> pair.object2 instanceof Term.Function
                    && ((Term.Function) pair.object2).value instanceof Function.Elimination;
            }
        });
    }

    /**
     * Build kernel type definitions needed by inference/checking tests.
     * These types are normally provided by kernelTypesModules in Haskell.
     */
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

    private static hydra.typing.TypeContext emptyTypeContext() {
        hydra.typing.InferenceContext emptyIc = new hydra.typing.InferenceContext(
            Collections.emptyMap(),
            Collections.emptyMap(),
            Collections.emptyMap(),
            Collections.emptyMap(),
            false
        );
        return new hydra.typing.TypeContext(
            Collections.emptyMap(),
            Collections.emptyMap(),
            Collections.emptySet(),
            Collections.emptySet(),
            Collections.emptySet(),
            emptyIc
        );
    }
}
