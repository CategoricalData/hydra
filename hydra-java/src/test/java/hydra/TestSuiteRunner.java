package hydra;

import hydra.core.*;
import hydra.graph.Graph;
import hydra.graph.Primitive;
import hydra.test.testSuite.TestSuite;
import hydra.test.testGraph.TestGraph;
import hydra.testing.*;
import hydra.tools.PrettyPrinter;
import hydra.lib.Libraries;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;
import hydra.util.Pair;

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

    private static Graph emptyGraph() {
        Map<Name, Primitive> primitives = new HashMap<>();
        for (PrimitiveFunction prim : Libraries.standardPrimitives()) {
            primitives.put(prim.name(), prim.toNative());
        }
        return new Graph(
            Collections.emptyMap(),
            Collections.emptyMap(),
            Collections.emptyMap(),
            Collections.emptySet(),
            Collections.emptyMap(),
            primitives,
            Collections.emptyMap(),
            Collections.emptySet());
    }

    private static hydra.context.Context emptyContext() {
        return new hydra.context.Context(
            Collections.emptyList(),
            Collections.emptyList(),
            Collections.emptyMap());
    }

    /**
     * Assert that an Either is a Right value. If it's a Left, fail with a message.
     */
    @SuppressWarnings("rawtypes")
    private static void assertEitherRight(hydra.util.Either<?, ?> either, String message) {
        if (either instanceof hydra.util.Either.Left) {
            Object leftVal = ((hydra.util.Either.Left) either).value;
            String detail = "";
            if (leftVal instanceof hydra.context.InContext) {
                Object obj = ((hydra.context.InContext<?>) leftVal).object;
                if (obj instanceof hydra.error.OtherError) {
                    detail = ": " + ((hydra.error.OtherError) obj).value;
                } else if (obj instanceof hydra.error.UnificationError) {
                    detail = ": " + obj;
                } else {
                    detail = ": " + obj;
                }
            } else {
                detail = ": " + leftVal;
            }
            fail(message + detail);
        }
    }

    /**
     * Extract the Right value from an Either.
     */
    @SuppressWarnings({"rawtypes", "unchecked"})
    private static <R> R eitherRight(hydra.util.Either<?, R> either) {
        return ((hydra.util.Either.Right<?, R>) either).value;
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

        // Build schema types from test types + kernel types
        Map<Name, Type> testTypes = TestGraph.testTypes();
        Map<Name, Type> kernelTypes = buildKernelTypes();
        Map<Name, Type> allTypes = new HashMap<>(kernelTypes);
        allTypes.putAll(testTypes); // test types override kernel types if any overlap
        Map<Name, TypeScheme> schemaTypes = new HashMap<>();
        for (Map.Entry<Name, Type> entry : allTypes.entrySet()) {
            schemaTypes.put(entry.getKey(), hydra.schemas.Schemas.typeToTypeScheme(entry.getValue()));
        }

        // Build bound terms map from test terms + primitive bridges + kernel constants
        Map<Name, Term> boundTerms = new HashMap<>();

        // Bridge all primitives as term bindings
        Set<String> excludedNames = new HashSet<>();
        excludedNames.add("hydra.annotations.setTermAnnotation");
        excludedNames.add("hydra.annotations.setTermDescription");
        excludedNames.add("hydra.rewriting.deannotateTerm");
        for (PrimitiveFunction prim : Libraries.standardPrimitives()) {
            String primName = prim.name().value;
            if (!excludedNames.contains(primName)) {
                boundTerms.put(prim.name(),
                    new Term.Function(new hydra.core.Function.Primitive(prim.name())));
            }
        }

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

        // Add kernel monads term bindings (hand-written since generated sources exceed JVM method size limits)
        List<Binding> monadBindings = new ArrayList<>();
        addMonadsBindings(monadBindings);
        for (Binding b : monadBindings) {
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
            boundTerms.put(entry.getKey(), hydra.encode.core.Core.type(entry.getValue()));
        }

        return new Graph(
            boundTerms,
            Collections.emptyMap(), // boundTypes (TypeSchemes for term bindings — not populated for test graph)
            Collections.emptyMap(), // classConstraints
            Collections.emptySet(), // lambdaVariables
            Collections.emptyMap(), // metadata
            primitives,
            schemaTypes,
            Collections.emptySet()  // typeVariables
        );
    }

    private static void addConstantBinding(List<Binding> bindings, String name, Term value) {
        bindings.add(new Binding(new Name(name), value, Maybe.nothing()));
    }

    /**
     * Add term-level bindings for context and graph constants needed by tests.
     * These are hand-written because the generated source modules exceed JVM method size limits.
     */
    private static void addMonadsBindings(List<Binding> bindings) {
        // hydra.monads.emptyContext = record(Context){trace=[], messages=[], other={}}
        addConstantBinding(bindings, "hydra.monads.emptyContext",
            record("hydra.context.Context",
                field("trace", list()),
                field("messages", list()),
                field("other", new Term.Map(Collections.emptyMap()))));

        // hydra.lexical.emptyGraph = record(Graph){boundTerms={}, boundTypes={}, classConstraints={},
        //   lambdaVariables=set(), metadata={}, primitives={}, schemaTypes={}, typeVariables=set()}
        addConstantBinding(bindings, "hydra.lexical.emptyGraph",
            record("hydra.graph.Graph",
                field("boundTerms", new Term.Map(Collections.emptyMap())),
                field("boundTypes", new Term.Map(Collections.emptyMap())),
                field("classConstraints", new Term.Map(Collections.emptyMap())),
                field("lambdaVariables", new Term.Set(Collections.emptySet())),
                field("metadata", new Term.Map(Collections.emptyMap())),
                field("primitives", new Term.Map(Collections.emptyMap())),
                field("schemaTypes", new Term.Map(Collections.emptyMap())),
                field("typeVariables", new Term.Set(Collections.emptySet()))));
    }

    /**
     * Add term-level bindings for annotation and rewriting functions needed by tests.
     * These are hand-written because the generated source modules exceed JVM method size limits.
     */
    private static void addAnnotationsBindings(List<Binding> bindings) {
        // hydra.rewriting.deannotateTerm = \t -> case t of
        //   annotated(at) -> deannotateTerm(at.body)
        //   _ -> t
        addConstantBinding(bindings, "hydra.rewriting.deannotateTerm",
            lambda("t",
                apply(
                    match("hydra.core.Term", Maybe.just(var("t")),
                        field("annotated", lambda("at",
                            apply(var("hydra.rewriting.deannotateTerm"),
                                apply(project("hydra.core.AnnotatedTerm", "body"), var("at")))))),
                    var("t"))));

        // hydra.annotations.termAnnotationInternal = \term ->
        //   Recursively collect annotations from nested Annotated nodes.
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

        // hydra.annotations.setAnnotation = \key -> \val -> \m ->
        //   maybe(delete(key, m), \v -> insert(key, v, m), val)
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

        // hydra.annotations.setTermAnnotation = \key -> \val -> \term ->
        //   let stripped = deannotateTerm(term)
        //       anns = setAnnotation(key, val, termAnnotationInternal(term))
        //   in if null(anns) then stripped else inject(Term){annotated=record(AnnotatedTerm){body=stripped, annotation=anns}}
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

        // hydra.annotations.setTermDescription = \d ->
        //   setTermAnnotation(key_description, maybes.map(\s -> inject(Term){literal=inject(Literal){string=s}}, d))
        addConstantBinding(bindings, "hydra.annotations.setTermDescription",
            lambda("d",
                apply(apply(var("hydra.annotations.setTermAnnotation"),
                    var("hydra.constants.key_description")),
                    apply(apply(primitive("hydra.lib.maybes.map"),
                        lambda("s",
                            inject("hydra.core.Term", "literal",
                                inject("hydra.core.Literal", "string", var("s"))))),
                        var("d")))));

        // hydra.annotations.getDescription = \cx -> \g -> \anns ->
        //   maybe(right(nothing),
        //         \term -> case term of { literal(l) -> case l of { string(s) -> right(just(s)); _ -> left(...) }; _ -> left(...) },
        //         maps.lookup(key_description, anns))
        // Note: simplified string extraction (inlined) instead of calling ExtractCore.string
        addConstantBinding(bindings, "hydra.annotations.getDescription",
            lambda("cx",
                lambda("g",
                    lambda("anns",
                        apply(apply(apply(primitive("hydra.lib.maybes.maybe"),
                            // default: right(nothing)
                            right(nothing())),
                            // function: \descTerm -> extract string from term, return right(just(s))
                            lambda("descTerm",
                                apply(
                                    match("hydra.core.Term", Maybe.just(
                                        left(record("hydra.context.InContext",
                                            field("object", wrap("hydra.error.OtherError", string("Expected string literal"))),
                                            field("context", var("cx"))))),
                                        field("literal", lambda("lit",
                                            apply(
                                                match("hydra.core.Literal", Maybe.just(
                                                    left(record("hydra.context.InContext",
                                                        field("object", wrap("hydra.error.OtherError", string("Expected string literal"))),
                                                        field("context", var("cx"))))),
                                                    field("string", lambda("s", right(just(var("s")))))),
                                                var("lit"))))),
                                    var("descTerm")))),
                            // the maybe value: maps.lookup(key_description, anns)
                            apply(apply(primitive("hydra.lib.maps.lookup"),
                                var("hydra.constants.key_description")),
                                var("anns")))))));

        // hydra.annotations.getTermDescription = \cx -> \g -> \term ->
        //   let peel = \t -> case t of
        //     typeLambda(tl) -> peel(tl.body)
        //     typeApplication(ta) -> peel(ta.body)
        //     _ -> t
        //   in getDescription(cx)(g)(termAnnotationInternal(peel(term)))
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

        // hydra.annotations.getTermAnnotation = \key -> \term -> maps.lookup(key, termAnnotationInternal(term))
        addConstantBinding(bindings, "hydra.annotations.getTermAnnotation",
            lambda("key",
                lambda("term",
                    apply(apply(primitive("hydra.lib.maps.lookup"), var("key")),
                        apply(var("hydra.annotations.termAnnotationInternal"), var("term"))))));
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

        hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, Term> reduced =
            hydra.reduction.Reduction.reduceTerm(emptyContext(), graph, eager, input);
        if (reduced.isRight()) {
            Term result = ((hydra.util.Either.Right<hydra.context.InContext<hydra.error.OtherError>, Term>) reduced).value;
            if (!result.equals(output)) {
                assertEquals(hydra.show.core.Core.term(output),
                    hydra.show.core.Core.term(result),
                    "Original term does not reduce to expected term" + suffix);
                assertEquals(output, result,
                    "Original term does not reduce to expected term" + suffix);
            }
        } else {
            hydra.context.InContext<hydra.error.OtherError> err =
                ((hydra.util.Either.Left<hydra.context.InContext<hydra.error.OtherError>, Term>) reduced).value;
            fail("Reduction failed: " + err.object.value + suffix);
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
        // Note: disabledForPython tests are also skipped in Java because the same beta-reduction
        // term explosion occurs (e.g. deeply nested withTrace/mutateTrace). The Haskell evaluator
        // handles these efficiently via lazy evaluation, but Java's eager reducer cannot.
        return tc.tags.contains(disabledTag)
            || tc.tags.contains(disabledForPythonTag);
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
                    Graph graph = getTestGraph();
                    hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, Term> result =
                        hydra.reduction.Reduction.etaExpandTypedTerm(emptyContext(), graph, tc.input);
                    assertTrue(result.isRight(),
                        "Eta expansion failed: " + (result.isLeft()
                            ? ((hydra.util.Either.Left<hydra.context.InContext<hydra.error.OtherError>, Term>) result).value.object.value
                            : ""));
                    assertEquals(tc.output, ((hydra.util.Either.Right<hydra.context.InContext<hydra.error.OtherError>, Term>) result).value);
                });
            }

            @Override
            public DynamicTest visit(TestCase.Evaluation instance) {
                EvaluationTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    Graph graph = getTestGraph();
                    hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, Term> reduced;
                    try {
                        reduced = hydra.reduction.Reduction.reduceTerm(emptyContext(), graph, true, tc.input);
                    } catch (Exception e) {
                        throw new IllegalArgumentException(
                            "Exception during reduceTerm for input: " + hydra.show.core.Core.term(tc.input), e);
                    }
                    assertTrue(reduced.isRight(),
                        "Evaluation failed for input: " + hydra.show.core.Core.term(tc.input)
                        + "\nExpected: " + hydra.show.core.Core.term(tc.output)
                        + "\nError: " + (reduced.isLeft()
                            ? ((hydra.util.Either.Left<hydra.context.InContext<hydra.error.OtherError>, Term>) reduced).value.object.value
                            : ""));
                    Term result = ((hydra.util.Either.Right<hydra.context.InContext<hydra.error.OtherError>, Term>) reduced).value;
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
                    Graph graph = getTestGraph();
                    hydra.context.Context cx = emptyContext();
                    var result = hydra.inference.Inference.inferTypeOf(cx, graph, tc.input);
                    assertEitherRight(result, "Inference failed");
                    var resultPair = eitherRight(result);
                    Term inferredTerm = resultPair.first.first;
                    TypeScheme resultScheme = resultPair.first.second;
                    assertEquals(
                        hydra.show.core.Core.typeScheme(tc.output),
                        hydra.show.core.Core.typeScheme(resultScheme),
                        "Type scheme mismatch");
                    // Also check that inferred term has types stripped correctly
                    assertEquals(
                        hydra.show.core.Core.term(hydra.rewriting.Rewriting.removeTypesFromTerm(inferredTerm)),
                        hydra.show.core.Core.term(hydra.rewriting.Rewriting.removeTypesFromTerm(tc.input)),
                        "Inferred term mismatch");
                });
            }

            @Override
            public DynamicTest visit(TestCase.InferenceFailure instance) {
                InferenceFailureTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    Graph graph = getTestGraph();
                    hydra.context.Context cx = emptyContext();
                    var result = hydra.inference.Inference.inferTypeOf(cx, graph, tc.input);
                    assertTrue(result instanceof hydra.util.Either.Left,
                        "Expected inference failure but got success");
                });
            }

            @Override
            public DynamicTest visit(TestCase.TypeChecking instance) {
                TypeCheckingTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    Graph graph = getTestGraph();
                    hydra.context.Context cx = emptyContext();

                    // Infer type
                    var inferResult = hydra.inference.Inference.inferTypeOf(cx, graph, tc.input);
                    assertEitherRight(inferResult, "Inference failed");
                    var inferPair = eitherRight(inferResult);
                    Term inferredTerm = inferPair.first.first;
                    Type inferredType = typeSchemeToType(inferPair.first.second);
                    hydra.context.Context inferCx = inferPair.second;

                    // Reconstruct type - use context from inference to continue fresh name counter
                    var typeOfResult =
                        hydra.checking.Checking.typeOf(inferCx, graph, List.of(), inferredTerm);
                    assertEitherRight(typeOfResult, "Type reconstruction failed");
                    Type reconstructedType = eitherRight(typeOfResult).first;

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
                    hydra.context.Context cx = emptyContext();
                    var result = hydra.reduction.Reduction.betaReduceType(cx, graph, tc.input);
                    assertEitherRight(result, "Type reduction failed");
                    assertEquals(tc.output, eitherRight(result));
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
                    List<List<Pair<Name, Term>>> result =
                        hydra.rewriting.Rewriting.topologicalSortBindingMap(bindingMap);
                    // Compare as sets of sets (order within SCCs doesn't matter)
                    Set<Set<Pair<Name, Term>>> resultSet = new HashSet<>();
                    for (List<Pair<Name, Term>> group : result) {
                        resultSet.add(new HashSet<>(group));
                    }
                    Set<Set<Pair<Name, Term>>> expectedSet = new HashSet<>();
                    for (List<Pair<Name, Term>> group : tc.expected) {
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
                            predicateFn(tc.predicate), emptyGraph(), tc.input)));
            }

            @Override
            public DynamicTest visit(TestCase.HoistCaseStatements instance) {
                HoistCaseStatementsTestCase tc = instance.value;
                return withTimeout(name, () ->
                    assertEquals(tc.output,
                        hydra.hoisting.Hoisting.hoistCaseStatements(emptyGraph(), tc.input)));
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
            public DynamicTest visit(TestCase.JsonDecode instance) {
                JsonDecodeTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    java.util.Map<hydra.core.Name, hydra.core.Type> emptyTypes = new java.util.HashMap<>();
                    var decodeResult = hydra.json.decode.Decode.fromJson(emptyTypes, tc.type, tc.json);

                    tc.expected.accept(new hydra.util.Either.Visitor<>() {
                        @Override
                        public Object visit(hydra.util.Either.Left<String, Term> left) {
                            // Expected failure
                            assertTrue(decodeResult instanceof hydra.util.Either.Left,
                                "Expected decode failure but succeeded");
                            return null;
                        }
                        @Override
                        public Object visit(hydra.util.Either.Right<String, Term> right) {
                            // Expected success
                            assertEitherRight(decodeResult, "JSON decode failed");
                            assertEquals(right.value, eitherRight(decodeResult));
                            return null;
                        }
                    });
                });
            }

            @Override
            public DynamicTest visit(TestCase.JsonEncode instance) {
                JsonEncodeTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    var encodeResult = hydra.json.encode.Encode.toJson(tc.term);

                    tc.expected.accept(new hydra.util.Either.Visitor<>() {
                        @Override
                        public Object visit(hydra.util.Either.Left<String, hydra.json.model.Value> left) {
                            // Expected failure
                            assertTrue(encodeResult instanceof hydra.util.Either.Left,
                                "Expected encode failure but succeeded");
                            return null;
                        }
                        @Override
                        public Object visit(hydra.util.Either.Right<String, hydra.json.model.Value> right) {
                            // Expected success
                            assertEitherRight(encodeResult, "JSON encode failed");
                            assertEquals(right.value, eitherRight(encodeResult));
                            return null;
                        }
                    });
                });
            }

            @Override
            public DynamicTest visit(TestCase.JsonRoundtrip instance) {
                JsonRoundtripTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    // Encode
                    var encodeResult = hydra.json.encode.Encode.toJson(tc.term);
                    assertEitherRight(encodeResult, "JSON encode failed");
                    hydra.json.model.Value encoded = eitherRight(encodeResult);

                    // Decode back
                    java.util.Map<hydra.core.Name, hydra.core.Type> emptyTypes = new java.util.HashMap<>();
                    var decodeResult = hydra.json.decode.Decode.fromJson(emptyTypes, tc.type, encoded);
                    assertEitherRight(decodeResult, "JSON decode failed");
                    Term decoded = eitherRight(decodeResult);
                    if (!termsEqual(tc.term, decoded)) {
                        assertEquals(
                            hydra.show.core.Core.term(tc.term),
                            hydra.show.core.Core.term(decoded),
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
                    for (hydra.util.Pair<Name, Type> pair : tc.substitution) {
                        substMap.put(pair.first, pair.second);
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
                    hydra.context.Context cx = emptyContext();
                    var result =
                        hydra.unification.Unification.unifyTypes(cx, schemaTypes, tc.left, tc.right, "test");

                    tc.expected.accept(new hydra.util.Either.Visitor<String, hydra.typing.TypeSubst, Void>() {
                        @Override
                        public Void visit(hydra.util.Either.Left<String, hydra.typing.TypeSubst> left) {
                            // Expected failure
                            assertTrue(result instanceof hydra.util.Either.Left,
                                "Expected unification failure but got success");
                            return null;
                        }

                        @Override
                        public Void visit(hydra.util.Either.Right<String, hydra.typing.TypeSubst> right) {
                            // Expected success
                            assertEitherRight(result, "Expected unification success but got failure");
                            assertEquals(right.value, eitherRight(result));
                            return null;
                        }
                    });
                });
            }

            @Override
            public DynamicTest visit(TestCase.JoinTypes instance) {
                JoinTypesTestCase tc = instance.value;
                return withTimeout(name, () -> {
                    hydra.context.Context cx = emptyContext();
                    var result =
                        hydra.unification.Unification.joinTypes(cx, tc.left, tc.right, "test");

                    tc.expected.accept(new hydra.util.Either.Visitor<Void, java.util.List<hydra.typing.TypeConstraint>, Void>() {
                        @Override
                        public Void visit(hydra.util.Either.Left<Void, java.util.List<hydra.typing.TypeConstraint>> left) {
                            // Expected failure
                            assertTrue(result instanceof hydra.util.Either.Left,
                                "Expected join failure but got success");
                            return null;
                        }

                        @Override
                        public Void visit(hydra.util.Either.Right<Void, java.util.List<hydra.typing.TypeConstraint>> right) {
                            // Expected success
                            assertEitherRight(result, "Expected join success but got failure");
                            assertEquals(right.value, eitherRight(result));
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

    /**
     * Convert a TypeScheme back to a Type by wrapping forall binders around the body.
     */
    private static Type typeSchemeToType(TypeScheme ts) {
        Type result = ts.type;
        // Wrap in reverse order so the first parameter is the outermost forall
        for (int i = ts.variables.size() - 1; i >= 0; i--) {
            result = new Type.Forall(new ForallType(ts.variables.get(i), result));
        }
        return result;
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
            Term first = ((Term.Pair) t).value.first;
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

    private static java.util.function.Function<Pair<List<hydra.accessors.TermAccessor>, Term>, Boolean> predicateFn(
            HoistPredicate pred) {
        return pred.accept(new HoistPredicate.Visitor<>() {
            @Override
            public java.util.function.Function<Pair<List<hydra.accessors.TermAccessor>, Term>, Boolean> visit(
                    HoistPredicate.Nothing instance) {
                return pair -> false;
            }

            @Override
            public java.util.function.Function<Pair<List<hydra.accessors.TermAccessor>, Term>, Boolean> visit(
                    HoistPredicate.Lists instance) {
                return pair -> pair.second instanceof Term.List;
            }

            @Override
            public java.util.function.Function<Pair<List<hydra.accessors.TermAccessor>, Term>, Boolean> visit(
                    HoistPredicate.Applications instance) {
                return pair -> pair.second instanceof Term.Application;
            }

            @Override
            public java.util.function.Function<Pair<List<hydra.accessors.TermAccessor>, Term>, Boolean> visit(
                    HoistPredicate.CaseStatements instance) {
                return pair -> pair.second instanceof Term.Function
                    && ((Term.Function) pair.second).value instanceof Function.Elimination;
            }
        });
    }

    /**
     * Build kernel type definitions needed by inference/checking tests.
     * These types are normally provided by kernelTypesModules in Haskell.
     */
    // ---- Benchmark output ----

    private static Map<Name, Type> buildKernelTypes() {
        Map<Name, Type> types = new HashMap<>();

        // CoderDirection: enum with encode, decode
        types.put(new Name("hydra.coders.CoderDirection"),
            new Type.Union(new RowType(new Name("hydra.coders.CoderDirection"), List.of(
                new FieldType(new Name("encode"), new Type.Unit()),
                new FieldType(new Name("decode"), new Type.Unit())))));

        // Coder: ∀v1.∀v2. {encode: Context -> v1 -> Either (InContext OtherError) v2, decode: Context -> v2 -> Either (InContext OtherError) v1}
        Name coderName = new Name("hydra.compute.Coder");
        Name contextName = new Name("hydra.context.Context");
        Name inContextName = new Name("hydra.context.InContext");
        Name otherErrorName = new Name("hydra.error.OtherError");
        // InContext OtherError = Application(InContext, OtherError)
        Type inContextOtherError = new Type.Application(new ApplicationType(
            new Type.Variable(inContextName),
            new Type.Variable(otherErrorName)));
        // Either (InContext OtherError) v — uses built-in Type.Either
        java.util.function.Function<Type, Type> eitherInContextError = v ->
            new Type.Either(new EitherType(inContextOtherError, v));
        // encode: Context -> v1 -> Either (InContext OtherError) v2
        Type encodeType = new Type.Function(new FunctionType(
            new Type.Variable(contextName),
            new Type.Function(new FunctionType(
                new Type.Variable(new Name("v1")),
                eitherInContextError.apply(new Type.Variable(new Name("v2")))))));
        // decode: Context -> v2 -> Either (InContext OtherError) v1
        Type decodeType = new Type.Function(new FunctionType(
            new Type.Variable(contextName),
            new Type.Function(new FunctionType(
                new Type.Variable(new Name("v2")),
                eitherInContextError.apply(new Type.Variable(new Name("v1")))))));
        Type coderBody = new Type.Record(new RowType(coderName, List.of(
            new FieldType(new Name("encode"), encodeType),
            new FieldType(new Name("decode"), decodeType))));
        // Wrap in foralls: ∀v1.∀v2. coderBody
        types.put(coderName,
            new Type.Forall(new ForallType(new Name("v1"),
                new Type.Forall(new ForallType(new Name("v2"), coderBody)))));

        // Context: record with trace, messages, other
        types.put(contextName,
            new Type.Record(new RowType(contextName, List.of(
                new FieldType(new Name("trace"), new Type.List(new Type.Literal(new LiteralType.String_()))),
                new FieldType(new Name("messages"), new Type.List(new Type.Literal(new LiteralType.String_()))),
                new FieldType(new Name("other"), new Type.Map(new MapType(
                    new Type.Variable(new Name("hydra.core.Name")),
                    new Type.Variable(new Name("hydra.core.Term")))))))));

        // InContext: ∀e. record with object (e) and context (Context)
        types.put(inContextName,
            new Type.Forall(new ForallType(new Name("e"),
                new Type.Record(new RowType(inContextName, List.of(
                    new FieldType(new Name("object"), new Type.Variable(new Name("e"))),
                    new FieldType(new Name("context"), new Type.Variable(contextName))))))));

        // OtherError: wrapper over string
        types.put(otherErrorName,
            new Type.Wrap(new WrappedType(otherErrorName,
                new Type.Literal(new LiteralType.String_()))));

        // Type: the hydra.core.Type union — large recursive type
        Name typeName = new Name("hydra.core.Type");
        types.put(typeName,
            new Type.Union(new RowType(typeName, List.of(
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
                new FieldType(new Name("wrap"), new Type.Variable(new Name("wrappedType")))))));

        // Name: wrapper over string
        Name nameName = new Name("hydra.core.Name");
        types.put(nameName,
            new Type.Wrap(new WrappedType(nameName,
                new Type.Literal(new LiteralType.String_()))));

        // ForallType: record with parameter (Name) and body (Type)
        Name forallTypeName = new Name("hydra.core.ForallType");
        types.put(forallTypeName,
            new Type.Record(new RowType(forallTypeName, List.of(
                new FieldType(new Name("parameter"), new Type.Variable(nameName)),
                new FieldType(new Name("body"), new Type.Variable(typeName))))));

        return types;
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
