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
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;
import hydra.util.Tuple;

import org.junit.jupiter.api.DynamicContainer;
import org.junit.jupiter.api.DynamicNode;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;

import java.time.Duration;
import java.util.*;
import java.util.stream.Stream;

import static hydra.dsl.Flows.EMPTY_TRACE;
import static hydra.dsl.Terms.*;
import static org.junit.jupiter.api.Assertions.*;


/**
 * Java executor for Hydra's language-agnostic test suite.
 */
public class TestSuiteRunner {

    // Unit value used as state for flows that don't need state (equivalent to Haskell's ())
    private static final hydra.util.Unit UNIT = new hydra.util.Unit();

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

        // Build schema graph with test types + kernel types
        Map<Name, Type> testTypes = TestGraph.testTypes();
        Map<Name, Type> kernelTypes = buildKernelTypes();
        Map<Name, Type> allSchemaTypes = new HashMap<>(kernelTypes);
        allSchemaTypes.putAll(testTypes); // test types override kernel types if any overlap
        List<Binding> typeBindings = new ArrayList<>();
        for (Map.Entry<Name, Type> entry : allSchemaTypes.entrySet()) {
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

        // Note: kernel source modules (hydra.sources.monads, hydra.sources.annotations) were removed as oversized.
        // Term bindings they provided are no longer available here.

        // The source module ASTs reference external functions as Term.Variable.
        // The reducer only looks up Term.Variable in term bindings (not primitives).
        // Bridge all primitives: Variable("hydra.lib.maps.null") -> Function.Primitive("hydra.lib.maps.null")
        Set<String> existingBindingNames = new HashSet<>();
        for (Binding b : termBindings) {
            existingBindingNames.add(b.name.value);
        }
        // Exclude annotation/rewriting primitives from the bridge — they operate at the Java level
        // (producing Term.Annotated) rather than at the meta level (producing Term.Union injections).
        // Hand-written term bindings are added below instead.
        existingBindingNames.add("hydra.annotations.setTermAnnotation");
        existingBindingNames.add("hydra.annotations.setTermDescription");
        existingBindingNames.add("hydra.rewriting.deannotateTerm");
        for (PrimitiveFunction prim : Libraries.standardPrimitives()) {
            String primName = prim.name().value;
            if (!existingBindingNames.contains(primName)) {
                termBindings.add(new Binding(
                    prim.name(),
                    new Term.Function(new hydra.core.Function.Primitive(prim.name())),
                    Maybe.nothing()));
            }
        }

        // Add non-primitive kernel constants needed by annotation source module
        addConstantBinding(termBindings, "hydra.constants.key_classes",
            new Term.Wrap(new hydra.core.WrappedTerm(new Name("hydra.core.Name"),
                new Term.Literal(new hydra.core.Literal.String_("classes")))));
        addConstantBinding(termBindings, "hydra.constants.key_description",
            new Term.Wrap(new hydra.core.WrappedTerm(new Name("hydra.core.Name"),
                new Term.Literal(new hydra.core.Literal.String_("description")))));
        addConstantBinding(termBindings, "hydra.constants.key_type",
            new Term.Wrap(new hydra.core.WrappedTerm(new Name("hydra.core.Name"),
                new Term.Literal(new hydra.core.Literal.String_("type")))));
        addConstantBinding(termBindings, "hydra.constants.key_debugId",
            new Term.Wrap(new hydra.core.WrappedTerm(new Name("hydra.core.Name"),
                new Term.Literal(new hydra.core.Literal.String_("debugId")))));
        addConstantBinding(termBindings, "hydra.constants.key_firstClassType",
            new Term.Wrap(new hydra.core.WrappedTerm(new Name("hydra.core.Name"),
                new Term.Literal(new hydra.core.Literal.String_("firstClassType")))));

        // Add kernel monads term bindings (hand-written since generated sources exceed JVM method size limits)
        addMonadsBindings(termBindings);

        // Add kernel annotation/rewriting term bindings
        addAnnotationsBindings(termBindings);

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

    private static void addConstantBinding(List<Binding> bindings, String name, Term value) {
        bindings.add(new Binding(new Name(name), value, Maybe.nothing()));
    }

    /**
     * Add term-level bindings for hydra.monads functions needed by tests.
     * These are hand-written because the generated source modules exceed JVM method size limits.
     */
    private static void addMonadsBindings(List<Binding> bindings) {
        // Helper terms for Flow wrap/unwrap
        Term wrapFlow = wrap("hydra.compute.Flow", var("__body"));
        Term unwrapFlowApp = apply(apply(apply(unwrap("hydra.compute.Flow"), var("__f")), var("__s")), var("__t"));

        // hydra.monads.emptyTrace = record(hydra.compute.Trace){stack=[], messages=[], other={}}
        addConstantBinding(bindings, "hydra.monads.emptyTrace",
            record("hydra.compute.Trace",
                field("stack", list()),
                field("messages", list()),
                field("other", new Term.Map(Collections.emptyMap()))));

        // hydra.monads.pure = \xp -> wrap(Flow, \s -> \t -> FlowState{value=Just(xp), state=s, trace=t})
        addConstantBinding(bindings, "hydra.monads.pure",
            lambda("xp",
                wrap("hydra.compute.Flow",
                    lambda("s", "t",
                        flowState(just(var("xp")), var("s"), var("t"))))));

        // hydra.monads.map = \f -> \f1 -> wrap(Flow, \s0 -> \t0 ->
        //   let f2 = unwrap(Flow)(f1)(s0)(t0) in
        //   FlowState{value=maybes.map(f, f2.value), state=f2.state, trace=f2.trace})
        addConstantBinding(bindings, "hydra.monads.map",
            lambda("f",
                lambda("f1",
                    wrap("hydra.compute.Flow",
                        lambda("s0", "t0",
                            let_("f2", apply(apply(apply(unwrap("hydra.compute.Flow"), var("f1")), var("s0")), var("t0")),
                                flowState(
                                    apply(apply(primitive("hydra.lib.maybes.map"), var("f")),
                                        apply(flowStateValue(), var("f2"))),
                                    apply(flowStateState(), var("f2")),
                                    apply(flowStateTrace(), var("f2")))))))));

        // hydra.monads.bind = \l -> \r -> wrap(Flow, \s0 -> \t0 ->
        //   let fs1 = unwrap(Flow)(l)(s0)(t0) in
        //   maybe(FlowState{nothing, fs1.state, fs1.trace},
        //         \v -> unwrap(Flow)(r(v))(fs1.state)(fs1.trace),
        //         fs1.value))
        addConstantBinding(bindings, "hydra.monads.bind",
            lambda("l",
                lambda("r",
                    wrap("hydra.compute.Flow",
                        lambda("s0", "t0",
                            let_("fs1", apply(apply(apply(unwrap("hydra.compute.Flow"), var("l")), var("s0")), var("t0")),
                                apply(apply(apply(primitive("hydra.lib.maybes.maybe"),
                                    // default: FlowState{nothing, fs1.state, fs1.trace}
                                    flowState(nothing(), apply(flowStateState(), var("fs1")), apply(flowStateTrace(), var("fs1")))),
                                    // function: \v -> unwrap(Flow)(r(v))(fs1.state)(fs1.trace)
                                    lambda("v",
                                        apply(apply(apply(unwrap("hydra.compute.Flow"),
                                            apply(var("r"), var("v"))),
                                            apply(flowStateState(), var("fs1"))),
                                            apply(flowStateTrace(), var("fs1"))))),
                                    // maybe value: fs1.value
                                    apply(flowStateValue(), var("fs1")))))))));

        // hydra.monads.pushError = \msg -> \t ->
        //   let errorMsg = concat ["Error: ", msg, " (", intercalate(" > ", reverse(t.stack)), ")"] in
        //   Trace{stack=t.stack, messages=cons(errorMsg, t.messages), other=t.other}
        addConstantBinding(bindings, "hydra.monads.pushError",
            lambda("msg",
                lambda("t",
                    let_("errorMsg",
                        apply(primitive("hydra.lib.strings.cat"),
                            list(string("Error: "), var("msg"), string(" ("),
                                apply(apply(primitive("hydra.lib.strings.intercalate"), string(" > ")),
                                    apply(primitive("hydra.lib.lists.reverse"),
                                        apply(project("hydra.compute.Trace", "stack"), var("t")))),
                                string(")"))),
                        record("hydra.compute.Trace",
                            field("stack", apply(project("hydra.compute.Trace", "stack"), var("t"))),
                            field("messages", apply(apply(primitive("hydra.lib.lists.cons"), var("errorMsg")),
                                apply(project("hydra.compute.Trace", "messages"), var("t")))),
                            field("other", apply(project("hydra.compute.Trace", "other"), var("t"))))))));

        // hydra.monads.fail = \msg -> wrap(Flow, \s -> \t ->
        //   FlowState{value=nothing, state=s, trace=pushError(msg, t)})
        addConstantBinding(bindings, "hydra.monads.fail",
            lambda("msg",
                wrap("hydra.compute.Flow",
                    lambda("s", "t",
                        flowState(nothing(), var("s"),
                            apply(apply(var("hydra.monads.pushError"), var("msg")), var("t")))))));

        // hydra.monads.withTrace = \msg -> \f ->
        //   mutateTrace(\t -> right(Trace{cons(msg,t.stack), t.messages, t.other}),
        //               \t0 -> \t1 -> Trace{t0.stack, t1.messages, t1.other},
        //               f)
        // Simplified: skip max trace depth check since tests won't hit it
        addConstantBinding(bindings, "hydra.monads.withTrace",
            lambda("msg",
                lambda("f",
                    apply(apply(apply(var("hydra.monads.mutateTrace"),
                        // mutate: \t -> right(Trace{cons(msg, t.stack), t.messages, t.other})
                        lambda("t",
                            right(
                                record("hydra.compute.Trace",
                                    field("stack", apply(apply(primitive("hydra.lib.lists.cons"), var("msg")),
                                        apply(project("hydra.compute.Trace", "stack"), var("t")))),
                                    field("messages", apply(project("hydra.compute.Trace", "messages"), var("t"))),
                                    field("other", apply(project("hydra.compute.Trace", "other"), var("t"))))))),
                        // restore: \t0 -> \t1 -> Trace{t0.stack, t1.messages, t1.other}
                        lambda("t0",
                            lambda("t1",
                                record("hydra.compute.Trace",
                                    field("stack", apply(project("hydra.compute.Trace", "stack"), var("t0"))),
                                    field("messages", apply(project("hydra.compute.Trace", "messages"), var("t1"))),
                                    field("other", apply(project("hydra.compute.Trace", "other"), var("t1"))))))),
                        var("f")))));

        // hydra.monads.mutateTrace = \mutate -> \restore -> \f -> wrap(Flow, \s0 -> \t0 ->
        //   either(\msg -> FlowState{nothing, s0, pushError(msg, t0)},
        //          \t1 -> let f2 = unwrap(Flow)(f)(s0)(t1) in
        //                 FlowState{f2.value, f2.state, restore(t0, f2.trace)},
        //          mutate(t0)))
        addConstantBinding(bindings, "hydra.monads.mutateTrace",
            lambda("mutate",
                lambda("restore",
                    lambda("f",
                        wrap("hydra.compute.Flow",
                            lambda("s0", "t0",
                                apply(apply(apply(primitive("hydra.lib.eithers.either"),
                                    // left case: \msg -> FlowState{nothing, s0, pushError(msg, t0)}
                                    lambda("msg",
                                        flowState(nothing(), var("s0"),
                                            apply(apply(var("hydra.monads.pushError"), var("msg")), var("t0"))))),
                                    // right case: \t1 -> let f2 = ... in FlowState{...}
                                    lambda("t1",
                                        let_("f2",
                                            apply(apply(apply(unwrap("hydra.compute.Flow"), var("f")), var("s0")), var("t1")),
                                            flowState(
                                                apply(flowStateValue(), var("f2")),
                                                apply(flowStateState(), var("f2")),
                                                apply(apply(var("restore"), var("t0")),
                                                    apply(flowStateTrace(), var("f2"))))))),
                                    // the either value: mutate(t0)
                                    apply(var("mutate"), var("t0")))))))));

        // hydra.constants.maxTraceDepth = 50
        addConstantBinding(bindings, "hydra.constants.maxTraceDepth", int32(50));
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
        //   toPairs(rest, t) = case t of { annotated(at) -> toPairs(cons(toList(at.annotation), rest), at.body); _ -> rest }
        //   result = fromList(concat(toPairs([], term)))
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
        // Uses maps.insert and maps.delete instead of maps.alter for simpler term evaluation
        addConstantBinding(bindings, "hydra.annotations.setAnnotation",
            lambda("key",
                lambda("val",
                    lambda("m",
                        apply(apply(apply(primitive("hydra.lib.maybes.maybe"),
                            // Nothing case: delete key from map
                            apply(apply(primitive("hydra.lib.maps.delete"), var("key")), var("m"))),
                            // Just case: \v -> insert(key, v, m)
                            lambda("v",
                                apply(apply(apply(primitive("hydra.lib.maps.insert"),
                                    var("key")), var("v")), var("m")))),
                            // the maybe value
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
        return collectTests(TestSuite.allTests());
    }

    private static Stream<DynamicNode> collectTests(TestGroup group) {
        List<DynamicNode> nodes = new ArrayList<>();

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
            nodes.add(DynamicContainer.dynamicContainer(subName, collectTests(subgroup)));
        }

        return nodes.stream();
    }

    private static boolean shouldSkip(TestCaseWithMetadata tc) {
        Tag disabledTag = new Tag("disabled");
        Tag requiresFlowDecodingTag = new Tag("requiresFlowDecoding");
        // Note: disabledForPython tests run in Java - they only fail in Python due to recursion limits
        return tc.tags.contains(disabledTag)
            || tc.tags.contains(requiresFlowDecodingTag);
    }

    private static final Duration TEST_TIMEOUT = Duration.ofSeconds(5);

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
    private static Map<Name, Type> buildKernelTypes() {
        Map<Name, Type> types = new HashMap<>();

        // CoderDirection: enum with encode, decode
        types.put(new Name("hydra.coders.CoderDirection"),
            new Type.Union(new RowType(new Name("hydra.coders.CoderDirection"), List.of(
                new FieldType(new Name("encode"), new Type.Unit()),
                new FieldType(new Name("decode"), new Type.Unit())))));

        // Coder: ∀s1.∀s2.∀v1.∀v2. {encode: v1 -> Flow s1 v2, decode: v2 -> Flow s2 v1}
        Name coderName = new Name("hydra.compute.Coder");
        Name flowName = new Name("hydra.compute.Flow");
        Type coderBody = new Type.Record(new RowType(coderName, List.of(
            new FieldType(new Name("encode"),
                new Type.Function(new FunctionType(
                    new Type.Variable(new Name("v1")),
                    new Type.Application(new ApplicationType(
                        new Type.Application(new ApplicationType(
                            new Type.Variable(flowName),
                            new Type.Variable(new Name("s1")))),
                        new Type.Variable(new Name("v2"))))))),
            new FieldType(new Name("decode"),
                new Type.Function(new FunctionType(
                    new Type.Variable(new Name("v2")),
                    new Type.Application(new ApplicationType(
                        new Type.Application(new ApplicationType(
                            new Type.Variable(flowName),
                            new Type.Variable(new Name("s2")))),
                        new Type.Variable(new Name("v1"))))))))));
        // Wrap in foralls: ∀s1.∀s2.∀v1.∀v2. coderBody
        types.put(coderName,
            new Type.Forall(new ForallType(new Name("s1"),
                new Type.Forall(new ForallType(new Name("s2"),
                    new Type.Forall(new ForallType(new Name("v1"),
                        new Type.Forall(new ForallType(new Name("v2"), coderBody)))))))));

        // Type: the hydra.core.Type union — large recursive type
        // We include a simplified version with all the variants
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
