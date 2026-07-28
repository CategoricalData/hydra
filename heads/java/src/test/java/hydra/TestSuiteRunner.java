package hydra;

import hydra.core.*;
import hydra.graph.Graph;
import hydra.graph.Primitive;
import hydra.test.TestSuite;
import hydra.test.TestGraph;
import hydra.testing.*;
import hydra.overlay.java.lib.Libraries;
import hydra.overlay.java.tools.PrimitiveFunction;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Optional;

import org.junit.jupiter.api.DynamicNode;
import org.junit.jupiter.api.TestFactory;

import java.util.*;
import java.util.stream.Stream;

import static hydra.overlay.java.dsl.Terms.*;


/**
 * Java executor for Hydra's language-agnostic test suite.
 *
 * Handles both UniversalTestCase instances (pure string comparison) and
 * EffectfulTestCase instances (interpret an effect, e.g. file I/O, then string comparison).
 * Legacy per-type handlers have been removed. The generic TestGroup walker (test-case
 * dispatch, skip tags, timeouts, benchmark output) lives in HydraTestGroupWalker,
 * shared with hydra-build's BuildTestSuiteRunner (#547).
 */
public class TestSuiteRunner {

    // Benchmark output support
    private static final String BENCHMARK_OUTPUT = System.getenv("HYDRA_BENCHMARK_OUTPUT");

    // When true, use primitiveDefinitionDefaultImplementation instead of native implementations.
    // Activated via -Dhydra.defaultImpls=true (Gradle: -PhydraDefaultImpls) or HYDRA_DEFAULT_IMPLS=1 env var.
    private static final boolean USE_DEFAULT_IMPLS =
        "true".equals(System.getProperty("hydra.defaultImpls"))
        || "1".equals(System.getenv("HYDRA_DEFAULT_IMPLS"));

    // Cached test infrastructure
    private static Graph testGraph;

    /**
     * Returns the cached, USE_DEFAULT_IMPLS-aware test graph, building it on first call.
     * This is public because hydra.test.TestEnv delegates to it.
     */
    public static synchronized Graph getTestGraph() {
        if (testGraph == null) {
            testGraph = buildTestGraph(USE_DEFAULT_IMPLS);
        }
        return testGraph;
    }

    private static hydra.typing.InferenceContext emptyContext() {
        return new hydra.typing.InferenceContext(0, new java.util.ArrayList<>());
    }

    /**
     * Patch a primitives map so that each primitive with a defaultImplementation
     * uses reduceTerm on that term instead of the native host implementation.
     */
    private static Map<Name, Primitive> patchWithDefaultImpls(
            Map<Name, Primitive> primitives, Graph nativeGraph) {
        Map<Name, Primitive> patched = new HashMap<>();
        for (Map.Entry<Name, Primitive> entry : primitives.entrySet()) {
            Primitive prim = entry.getValue();
            hydra.overlay.java.util.Optional<hydra.core.Term> defImpl = prim.definition.defaultImplementation;
            if (defImpl.isGiven()) {
                hydra.core.Term implTerm = defImpl.fromGiven();
                patched.put(entry.getKey(), prim.withImplementation(g -> args -> {
                    hydra.core.Term applied = implTerm;
                    for (hydra.core.Term arg : args) {
                        applied = new hydra.core.Term.Application(new hydra.core.Application(applied, arg));
                    }
                    return hydra.Reduction.reduceTerm(emptyContext(), nativeGraph, true, applied);
                }));
            } else {
                patched.put(entry.getKey(), prim);
            }
        }
        return patched;
    }

    /**
     * Build the test graph with schema, test data, and primitives, always using
     * native implementations. Mirrors the Haskell testGraph in TestUtils.hs.
     */
    public static Graph buildTestGraph() {
        return buildTestGraph(false);
    }

    /**
     * Build the test graph. When useDefaultImpls is true, primitives with a
     * defaultImplementation use it (via reduceTerm) instead of the native implementation.
     */
    public static Graph buildTestGraph(boolean useDefaultImpls) {
        // Build primitives map
        hydra.overlay.java.util.PersistentMap<Name, Primitive> primitives = hydra.overlay.java.util.PersistentMap.empty();
        for (PrimitiveFunction prim : Libraries.standardPrimitives()) {
            primitives = primitives.insert(prim.name(), prim.toNative());
        }

        // Build schema types from test types + kernel types
        Map<Name, Type> testTypes = TestGraph.testTypes();
        Map<Name, Type> kernelTypes = buildKernelTypes();
        Map<Name, Type> allTypes = new HashMap<>(kernelTypes);
        allTypes.putAll(testTypes); // test types override kernel types if any overlap
        hydra.overlay.java.util.PersistentMap<Name, TypeScheme> schemaTypes = hydra.overlay.java.util.PersistentMap.empty();
        for (Map.Entry<Name, Type> entry : allTypes.entrySet()) {
            schemaTypes = schemaTypes.insert(entry.getKey(), hydra.Resolution.typeToTypeScheme(entry.getValue()));
        }

        // Build bound terms map from test terms + primitive bridges + kernel constants
        Map<Name, Term> boundTerms = new HashMap<>();

        // Primitives are resolved via graphPrimitives, not boundTerms.
        // No need to bridge them as term bindings.

        // Add non-primitive kernel constants needed by annotation source module
        boundTerms.put(new Name("hydra.constants.keyClasses"),
            new Term.Wrap(new hydra.core.WrappedTerm(new Name("hydra.core.Name"),
                new Term.Literal(new hydra.core.Literal.String_("classes")))));
        boundTerms.put(new Name("hydra.constants.keyDescription"),
            new Term.Wrap(new hydra.core.WrappedTerm(new Name("hydra.core.Name"),
                new Term.Literal(new hydra.core.Literal.String_("description")))));
        boundTerms.put(new Name("hydra.constants.keyType"),
            new Term.Wrap(new hydra.core.WrappedTerm(new Name("hydra.core.Name"),
                new Term.Literal(new hydra.core.Literal.String_("type")))));
        boundTerms.put(new Name("hydra.constants.keyDebugId"),
            new Term.Wrap(new hydra.core.WrappedTerm(new Name("hydra.core.Name"),
                new Term.Literal(new hydra.core.Literal.String_("debugId")))));
        boundTerms.put(new Name("hydra.constants.keyFirstClassType"),
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

        hydra.overlay.java.util.PersistentMap<Name, Term> persistentBoundTerms = hydra.overlay.java.util.PersistentMap.empty();
        for (Map.Entry<Name, Term> entry : boundTerms.entrySet()) {
            persistentBoundTerms = persistentBoundTerms.insert(entry.getKey(), entry.getValue());
        }

        Graph nativeGraph = new Graph(
            persistentBoundTerms,
            hydra.overlay.java.util.PersistentMap.empty(), // boundTypes (TypeSchemes for term bindings — not populated for test graph)
            hydra.overlay.java.util.PersistentMap.empty(), // classConstraints
            hydra.overlay.java.util.PersistentSet.empty(), // lambdaVariables
            hydra.overlay.java.util.PersistentMap.empty(), // metadata
            primitives,
            schemaTypes,
            hydra.overlay.java.util.PersistentSet.empty()  // typeVariables
        );

        if (!useDefaultImpls) return nativeGraph;

        return nativeGraph.withPrimitives(patchWithDefaultImpls(nativeGraph.primitives, nativeGraph));
    }

    private static void addConstantBinding(List<Binding> bindings, String name, Term value) {
        bindings.add(new Binding(new Name(name), value, Optional.none()));
    }

    /**
     * Add term-level bindings for graph constants needed by tests.
     * These are hand-written because the generated source modules exceed JVM method size limits.
     */
    private static void addConstantBindings(List<Binding> bindings) {
        addConstantBinding(bindings, "hydra.lexical.emptyGraph",
            record("hydra.graph.Graph",
                field("boundTerms", new Term.Map(hydra.overlay.java.util.PersistentMap.empty())),
                field("boundTypes", new Term.Map(hydra.overlay.java.util.PersistentMap.empty())),
                field("classConstraints", new Term.Map(hydra.overlay.java.util.PersistentMap.empty())),
                field("lambdaVariables", new Term.Set(hydra.overlay.java.util.PersistentSet.empty())),
                field("metadata", new Term.Map(hydra.overlay.java.util.PersistentMap.empty())),
                field("primitives", new Term.Map(hydra.overlay.java.util.PersistentMap.empty())),
                field("schemaTypes", new Term.Map(hydra.overlay.java.util.PersistentMap.empty())),
                field("typeVariables", new Term.Set(hydra.overlay.java.util.PersistentSet.empty()))));
    }

    /**
     * Add term-level bindings for annotation and rewriting functions needed by tests.
     * These are hand-written because the generated source modules exceed JVM method size limits.
     */
    private static void addAnnotationsBindings(List<Binding> bindings) {
        // hydra.annotations.getAnnotationMap (#386):
        //   getAnnotationMap :: Term -> Map<Name, Term>
        //   Project the (Name, value) entries from a TermMap-with-TermVariable-keys
        //   annotation; return Maps.empty for any other shape.
        addConstantBinding(bindings, "hydra.annotations.getAnnotationMap",
            lambda("t",
                apply(
                    match("hydra.core.Term", Optional.given(apply(primitive("hydra.lib.maps.empty"), var("t"))),
                        field("map", lambda("m",
                            apply(primitive("hydra.lib.maps.fromList"),
                                apply(apply(primitive("hydra.lib.lists.foldl"),
                                    lambda("acc", "pair",
                                        apply(
                                            match("hydra.core.Term",
                                                Optional.given(var("acc")),
                                                field("variable", lambda("n",
                                                    apply(apply(primitive("hydra.lib.lists.cons"),
                                                        pair(
                                                            var("n"),
                                                            apply(primitive("hydra.lib.pairs.second"), var("pair")))),
                                                        var("acc"))))),
                                            apply(primitive("hydra.lib.pairs.first"), var("pair")))),
                                    list()),
                                    apply(primitive("hydra.lib.maps.toList"), var("m"))))))),
                    var("t"))));

        // hydra.annotations.wrapAnnotationMap (#386):
        //   wrapAnnotationMap :: Map<Name, Term> -> Term
        //   Encode each Name key as a TermVariable, then wrap as a TermMap.
        addConstantBinding(bindings, "hydra.annotations.wrapAnnotationMap",
            lambda("m",
                inject("hydra.core.Term", field("map",
                    apply(primitive("hydra.lib.maps.fromList"),
                        apply(apply(primitive("hydra.lib.lists.map"),
                            lambda("pair",
                                pair(
                                    inject("hydra.core.Term", field("variable",
                                        apply(primitive("hydra.lib.pairs.first"), var("pair")))),
                                    apply(primitive("hydra.lib.pairs.second"), var("pair"))))),
                            apply(primitive("hydra.lib.maps.toList"), var("m"))))))));

        addConstantBinding(bindings, "hydra.rewriting.deannotateTerm",
            lambda("t",
                apply(
                    match("hydra.core.Term", Optional.given(var("t")),
                        field("annotated", lambda("at",
                            apply(var("hydra.rewriting.deannotateTerm"),
                                apply(project("hydra.core.AnnotatedTerm", "body"), var("at")))))),
                    var("t"))));

        // After #386: project the map payload out of the Term annotation via getAnnotationMap.
        addConstantBinding(bindings, "hydra.annotations.termAnnotationInternal",
            lambda("term",
                let_("toPairs",
                    lambda("rest", "t",
                        apply(
                            match("hydra.core.Term",
                                Optional.given(var("rest")),
                                field("annotated", lambda("at",
                                    apply(apply(var("toPairs"),
                                        apply(apply(primitive("hydra.lib.lists.cons"),
                                            apply(primitive("hydra.lib.maps.toList"),
                                                apply(var("hydra.annotations.getAnnotationMap"),
                                                    apply(project("hydra.core.AnnotatedTerm", "annotation"), var("at"))))),
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
                        apply(apply(apply(primitive("hydra.lib.optionals.cases"),
                            var("val")),
                            apply(apply(primitive("hydra.lib.maps.delete"), var("key")), var("m"))),
                            lambda("v",
                                apply(apply(apply(primitive("hydra.lib.maps.insert"),
                                    var("key")), var("v")), var("m"))))))));

        // After #386: wrap the resulting map via wrapAnnotationMap before storing in AnnotatedTerm.annotation.
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
                                            field("annotation",
                                                apply(var("hydra.annotations.wrapAnnotationMap"), var("anns"))))))))))));

        addConstantBinding(bindings, "hydra.annotations.setTermDescription",
            lambda("d",
                apply(apply(var("hydra.annotations.setTermAnnotation"),
                    var("hydra.constants.keyDescription")),
                    apply(apply(primitive("hydra.lib.optionals.map"),
                        lambda("s",
                            inject("hydra.core.Term", "literal",
                                inject("hydra.core.Literal", "string", var("s"))))),
                        var("d")))));

        addConstantBinding(bindings, "hydra.annotations.getDescription",
            lambda("cx",
                lambda("g",
                    lambda("anns",
                        apply(apply(apply(primitive("hydra.lib.optionals.cases"),
                            apply(apply(primitive("hydra.lib.maps.lookup"),
                                var("hydra.constants.keyDescription")),
                                var("anns"))),
                            right(nothing())),
                            lambda("descTerm",
                                apply(
                                    match("hydra.core.Term", Optional.given(
                                        left(inject("hydra.errors.Error", field("other", wrap("hydra.errors.OtherError", string("Expected string literal")))))),
                                        field("literal", lambda("lit",
                                            apply(
                                                match("hydra.core.Literal", Optional.given(
                                                    left(inject("hydra.errors.Error", field("other", wrap("hydra.errors.OtherError", string("Expected string literal")))))),
                                                    field("string", lambda("s", right(just(var("s")))))),
                                                var("lit"))))),
                                    var("descTerm"))))))));

        addConstantBinding(bindings, "hydra.annotations.getTermDescription",
            lambda("cx",
                lambda("g",
                    lambda("term",
                        let_("peel",
                            lambda("t",
                                apply(
                                    match("hydra.core.Term", Optional.given(var("t")),
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

        Name contextName = new Name("hydra.typing.InferenceContext");
        Name errorName = new Name("hydra.errors.Error");
        java.util.function.Function<Type, Type> eitherError = v ->
            new Type.Either(new EitherType(new Type.Variable(new Name("e")), v));

        // Coder: forall v1 v2 e. {encode: v1 -> Either e v2, decode: v2 -> Either e v1}
        Type encodeType = new Type.Function(new FunctionType(
            new Type.Variable(new Name("v1")),
            eitherError.apply(new Type.Variable(new Name("v2")))));
        Type decodeType = new Type.Function(new FunctionType(
            new Type.Variable(new Name("v2")),
            eitherError.apply(new Type.Variable(new Name("v1")))));
        Type coderBody = new Type.Record(ConsList.of(
            new FieldType(new Name("encode"), encodeType),
            new FieldType(new Name("decode"), decodeType)));
        types.put(new Name("hydra.coders.Coder"),
            new Type.Forall(new ForallType(new Name("v1"),
                new Type.Forall(new ForallType(new Name("v2"),
                    new Type.Forall(new ForallType(new Name("e"), coderBody)))))));

        // Context
        types.put(contextName,
            new Type.Record(ConsList.of(
                new FieldType(new Name("trace"), new Type.List(new Type.Literal(new LiteralType.String_()))),
                new FieldType(new Name("messages"), new Type.List(new Type.Literal(new LiteralType.String_()))),
                new FieldType(new Name("other"), new Type.Map(new MapType(
                    new Type.Variable(new Name("hydra.core.Name")),
                    new Type.Variable(new Name("hydra.core.Term"))))))));

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
                new FieldType(new Name("optional"), new Type.Variable(typeName)),
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

    @TestFactory
    Stream<DynamicNode> kernelTests() {
        TestGroup allTests = TestSuite.allTests();

        // Eagerly initialize test infrastructure and measure time.
        // This ensures startup cost is not attributed to the first test group.
        long initStart = System.nanoTime();
        getTestGraph();
        double initMs = (System.nanoTime() - initStart) / 1_000_000.0;

        return new HydraTestGroupWalker("java", BENCHMARK_OUTPUT).walk(allTests, initMs);
    }

}
