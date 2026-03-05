package hydra.dsl.meta;

import hydra.phantoms.TTerm;

import static hydra.dsl.meta.Phantoms.*;

/**
 * Meta-DSL for constructing graph-related terms (Graph, Primitive, Comparison, TypeClass).
 *
 * <p>Mirrors Hydra.Dsl.Meta.Graph (Haskell).
 */
public interface Graph {

    // ============================================================
    // Comparison
    // ============================================================

    static Expr<hydra.util.Comparison> comparisonLessThan() {
        return injectUnit(hydra.util.Comparison.TYPE_, hydra.util.Comparison.LESS_THAN);
    }

    static Expr<hydra.util.Comparison> comparisonEqualTo() {
        return injectUnit(hydra.util.Comparison.TYPE_, hydra.util.Comparison.EQUAL_TO);
    }

    static Expr<hydra.util.Comparison> comparisonGreaterThan() {
        return injectUnit(hydra.util.Comparison.TYPE_, hydra.util.Comparison.GREATER_THAN);
    }

    // ============================================================
    // Graph
    // ============================================================

    static Expr<hydra.graph.Graph> graph(
            TTerm<?> boundTerms,
            TTerm<?> boundTypes,
            TTerm<?> classConstraints,
            TTerm<?> lambdaVariables,
            TTerm<?> metadata,
            TTerm<?> primitives,
            TTerm<?> schemaTypes,
            TTerm<?> typeVariables) {
        return record(hydra.graph.Graph.TYPE_,
                field(hydra.graph.Graph.BOUND_TERMS, boundTerms),
                field(hydra.graph.Graph.BOUND_TYPES, boundTypes),
                field(hydra.graph.Graph.CLASS_CONSTRAINTS, classConstraints),
                field(hydra.graph.Graph.LAMBDA_VARIABLES, lambdaVariables),
                field(hydra.graph.Graph.METADATA, metadata),
                field(hydra.graph.Graph.PRIMITIVES, primitives),
                field(hydra.graph.Graph.SCHEMA_TYPES, schemaTypes),
                field(hydra.graph.Graph.TYPE_VARIABLES, typeVariables));
    }

    static Expr<?> graphBoundTerms(TTerm<hydra.graph.Graph> g) {
        return apply(project(hydra.graph.Graph.TYPE_, hydra.graph.Graph.BOUND_TERMS), g);
    }

    static Expr<?> graphBoundTypes(TTerm<hydra.graph.Graph> g) {
        return apply(project(hydra.graph.Graph.TYPE_, hydra.graph.Graph.BOUND_TYPES), g);
    }

    static Expr<?> graphClassConstraints(TTerm<hydra.graph.Graph> g) {
        return apply(project(hydra.graph.Graph.TYPE_, hydra.graph.Graph.CLASS_CONSTRAINTS), g);
    }

    static Expr<?> graphLambdaVariables(TTerm<hydra.graph.Graph> g) {
        return apply(project(hydra.graph.Graph.TYPE_, hydra.graph.Graph.LAMBDA_VARIABLES), g);
    }

    static Expr<?> graphMetadata(TTerm<hydra.graph.Graph> g) {
        return apply(project(hydra.graph.Graph.TYPE_, hydra.graph.Graph.METADATA), g);
    }

    static Expr<?> graphPrimitives(TTerm<hydra.graph.Graph> g) {
        return apply(project(hydra.graph.Graph.TYPE_, hydra.graph.Graph.PRIMITIVES), g);
    }

    static Expr<?> graphSchemaTypes(TTerm<hydra.graph.Graph> g) {
        return apply(project(hydra.graph.Graph.TYPE_, hydra.graph.Graph.SCHEMA_TYPES), g);
    }

    static Expr<?> graphTypeVariables(TTerm<hydra.graph.Graph> g) {
        return apply(project(hydra.graph.Graph.TYPE_, hydra.graph.Graph.TYPE_VARIABLES), g);
    }

    static Expr<hydra.graph.Graph> graphWithBoundTerms(TTerm<hydra.graph.Graph> g, TTerm<?> newBoundTerms) {
        return graph(newBoundTerms, graphBoundTypes(g), graphClassConstraints(g),
                graphLambdaVariables(g), graphMetadata(g), graphPrimitives(g), graphSchemaTypes(g), graphTypeVariables(g));
    }

    static Expr<hydra.graph.Graph> graphWithBoundTypes(TTerm<hydra.graph.Graph> g, TTerm<?> newBoundTypes) {
        return graph(graphBoundTerms(g), newBoundTypes, graphClassConstraints(g),
                graphLambdaVariables(g), graphMetadata(g), graphPrimitives(g), graphSchemaTypes(g), graphTypeVariables(g));
    }

    static Expr<hydra.graph.Graph> graphWithClassConstraints(TTerm<hydra.graph.Graph> g, TTerm<?> newClassConstraints) {
        return graph(graphBoundTerms(g), graphBoundTypes(g), newClassConstraints,
                graphLambdaVariables(g), graphMetadata(g), graphPrimitives(g), graphSchemaTypes(g), graphTypeVariables(g));
    }

    static Expr<hydra.graph.Graph> graphWithLambdaVariables(TTerm<hydra.graph.Graph> g, TTerm<?> newLambdaVariables) {
        return graph(graphBoundTerms(g), graphBoundTypes(g), graphClassConstraints(g),
                newLambdaVariables, graphMetadata(g), graphPrimitives(g), graphSchemaTypes(g), graphTypeVariables(g));
    }

    static Expr<hydra.graph.Graph> graphWithMetadata(TTerm<hydra.graph.Graph> g, TTerm<?> newMetadata) {
        return graph(graphBoundTerms(g), graphBoundTypes(g), graphClassConstraints(g),
                graphLambdaVariables(g), newMetadata, graphPrimitives(g), graphSchemaTypes(g), graphTypeVariables(g));
    }

    static Expr<hydra.graph.Graph> graphWithPrimitives(TTerm<hydra.graph.Graph> g, TTerm<?> newPrimitives) {
        return graph(graphBoundTerms(g), graphBoundTypes(g), graphClassConstraints(g),
                graphLambdaVariables(g), graphMetadata(g), newPrimitives, graphSchemaTypes(g), graphTypeVariables(g));
    }

    static Expr<hydra.graph.Graph> graphWithSchemaTypes(TTerm<hydra.graph.Graph> g, TTerm<?> newSchemaTypes) {
        return graph(graphBoundTerms(g), graphBoundTypes(g), graphClassConstraints(g),
                graphLambdaVariables(g), graphMetadata(g), graphPrimitives(g), newSchemaTypes, graphTypeVariables(g));
    }

    static Expr<hydra.graph.Graph> graphWithTypeVariables(TTerm<hydra.graph.Graph> g, TTerm<?> newTypeVariables) {
        return graph(graphBoundTerms(g), graphBoundTypes(g), graphClassConstraints(g),
                graphLambdaVariables(g), graphMetadata(g), graphPrimitives(g), graphSchemaTypes(g), newTypeVariables);
    }

    // ============================================================
    // Primitive
    // ============================================================

    static Expr<hydra.graph.Primitive> primitive(TTerm<?> name, TTerm<?> type, TTerm<?> implementation) {
        return record(hydra.graph.Primitive.TYPE_,
                field(hydra.graph.Primitive.NAME, name),
                field(hydra.graph.Primitive.TYPE, type),
                field(hydra.graph.Primitive.IMPLEMENTATION, implementation));
    }

    static Expr<?> primitiveName(TTerm<hydra.graph.Primitive> p) {
        return apply(project(hydra.graph.Primitive.TYPE_, hydra.graph.Primitive.NAME), p);
    }

    static Expr<?> primitiveType(TTerm<hydra.graph.Primitive> p) {
        return apply(project(hydra.graph.Primitive.TYPE_, hydra.graph.Primitive.TYPE), p);
    }

    static Expr<?> primitiveImplementation(TTerm<hydra.graph.Primitive> p) {
        return apply(project(hydra.graph.Primitive.TYPE_, hydra.graph.Primitive.IMPLEMENTATION), p);
    }

    static Expr<hydra.graph.Primitive> primitiveWithType(TTerm<hydra.graph.Primitive> p, TTerm<?> newType) {
        return primitive(primitiveName(p), newType, primitiveImplementation(p));
    }

    // ============================================================
    // TypeClass
    // ============================================================

    static Expr<hydra.classes.TypeClass> typeClassEquality() {
        return injectUnit(hydra.classes.TypeClass.TYPE_, hydra.classes.TypeClass.EQUALITY);
    }

    static Expr<hydra.classes.TypeClass> typeClassOrdering() {
        return injectUnit(hydra.classes.TypeClass.TYPE_, hydra.classes.TypeClass.ORDERING);
    }
}
