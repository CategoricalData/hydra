package hydra.langs.tinkerpop;

import hydra.basics.Basics;
import hydra.core.Literal;
import hydra.core.LiteralType;
import hydra.dsl.LiteralTypes;
import hydra.dsl.Literals;
import hydra.langs.tinkerpop.propertyGraph.Edge;
import hydra.langs.tinkerpop.propertyGraph.EdgeType;
import hydra.langs.tinkerpop.propertyGraph.Element;
import hydra.langs.tinkerpop.propertyGraph.ElementType;
import hydra.langs.tinkerpop.propertyGraph.PropertyKey;
import hydra.langs.tinkerpop.propertyGraph.Vertex;
import hydra.langs.tinkerpop.propertyGraph.VertexLabel;
import hydra.langs.tinkerpop.propertyGraph.VertexType;
import hydra.langs.tinkerpop.validate.Validate;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.fail;

public class ValidationTest extends PropertyGraphTestBase {
    private static final Function<LiteralType, Function<Literal, Optional<String>>> CHECK_LITERAL
            = type -> value -> checkLiteral(type, value);

    @Test
    public void testValidVertexSucceeds() {
        assertValid(VERTEX_TYPE_PERSON_A, VERTEX_PERSON_2);
    }

    @Test
    public void testVertexLabelMismatchFails() {
        assertInvalid(VERTEX_TYPE_PERSON_A.withLabel(new VertexLabel("SomeOtherLabel")), VERTEX_PERSON_2);
        assertInvalid(VERTEX_TYPE_PERSON_A, VERTEX_PERSON_2.withLabel(new VertexLabel("SomeOtherLabel")));
    }

    @Test
    public void testWrongVertexIdTypeFails() {
        assertInvalid(VERTEX_TYPE_PERSON_A.withId(LiteralTypes.int64()), VERTEX_PERSON_2);
        assertInvalid(VERTEX_TYPE_PERSON_A, VERTEX_PERSON_2.withId(Literals.float64(42.0)));
    }

    @Test
    public void testMissingRequiredVertexPropertyFails() {
        assertInvalid(VERTEX_TYPE_PERSON_A, VERTEX_PERSON_2.withProperties(new HashMap<>()));
        assertInvalid(VERTEX_TYPE_PERSON_A, VERTEX_PERSON_2.withProperties(
                new HashMap<PropertyKey, Literal>(){{put(new PropertyKey("nickname"), Literals.string("Ix"));}}));
    }

    @Test
    public void testMissingOptionalVertexPropertySucceeds() {
        assertValid(VERTEX_TYPE_PERSON_A, VERTEX_PERSON_2.withProperties(
                new HashMap<PropertyKey, Literal>(){{put(new PropertyKey("name"), Literals.string("Ford"));}}));
    }

    @Test
    public void testValidEdgeSucceedsNoGraph() {
        assertValid(EDGE_TYPE_WORKSAT_A, EDGE_WORKSAT_1);
    }

    @Test
    public void testValidEdgeSucceedsWithGraph() {
        Map<Literal, Vertex<Literal>> vertices = new HashMap<Literal, Vertex<Literal>>(){{
            put(VERTEX_PERSON_2.id, VERTEX_PERSON_2);
            put(VERTEX_ORGANIZATION_1.id, VERTEX_ORGANIZATION_1);
        }};
        Function<Literal, Optional<VertexLabel>> labelForVertexId
                = id -> Optional.ofNullable(vertices.get(id)).map(v -> v.label);

        assertValid(EDGE_TYPE_WORKSAT_A, EDGE_WORKSAT_1, Optional.of(labelForVertexId));
    }
    
    @Test
    public void testMissingOutOrInVertexFails() {
        Function<Literal, Optional<VertexLabel>> labelForVertexId = id -> Optional.empty();

        assertInvalid(EDGE_TYPE_WORKSAT_A, EDGE_WORKSAT_1, Optional.of(labelForVertexId));
    }

    @Test
    public void testEdgeWithOutOrInVertexOfWrongLabelFails() {
        // Oops: ids point to the wrong vertices
        Map<Literal, Vertex<Literal>> vertices = new HashMap<Literal, Vertex<Literal>>(){{
            put(VERTEX_PERSON_2.id, VERTEX_ORGANIZATION_1);
            put(VERTEX_ORGANIZATION_1.id, VERTEX_PERSON_2);
        }};

        Function<Literal, Optional<VertexLabel>> labelForVertexId
                = id -> Optional.ofNullable(vertices.get(id)).map(v -> v.label);

        assertInvalid(EDGE_TYPE_WORKSAT_A, EDGE_WORKSAT_1, Optional.of(labelForVertexId));
    }

    @Test
    public void testValidElementSucceeds() {
        assertValid(toElementType(VERTEX_TYPE_PERSON_A), toElement(VERTEX_PERSON_2));
        assertValid(toElementType(EDGE_TYPE_WORKSAT_A), toElement(EDGE_WORKSAT_1));
    }

    @Test
    public void testInvalidElementFails() {
        assertInvalid(toElementType(VERTEX_TYPE_PERSON_A), toElement(VERTEX_ORGANIZATION_1));
        assertInvalid(toElementType(EDGE_TYPE_WORKSAT_A), toElement(EDGE_FOUNDED_1));
    }

    @Test
    public void testElementOfWrongClassFails() {
        assertInvalid(toElementType(VERTEX_TYPE_PERSON_A), toElement(EDGE_WORKSAT_1));
        assertInvalid(toElementType(EDGE_TYPE_WORKSAT_A), toElement(VERTEX_PERSON_2));
    }

    private static void assertInvalid(VertexType<LiteralType> type, Vertex<Literal> element) {
        Optional<String> result = Validate.validateVertex(CHECK_LITERAL).apply(Literals::showLiteral)
                .apply(type).apply(element);
        if (!result.isPresent()) {
            fail("Validation succeeded where it should have failed");
        }
    }

    private static void assertValid(VertexType<LiteralType> type, Vertex<Literal> element) {
        Optional<String> result = Validate.validateVertex(CHECK_LITERAL).apply(Literals::showLiteral)
                .apply(type).apply(element);
        result.ifPresent(s -> fail("Validation failed: " + s));
    }

    private static void assertInvalid(EdgeType<LiteralType> type,
                                      Edge<Literal> element,
                                      Optional<Function<Literal, Optional<VertexLabel>>> labelForVertexId) {
        Optional<String> result = Validate.validateEdge(CHECK_LITERAL).apply(Literals::showLiteral)
                .apply(labelForVertexId).apply(type).apply(element);
        if (!result.isPresent()) {
            fail("Validation succeeded where it should have failed");
        }
    }

    private static void assertInvalid(EdgeType<LiteralType> type,
                                      Edge<Literal> element) {
        assertInvalid(type, element, Optional.empty());
    }

    private static void assertValid(EdgeType<LiteralType> type,
                                    Edge<Literal> element,
                                    Optional<Function<Literal, Optional<VertexLabel>>> labelForVertexId) {
        Optional<String> result = Validate.validateEdge(CHECK_LITERAL).apply(Literals::showLiteral)
                .apply(labelForVertexId).apply(type).apply(element);
        result.ifPresent(s -> fail("Validation failed: " + s));
    }

    private static void assertValid(EdgeType<LiteralType> type,
                                    Edge<Literal> element) {
        assertValid(type, element, Optional.empty());
    }

    private static void assertInvalid(ElementType<LiteralType> type,
                                      Element<Literal> element) {
        Optional<String> result = Validate.validateElement(CHECK_LITERAL).apply(Literals::showLiteral)
                .apply(Optional.empty()).apply(type).apply(element);
        if (!result.isPresent()) {
            fail("Validation succeeded where it should have failed");
        }
    }

    private static void assertValid(ElementType<LiteralType> type, Element<Literal> element) {
        Optional<String> result = Validate.validateElement(CHECK_LITERAL).apply(Literals::showLiteral)
                .apply(Optional.empty()).apply(type).apply(element);
        result.ifPresent(s -> fail("Validation failed: " + s));
    }

    private static Optional<String> checkLiteral(LiteralType type, Literal value) {
        String expected = LiteralTypes.showLiteralType(type);
        String actual = LiteralTypes.showLiteralType(Basics.literalType(value));
        return expected.equals(actual)
                ? Optional.empty()
                : Optional.of("Expected literal of type " + expected + ", found " + actual);
    }
}
