package hydra.demos.validatepg;

import hydra.core.Literal;
import hydra.core.LiteralType;
import hydra.core.Term;
import hydra.dsl.LiteralTypes;
import hydra.dsl.Literals;
import hydra.encode.pg.Model;
import hydra.json.Encode;
import hydra.json.model.Value;
import hydra.json.Writer;
import hydra.pg.dsl.Graphs;
import hydra.pg.model.Edge;
import hydra.pg.model.EdgeType;
import hydra.pg.model.Graph;
import hydra.pg.model.GraphSchema;
import hydra.pg.model.Vertex;
import hydra.pg.model.VertexType;
import hydra.util.Either;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Generates example property graph schema and graph data as JSON files for the PG validation
 * translingual demo.
 *
 * <p>Usage: java hydra.demos.validatepg.GenerateData &lt;output-directory&gt;
 *
 * <p>Produces schema.json plus one graph JSON file per validation condition:
 * valid, missing required property, wrong id type, unknown edge endpoint,
 * unexpected vertex/edge label, property value type mismatch, unexpected property key,
 * wrong out-vertex label, and missing required edge property.
 */
public class GenerateData {

    // -- Schema definition --

    private static GraphSchema<LiteralType> buildSchema() {
        VertexType<LiteralType> personType = Graphs.<LiteralType>vertexType("Person", LiteralTypes.string())
                .property("name", LiteralTypes.string(), true)
                .property("age", LiteralTypes.int32(), false)
                .property("nickname", LiteralTypes.string(), false)
                .build();

        VertexType<LiteralType> organizationType = Graphs.<LiteralType>vertexType("Organization", LiteralTypes.string())
                .property("name", LiteralTypes.string(), true)
                .property("industry", LiteralTypes.string(), true)
                .build();

        EdgeType<LiteralType> worksAtType = Graphs.<LiteralType>edgeType("worksAt", LiteralTypes.string(), "Person", "Organization")
                .property("role", LiteralTypes.string(), true)
                .build();

        EdgeType<LiteralType> foundedType = Graphs.<LiteralType>edgeType("founded", LiteralTypes.string(), "Person", "Organization")
                .build();

        return Graphs.schema(
                java.util.Arrays.asList(personType, organizationType),
                java.util.Arrays.asList(worksAtType, foundedType));
    }

    // -- Graph builders --

    private static Graph<Literal> buildValidSocialNetwork() {
        Vertex<Literal> p1 = Graphs.<Literal>vertex("Person", Literals.string("p1"))
                .property("name", Literals.string("Alice"))
                .property("age", Literals.int32(30))
                .build();

        Vertex<Literal> p2 = Graphs.<Literal>vertex("Person", Literals.string("p2"))
                .property("name", Literals.string("Bob"))
                .property("nickname", Literals.string("Bobby"))
                .build();

        Vertex<Literal> o1 = Graphs.<Literal>vertex("Organization", Literals.string("o1"))
                .property("name", Literals.string("Acme Corp"))
                .property("industry", Literals.string("technology"))
                .build();

        Edge<Literal> e1 = Graphs.<Literal>edge("worksAt", Literals.string("e1"),
                        Literals.string("p1"), Literals.string("o1"))
                .property("role", Literals.string("engineer"))
                .build();

        Edge<Literal> e2 = Graphs.<Literal>edge("founded", Literals.string("e2"),
                        Literals.string("p2"), Literals.string("o1"))
                .build();

        return Graphs.graph(
                java.util.Arrays.asList(p1, p2, o1),
                java.util.Arrays.asList(e1, e2));
    }

    private static Graph<Literal> buildMissingRequiredProperty() {
        Vertex<Literal> p3 = Graphs.<Literal>vertex("Person", Literals.string("p3"))
                .property("age", Literals.int32(25))
                .build();

        return Graphs.graph(
                java.util.Collections.singletonList(p3),
                java.util.Collections.emptyList());
    }

    private static Graph<Literal> buildWrongIdType() {
        Vertex<Literal> bad = Graphs.<Literal>vertex("Person", Literals.int32(42))
                .property("name", Literals.string("Charlie"))
                .build();

        return Graphs.graph(
                java.util.Collections.singletonList(bad),
                java.util.Collections.emptyList());
    }

    private static Graph<Literal> buildUnknownEdgeEndpoint() {
        Vertex<Literal> p4 = Graphs.<Literal>vertex("Person", Literals.string("p4"))
                .property("name", Literals.string("Diana"))
                .build();

        Edge<Literal> e3 = Graphs.<Literal>edge("worksAt", Literals.string("e3"),
                        Literals.string("p4"), Literals.string("o_nonexistent"))
                .property("role", Literals.string("manager"))
                .build();

        return Graphs.graph(
                java.util.Collections.singletonList(p4),
                java.util.Collections.singletonList(e3));
    }

    // Vertex label not defined in schema
    private static Graph<Literal> buildUnexpectedVertexLabel() {
        Vertex<Literal> v = Graphs.<Literal>vertex("Robot", Literals.string("r1"))
                .property("name", Literals.string("Bender"))
                .build();

        return Graphs.graph(
                java.util.Collections.singletonList(v),
                java.util.Collections.emptyList());
    }

    // Edge label not defined in schema
    private static Graph<Literal> buildUnexpectedEdgeLabel() {
        Vertex<Literal> p = Graphs.<Literal>vertex("Person", Literals.string("p5"))
                .property("name", Literals.string("Eve"))
                .build();
        Vertex<Literal> o = Graphs.<Literal>vertex("Organization", Literals.string("o2"))
                .property("name", Literals.string("Initech"))
                .property("industry", Literals.string("consulting"))
                .build();

        Edge<Literal> e = Graphs.<Literal>edge("manages", Literals.string("e4"),
                        Literals.string("p5"), Literals.string("o2"))
                .build();

        return Graphs.graph(
                java.util.Arrays.asList(p, o),
                java.util.Collections.singletonList(e));
    }

    // Property value has wrong type (int32 where string is expected)
    private static Graph<Literal> buildPropertyValueTypeMismatch() {
        Vertex<Literal> v = Graphs.<Literal>vertex("Person", Literals.string("p6"))
                .property("name", Literals.int32(999))  // name should be string, not int32
                .build();

        return Graphs.graph(
                java.util.Collections.singletonList(v),
                java.util.Collections.emptyList());
    }

    // Property key not defined in schema
    private static Graph<Literal> buildUnexpectedPropertyKey() {
        Vertex<Literal> v = Graphs.<Literal>vertex("Person", Literals.string("p7"))
                .property("name", Literals.string("Frank"))
                .property("favoriteColor", Literals.string("blue"))  // not in schema
                .build();

        return Graphs.graph(
                java.util.Collections.singletonList(v),
                java.util.Collections.emptyList());
    }

    // Edge's in-vertex has wrong label (Person instead of Organization)
    private static Graph<Literal> buildWrongInVertexLabel() {
        Vertex<Literal> p1 = Graphs.<Literal>vertex("Person", Literals.string("p8"))
                .property("name", Literals.string("Grace"))
                .build();
        Vertex<Literal> p2 = Graphs.<Literal>vertex("Person", Literals.string("p9"))
                .property("name", Literals.string("Heidi"))
                .build();

        // worksAt requires Person→Organization, but p9 is a Person
        Edge<Literal> e = Graphs.<Literal>edge("worksAt", Literals.string("e5"),
                        Literals.string("p8"), Literals.string("p9"))
                .property("role", Literals.string("colleague"))
                .build();

        return Graphs.graph(
                java.util.Arrays.asList(p1, p2),
                java.util.Collections.singletonList(e));
    }

    // Edge's out-vertex has wrong label (Organization instead of Person)
    private static Graph<Literal> buildWrongOutVertexLabel() {
        Vertex<Literal> o1 = Graphs.<Literal>vertex("Organization", Literals.string("o4"))
                .property("name", Literals.string("Stark Industries"))
                .property("industry", Literals.string("defense"))
                .build();
        Vertex<Literal> o2 = Graphs.<Literal>vertex("Organization", Literals.string("o5"))
                .property("name", Literals.string("Wayne Enterprises"))
                .property("industry", Literals.string("technology"))
                .build();

        // worksAt requires Person→Organization, but o4 (out-vertex) is an Organization
        Edge<Literal> e = Graphs.<Literal>edge("worksAt", Literals.string("e7"),
                        Literals.string("o4"), Literals.string("o5"))
                .property("role", Literals.string("partner"))
                .build();

        return Graphs.graph(
                java.util.Arrays.asList(o1, o2),
                java.util.Collections.singletonList(e));
    }

    // Edge missing a required property
    private static Graph<Literal> buildMissingRequiredEdgeProperty() {
        Vertex<Literal> p = Graphs.<Literal>vertex("Person", Literals.string("p10"))
                .property("name", Literals.string("Ivan"))
                .build();
        Vertex<Literal> o = Graphs.<Literal>vertex("Organization", Literals.string("o3"))
                .property("name", Literals.string("Globex"))
                .property("industry", Literals.string("defense"))
                .build();

        // worksAt requires "role" property, but we omit it
        Edge<Literal> e = Graphs.<Literal>edge("worksAt", Literals.string("e6"),
                        Literals.string("p10"), Literals.string("o3"))
                .build();

        return Graphs.graph(
                java.util.Arrays.asList(p, o),
                java.util.Collections.singletonList(e));
    }

    // -- Encoding helpers --

    private static String encodeSchemaToJson(GraphSchema<LiteralType> schema) {
        Term term = Model.graphSchema(hydra.encode.Core::literalType, schema);
        Either<String, Value> result = Encode.toJsonUntyped(term);
        return result.accept(new Either.Visitor<String, Value, String>() {
            @Override
            public String visit(Either.Left<String, Value> instance) {
                throw new RuntimeException("Failed to encode schema to JSON: " + instance.value);
            }

            @Override
            public String visit(Either.Right<String, Value> instance) {
                return Writer.printJson(instance.value);
            }
        });
    }

    private static String encodeGraphToJson(Graph<Literal> graph) {
        Term term = Model.graph(hydra.encode.Core::literal, graph);
        Either<String, Value> result = Encode.toJsonUntyped(term);
        return result.accept(new Either.Visitor<String, Value, String>() {
            @Override
            public String visit(Either.Left<String, Value> instance) {
                throw new RuntimeException("Failed to encode graph to JSON: " + instance.value);
            }

            @Override
            public String visit(Either.Right<String, Value> instance) {
                return Writer.printJson(instance.value);
            }
        });
    }

    private static void writeJsonFile(Path dir, String filename, String json) throws IOException {
        Path file = dir.resolve(filename);
        Files.write(file, json.getBytes(StandardCharsets.UTF_8));
        System.out.println("  Wrote " + file);
    }

    // -- Main --

    public static void main(String[] args) throws IOException {
        if (args.length < 1) {
            System.err.println("Usage: java hydra.demos.validatepg.GenerateData <output-directory>");
            System.exit(1);
        }

        Path outputDir = Paths.get(args[0]);
        Files.createDirectories(outputDir);
        System.out.println("Writing PG validation data to " + outputDir);

        // Schema
        GraphSchema<LiteralType> schema = buildSchema();
        writeJsonFile(outputDir, "schema.json", encodeSchemaToJson(schema));

        // Valid graph
        writeJsonFile(outputDir, "valid_social_network.json",
                encodeGraphToJson(buildValidSocialNetwork()));

        // Invalid: missing required property
        writeJsonFile(outputDir, "missing_required_property.json",
                encodeGraphToJson(buildMissingRequiredProperty()));

        // Invalid: wrong id type
        writeJsonFile(outputDir, "wrong_id_type.json",
                encodeGraphToJson(buildWrongIdType()));

        // Invalid: unknown edge endpoint
        writeJsonFile(outputDir, "unknown_edge_endpoint.json",
                encodeGraphToJson(buildUnknownEdgeEndpoint()));

        // Invalid: vertex label not in schema
        writeJsonFile(outputDir, "unexpected_vertex_label.json",
                encodeGraphToJson(buildUnexpectedVertexLabel()));

        // Invalid: edge label not in schema
        writeJsonFile(outputDir, "unexpected_edge_label.json",
                encodeGraphToJson(buildUnexpectedEdgeLabel()));

        // Invalid: property value type mismatch
        writeJsonFile(outputDir, "property_value_type_mismatch.json",
                encodeGraphToJson(buildPropertyValueTypeMismatch()));

        // Invalid: unexpected property key
        writeJsonFile(outputDir, "unexpected_property_key.json",
                encodeGraphToJson(buildUnexpectedPropertyKey()));

        // Invalid: wrong in-vertex label for edge
        writeJsonFile(outputDir, "wrong_in_vertex_label.json",
                encodeGraphToJson(buildWrongInVertexLabel()));

        // Invalid: wrong out-vertex label for edge
        writeJsonFile(outputDir, "wrong_out_vertex_label.json",
                encodeGraphToJson(buildWrongOutVertexLabel()));

        // Invalid: missing required edge property
        writeJsonFile(outputDir, "missing_required_edge_property.json",
                encodeGraphToJson(buildMissingRequiredEdgeProperty()));

        System.out.println("Done. Wrote 12 JSON files.");
    }
}
