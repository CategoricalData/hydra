package hydra.demos.neo4jvalidation;

import hydra.core.Term;
import hydra.encode.neo4j.Model;
import hydra.json.Encode;
import hydra.json.Writer;
import hydra.json.model.Value;
import hydra.neo4j.model.Constraint;
import hydra.neo4j.model.ConstraintDefinition;
import hydra.neo4j.model.ElementId;
import hydra.neo4j.model.GraphType;
import hydra.neo4j.model.Key;
import hydra.neo4j.model.Node;
import hydra.neo4j.model.NodeElementType;
import hydra.neo4j.model.NodeLabel;
import hydra.neo4j.model.PropertyExistenceConstraint;
import hydra.neo4j.model.PropertyTypeConstraint;
import hydra.neo4j.model.Relationship;
import hydra.neo4j.model.RelationshipElementType;
import hydra.neo4j.model.RelationshipType;
import hydra.neo4j.model.ValueType;
import hydra.overlay.java.util.Either;
import hydra.overlay.java.util.Optional;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Generates the shared schema and graph data for the Neo4j validation translingual demo,
 * as JSON files, from DSL-based definitions in Java.
 *
 * <p>The schema (a Neo4j {@code GraphType}) and a family of graphs (one valid, four invalid)
 * are authored once here as {@code hydra.neo4j.model} values, encoded to Hydra's canonical
 * term-JSON via {@code hydra.encode.neo4j.Model} + {@code hydra.json}, and written to disk.
 * Those JSON files are the single source of truth: every host language (Java, Python, Haskell,
 * ...) then loads the <em>same</em> files and runs the <em>same</em> generated validator, so the
 * data under test and the validation logic are identical across languages by construction.
 *
 * <p>Usage: java hydra.demos.neo4jvalidation.GenerateData &lt;output-directory&gt;
 */
public class GenerateData {

    // ------------------------------------------------------------------------
    // The schema: a small movie domain shared by every graph below.
    //
    //   (:Person  { name :: STRING (required), born :: INTEGER })
    //   (:Movie   { title :: STRING (required), released :: INTEGER (required) })
    //   (:Person)-[:ACTED_IN]->(:Movie)
    //   (:Person)-[:LIKES]->(:Movie)   and   (:Person)-[:LIKES]->(:Person)   (overloaded)
    // ------------------------------------------------------------------------

    private static GraphType buildSchema() {
        NodeElementType person = new NodeElementType(
            new NodeLabel("Person"),
            new LinkedHashSet<>(),
            List.of(
                existence("name"),
                propertyType("name", new ValueType.String_()),
                propertyType("born", new ValueType.Integer_())));

        NodeElementType movie = new NodeElementType(
            new NodeLabel("Movie"),
            new LinkedHashSet<>(),
            List.of(
                existence("title"),
                propertyType("title", new ValueType.String_()),
                existence("released"),
                propertyType("released", new ValueType.Integer_())));

        RelationshipElementType actedIn = new RelationshipElementType(
            new RelationshipType("ACTED_IN"), new NodeLabel("Person"), new NodeLabel("Movie"),
            List.of());

        RelationshipElementType likesMovie = new RelationshipElementType(
            new RelationshipType("LIKES"), new NodeLabel("Person"), new NodeLabel("Movie"), List.of());
        RelationshipElementType likesPerson = new RelationshipElementType(
            new RelationshipType("LIKES"), new NodeLabel("Person"), new NodeLabel("Person"), List.of());

        return new GraphType(
            List.of(person, movie),
            List.of(actedIn, likesMovie, likesPerson));
    }

    private static ConstraintDefinition existence(String key) {
        return new ConstraintDefinition(
            Optional.none(),
            new Constraint.PropertyExistence(new PropertyExistenceConstraint(new Key(key))));
    }

    private static ConstraintDefinition propertyType(String key, ValueType type) {
        return new ConstraintDefinition(
            Optional.none(),
            new Constraint.PropertyType(new PropertyTypeConstraint(new Key(key), type)));
    }

    // ------------------------------------------------------------------------
    // Graph builders. A "graph" here is a (nodes, relationships) pair, written
    // to JSON as a two-field object so a host can decode it without a Neo4j server.
    // ------------------------------------------------------------------------

    /** A valid movie graph: two people, one movie, an ACTED_IN and a LIKES that all conform. */
    private static Graph buildValid() {
        Node alice = node("p1", labels("Person"), props(
            entry("name", str("Alice")),
            entry("born", integer(1970))));
        Node bob = node("p2", labels("Person"), props(
            entry("name", str("Bob")),
            entry("born", integer(1965))));
        Node matrix = node("m1", labels("Movie"), props(
            entry("title", str("The Matrix")),
            entry("released", integer(1999))));

        Relationship acted = rel("r1", "ACTED_IN", "p1", "m1", props());
        Relationship likes = rel("r2", "LIKES", "p2", "m1", props());

        return new Graph(List.of(alice, bob, matrix), List.of(acted, likes));
    }

    /** Invalid: a Person node with no required {@code name} property. */
    private static Graph buildMissingRequiredProperty() {
        Node noName = node("p3", labels("Person"), props(
            entry("born", integer(1970))));
        return new Graph(List.of(noName), List.of());
    }

    /** Invalid: a Movie whose {@code released} is a STRING where the schema requires INTEGER. */
    private static Graph buildWrongPropertyType() {
        Node movie = node("m2", labels("Movie"), props(
            entry("title", str("Cypher")),
            entry("released", str("2003"))));
        return new Graph(List.of(movie), List.of());
    }

    /** Invalid: a Movie missing the required {@code released} property. */
    private static Graph buildMissingRequiredReleased() {
        Node movie = node("m3", labels("Movie"), props(
            entry("title", str("Untitled"))));
        return new Graph(List.of(movie), List.of());
    }

    /** Invalid: a LIKES whose endpoints (Movie-&gt;Person) match no declared pattern for LIKES. */
    private static Graph buildEndpointMismatch() {
        Node person = node("p4", labels("Person"), props(
            entry("name", str("Carol")),
            entry("born", integer(1980))));
        Node movie = node("m4", labels("Movie"), props(
            entry("title", str("Inception")),
            entry("released", integer(2010))));
        // LIKES is declared Person->Movie and Person->Person, never Movie->Person.
        Relationship badLikes = rel("r3", "LIKES", "m4", "p4", props());
        return new Graph(List.of(person, movie), List.of(badLikes));
    }

    /**
     * A richer valid graph that exercises every type: two movies, three people, ACTED_IN, and both
     * overloaded LIKES patterns (Person-&gt;Movie and Person-&gt;Person). "VALID" here means something.
     */
    private static Graph buildValidRicher() {
        Node keanu = node("vp1", labels("Person"), props(
            entry("name", str("Keanu Reeves")), entry("born", integer(1964))));
        Node carrie = node("vp2", labels("Person"), props(
            entry("name", str("Carrie-Anne Moss")), entry("born", integer(1967))));
        Node fan = node("vp3", labels("Person"), props(entry("name", str("A Fan"))));
        Node matrix = node("vm1", labels("Movie"), props(
            entry("title", str("The Matrix")), entry("released", integer(1999))));
        Node john = node("vm2", labels("Movie"), props(
            entry("title", str("John Wick")), entry("released", integer(2014))));

        Relationship a1 = rel("vr1", "ACTED_IN", "vp1", "vm1", props());
        Relationship a2 = rel("vr2", "ACTED_IN", "vp2", "vm1", props());
        Relationship a3 = rel("vr3", "ACTED_IN", "vp1", "vm2", props());
        Relationship likesMovie = rel("vr4", "LIKES", "vp3", "vm1", props());      // Person->Movie
        Relationship likesPerson = rel("vr5", "LIKES", "vp3", "vp1", props());      // Person->Person

        return new Graph(
            List.of(keanu, carrie, fan, matrix, john),
            List.of(a1, a2, a3, likesMovie, likesPerson));
    }

    /** Valid: uses the OTHER overloaded LIKES pattern (Person-&gt;Person), proving overload resolution. */
    private static Graph buildValidLikesPerson() {
        Node a = node("lp1", labels("Person"), props(entry("name", str("Dana"))));
        Node b = node("lp2", labels("Person"), props(entry("name", str("Erin"))));
        Relationship likes = rel("lr1", "LIKES", "lp1", "lp2", props());
        return new Graph(List.of(a, b), List.of(likes));
    }

    /**
     * Several planted violations in ONE graph, to show the validator reports them all at once —
     * the differentiator versus Neo4j, which enforces at write time, one element at a time.
     */
    private static Graph buildMultipleViolations() {
        Node noName = node("x1", labels("Person"), props(entry("born", integer(1990))));      // missing name
        Node badReleased = node("x2", labels("Movie"), props(
            entry("title", str("Glitch")), entry("released", str("two thousand"))));          // wrong type
        Node noReleased = node("x3", labels("Movie"), props(entry("title", str("Sequel"))));  // missing released
        Node person = node("x4", labels("Person"), props(entry("name", str("Finn"))));
        Node movie = node("x5", labels("Movie"), props(
            entry("title", str("Tenet")), entry("released", integer(2020))));
        Relationship badLikes = rel("xr1", "LIKES", "x5", "x4", props());                     // Movie->Person

        return new Graph(
            List.of(noName, badReleased, noReleased, person, movie),
            List.of(badLikes));
    }

    /** Invalid: an OPTIONAL property ({@code born}) present with the wrong type (STRING, not INTEGER). */
    private static Graph buildOptionalWrongType() {
        Node person = node("o1", labels("Person"), props(
            entry("name", str("Gale")),
            entry("born", str("1975"))));   // born is optional, but when present must be INTEGER
        return new Graph(List.of(person), List.of());
    }

    /** Invalid: an ACTED_IN going the wrong way (Movie-&gt;Person); ACTED_IN is declared Person-&gt;Movie. */
    private static Graph buildActedInWrongDirection() {
        Node person = node("d1", labels("Person"), props(entry("name", str("Hana"))));
        Node movie = node("d2", labels("Movie"), props(
            entry("title", str("Arrival")), entry("released", integer(2016))));
        Relationship acted = rel("dr1", "ACTED_IN", "d2", "d1", props());  // reversed
        return new Graph(List.of(person, movie), List.of(acted));
    }

    /**
     * Open-world: a node with a label the schema never declares ({@code Robot}). Under the default
     * open-world profile this is VALID (a node matching no element type is not an error), matching
     * Neo4j's open GRAPH TYPE. The strict profile would flag it; this demo uses the default profile.
     */
    private static Graph buildUndeclaredLabelOpenWorld() {
        Node robot = node("w1", labels("Robot"), props(entry("serial", str("R2D2"))));
        return new Graph(List.of(robot), List.of());
    }

    // ------------------------------------------------------------------------
    // Small builders over the Neo4j model.
    // ------------------------------------------------------------------------

    private static Node node(String id, Set<NodeLabel> labels, Map<Key, hydra.neo4j.model.Value> props) {
        return new Node(new ElementId(id), labels, props);
    }

    private static Relationship rel(String id, String type, String start, String end,
                                    Map<Key, hydra.neo4j.model.Value> props) {
        return new Relationship(
            new ElementId(id), props, new RelationshipType(type),
            new ElementId(start), new ElementId(end));
    }

    private static Set<NodeLabel> labels(String... ls) {
        Set<NodeLabel> set = new LinkedHashSet<>();
        for (String l : ls) {
            set.add(new NodeLabel(l));
        }
        return set;
    }

    @SafeVarargs
    private static Map<Key, hydra.neo4j.model.Value> props(Map.Entry<Key, hydra.neo4j.model.Value>... es) {
        Map<Key, hydra.neo4j.model.Value> m = new LinkedHashMap<>();
        for (Map.Entry<Key, hydra.neo4j.model.Value> e : es) {
            m.put(e.getKey(), e.getValue());
        }
        return m;
    }

    private static Map.Entry<Key, hydra.neo4j.model.Value> entry(String key, hydra.neo4j.model.Value v) {
        return Map.entry(new Key(key), v);
    }

    private static hydra.neo4j.model.Value str(String s) {
        return new hydra.neo4j.model.Value.String_(s);
    }

    private static hydra.neo4j.model.Value integer(long n) {
        return new hydra.neo4j.model.Value.Integer_(n);
    }

    // ------------------------------------------------------------------------
    // Encoding to JSON. A graph is encoded as { "nodes": [...], "relationships": [...] }
    // using the generated per-type encoders; the schema uses the generated graphType encoder.
    // ------------------------------------------------------------------------

    private static String encodeGraphTypeToJson(GraphType schema) {
        return termToJson(Model.graphType(schema), "schema");
    }

    private static String encodeGraphToJson(Graph graph) {
        List<Term> nodeTerms = graph.nodes.stream().map(Model::node).toList();
        List<Term> relTerms = graph.relationships.stream().map(Model::relationship).toList();
        Term term = new Term.Record(new hydra.core.Record(
            new hydra.core.Name("hydra.demos.neo4jvalidation.Graph"),
            hydra.overlay.java.util.ConsList.<hydra.core.Field>of(
                new hydra.core.Field(new hydra.core.Name("nodes"), new Term.List(nodeTerms)),
                new hydra.core.Field(new hydra.core.Name("relationships"), new Term.List(relTerms)))));
        return termToJson(term, "graph");
    }

    private static String termToJson(Term term, String what) {
        Either<String, Value> result = Encode.toJsonUntyped(term);
        return result.accept(new Either.Visitor<String, Value, String>() {
            @Override
            public String visit(Either.Left<String, Value> instance) {
                throw new RuntimeException("Failed to encode " + what + " to JSON: " + instance.value);
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

    // A nodes/relationships pair, the unit this demo validates.
    private record Graph(List<Node> nodes, List<Relationship> relationships) {}

    // ------------------------------------------------------------------------

    public static void main(String[] args) throws IOException {
        if (args.length < 1) {
            System.err.println("Usage: java hydra.demos.neo4jvalidation.GenerateData <output-directory>");
            System.exit(1);
        }

        Path outputDir = Paths.get(args[0]);
        Files.createDirectories(outputDir);
        System.out.println("Writing Neo4j validation data to " + outputDir);

        writeJsonFile(outputDir, "schema.json", encodeGraphTypeToJson(buildSchema()));

        writeJsonFile(outputDir, "valid.json", encodeGraphToJson(buildValid()));
        writeJsonFile(outputDir, "missing_required_property.json",
            encodeGraphToJson(buildMissingRequiredProperty()));
        writeJsonFile(outputDir, "wrong_property_type.json",
            encodeGraphToJson(buildWrongPropertyType()));
        writeJsonFile(outputDir, "missing_required_released.json",
            encodeGraphToJson(buildMissingRequiredReleased()));
        writeJsonFile(outputDir, "endpoint_mismatch.json",
            encodeGraphToJson(buildEndpointMismatch()));

        // Additional graphs (a menu to choose from).
        writeJsonFile(outputDir, "valid_richer.json",
            encodeGraphToJson(buildValidRicher()));
        writeJsonFile(outputDir, "valid_likes_person.json",
            encodeGraphToJson(buildValidLikesPerson()));
        writeJsonFile(outputDir, "multiple_violations.json",
            encodeGraphToJson(buildMultipleViolations()));
        writeJsonFile(outputDir, "optional_wrong_type.json",
            encodeGraphToJson(buildOptionalWrongType()));
        writeJsonFile(outputDir, "acted_in_wrong_direction.json",
            encodeGraphToJson(buildActedInWrongDirection()));
        writeJsonFile(outputDir, "undeclared_label_open_world.json",
            encodeGraphToJson(buildUndeclaredLabelOpenWorld()));

        System.out.println("Done. Wrote 1 schema + 11 graph JSON files.");
    }
}
