package hydra.demos.neo4jvalidation;

import hydra.neo4j.model.ConstraintDefinition;
import hydra.neo4j.model.Constraint;
import hydra.neo4j.model.ElementId;
import hydra.neo4j.model.GraphType;
import hydra.neo4j.model.Key;

import hydra.neo4j.model.NodeElementType;
import hydra.neo4j.model.NodeLabel;
import hydra.neo4j.model.PropertyExistenceConstraint;
import hydra.neo4j.model.PropertyTypeConstraint;
import hydra.neo4j.model.RelationshipElementType;
import hydra.neo4j.model.RelationshipType;
import hydra.neo4j.model.Value;
import hydra.neo4j.model.ValueType;
import hydra.error.neo4j.InvalidGraphError;
import hydra.validate.Neo4j;
import hydra.validation.ValidationProfile;
import hydra.validation.ValidationResult;
import hydra.overlay.java.util.Optional;

import org.neo4j.driver.AuthTokens;
import org.neo4j.driver.Driver;
import org.neo4j.driver.GraphDatabase;
import org.neo4j.driver.Record;
import org.neo4j.driver.Result;
import org.neo4j.driver.Session;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Java driver for the Neo4j validation demo.
 *
 * <p>Connects to a running Neo4j over the Bolt protocol using the official
 * Neo4j Java driver ({@code org.neo4j.driver}), reads all nodes and
 * relationships, maps the driver's types onto Hydra's {@code hydra.neo4j.model}
 * types, and runs {@code hydra.validate.neo4j.validateGraph} against a graph
 * type defined here. The same validation runs identically in the Python
 * counterpart (demo.py).
 *
 * <p>The client-interfacing logic (the driver calls and the mapping from the
 * driver's types to Hydra's) lives in this demo for now; it will move to a
 * reusable {@code overlay/java/hydra-neo4j} once #511 lands.
 *
 * <p>Usage: java hydra.demos.neo4jvalidation.Neo4jValidationDemo [uri] [user] [password]
 * (defaults: bolt://localhost:7687, neo4j, neo4j). If no Neo4j is reachable the
 * demo prints a notice and exits 0 (so it does not break offline runs).
 */
public class Neo4jValidationDemo {

    public static void main(String[] args) {
        String uri = args.length > 0 ? args[0] : "bolt://localhost:7687";
        String user = args.length > 1 ? args[1] : "neo4j";
        String password = args.length > 2 ? args[2] : "neo4j";

        Driver driver;
        try {
            driver = GraphDatabase.driver(uri, AuthTokens.basic(user, password));
            driver.verifyConnectivity();
        } catch (Exception e) {
            System.out.println("No Neo4j reachable at " + uri + " (" + e.getMessage() + ").");
            System.out.println("Skipping the live-validation demo. Start Neo4j and seed it with "
                + "demos/neo4j-validation/fixture.cypher to run it.");
            return;
        }

        try (driver; Session session = driver.session()) {
            List<hydra.neo4j.model.Node> nodes = readNodes(session);
            List<hydra.neo4j.model.Relationship> rels = readRelationships(session);

            System.out.println("Read " + nodes.size() + " nodes and " + rels.size()
                + " relationships from Neo4j.");

            GraphType graphType = movieGraphType();
            ValidationProfile profile = reportAllProfile();

            ValidationResult<InvalidGraphError> result =
                Neo4j.validateGraph(profile, graphType, nodes, rels);

            List<InvalidGraphError> errors = result.errors;
            if (errors.isEmpty()) {
                System.out.println("The graph conforms to the graph type. No violations.");
            } else {
                System.out.println(errors.size() + " violation(s):");
                for (InvalidGraphError err : errors) {
                    System.out.println("  - " + describe(err));
                }
            }
        }
    }

    // ------------------------------------------------------------------------
    // Reading from Neo4j and mapping to the Hydra model
    // ------------------------------------------------------------------------

    private static List<hydra.neo4j.model.Node> readNodes(Session session) {
        List<hydra.neo4j.model.Node> result = new ArrayList<>();
        Result r = session.run("MATCH (n) RETURN n");
        while (r.hasNext()) {
            Record rec = r.next();
            org.neo4j.driver.types.Node dn = rec.get("n").asNode();
            result.add(mapNode(dn));
        }
        return result;
    }

    private static List<hydra.neo4j.model.Relationship> readRelationships(Session session) {
        List<hydra.neo4j.model.Relationship> result = new ArrayList<>();
        Result r = session.run("MATCH ()-[rel]->() RETURN rel");
        while (r.hasNext()) {
            Record rec = r.next();
            org.neo4j.driver.types.Relationship dr = rec.get("rel").asRelationship();
            result.add(mapRelationship(dr));
        }
        return result;
    }

    private static hydra.neo4j.model.Node mapNode(org.neo4j.driver.types.Node dn) {
        Set<NodeLabel> labels = new LinkedHashSet<>();
        for (String l : dn.labels()) {
            labels.add(new NodeLabel(l));
        }
        Map<Key, Value> props = mapProperties(dn.asMap());
        return new hydra.neo4j.model.Node(new ElementId(dn.elementId()), labels, props);
    }

    private static hydra.neo4j.model.Relationship mapRelationship(org.neo4j.driver.types.Relationship dr) {
        Map<Key, Value> props = mapProperties(dr.asMap());
        return new hydra.neo4j.model.Relationship(
            new ElementId(dr.elementId()),
            props,
            new RelationshipType(dr.type()),
            new ElementId(dr.startNodeElementId()),
            new ElementId(dr.endNodeElementId()));
    }

    private static Map<Key, Value> mapProperties(Map<String, Object> driverProps) {
        Map<Key, Value> props = new LinkedHashMap<>();
        for (Map.Entry<String, Object> e : driverProps.entrySet()) {
            props.put(new Key(e.getKey()), mapValue(e.getValue()));
        }
        return props;
    }

    /** Map a driver property value to a hydra.neo4j.model.Value. Covers the kinds the fixture uses. */
    private static Value mapValue(Object o) {
        if (o instanceof Boolean) {
            return new Value.Boolean_((Boolean) o);
        } else if (o instanceof Long) {
            return new Value.Integer_((Long) o);
        } else if (o instanceof Integer) {
            return new Value.Integer_(((Integer) o).longValue());
        } else if (o instanceof Double || o instanceof Float) {
            return new Value.Float_(((Number) o).doubleValue());
        } else if (o instanceof String) {
            return new Value.String_((String) o);
        } else if (o instanceof List) {
            List<Value> vs = new ArrayList<>();
            for (Object item : (List<?>) o) {
                vs.add(mapValue(item));
            }
            return new Value.List(vs);
        } else {
            // Anything else (temporal, spatial, bytes) — out of scope for this demo's fixture.
            return new Value.String_(String.valueOf(o));
        }
    }

    // ------------------------------------------------------------------------
    // The graph type (schema) to validate against
    // ------------------------------------------------------------------------

    private static GraphType movieGraphType() {
        // (:Person { name :: STRING (required), born :: INTEGER })
        NodeElementType person = new NodeElementType(
            new NodeLabel("Person"),
            new LinkedHashSet<>(),
            List.of(
                existence("name"),
                propertyType("name", new ValueType.String_()),
                propertyType("born", new ValueType.Integer_())));

        // (:Movie { title :: STRING (required), released :: INTEGER (required) })
        NodeElementType movie = new NodeElementType(
            new NodeLabel("Movie"),
            new LinkedHashSet<>(),
            List.of(
                existence("title"),
                propertyType("title", new ValueType.String_()),
                existence("released"),
                propertyType("released", new ValueType.Integer_())));

        // (:Person)-[:ACTED_IN]->(:Movie)
        RelationshipElementType actedIn = new RelationshipElementType(
            new RelationshipType("ACTED_IN"), new NodeLabel("Person"), new NodeLabel("Movie"),
            List.of());

        // (:Person)-[:LIKES]->(:Movie) and (:Person)-[:LIKES]->(:Person) — overloaded type
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
    // Profile and reporting
    // ------------------------------------------------------------------------

    /** A profile that reports all violations (high maxErrors), open-world (default rules). */
    private static ValidationProfile reportAllProfile() {
        ValidationProfile base = Neo4j.defaultNeo4jProfile();
        return new ValidationProfile(base.errorRules, base.warningRules, 1000, 1000);
    }

    private static String describe(InvalidGraphError err) {
        // A compact rendering of the structured error. The full structured value
        // is available for programmatic use; this is just for display.
        return err.toString();
    }
}
