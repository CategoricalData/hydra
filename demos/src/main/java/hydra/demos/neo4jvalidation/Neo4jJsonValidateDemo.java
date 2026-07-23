package hydra.demos.neo4jvalidation;

import hydra.overlay.java.build.Generation;
import hydra.error.neo4j.InvalidGraphError;
import hydra.json.model.Value;
import hydra.neo4j.model.GraphType;
import hydra.neo4j.model.Node;
import hydra.neo4j.model.Relationship;
import hydra.validate.Neo4j;
import hydra.validation.ValidationProfile;
import hydra.validation.ValidationResult;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

/**
 * Java driver for the Neo4j validation translingual demo (JSON-artifact variant).
 *
 * <p>Reads the shared {@code schema.json} and each graph JSON file (produced by
 * {@link GenerateData} from Java DSL definitions), decodes them back into
 * {@code hydra.neo4j.model} values, and validates each graph against the schema
 * with {@code hydra.validate.neo4j.validateGraph}. The same validator — generated
 * from one Hydra source — runs identically in the Python and Haskell counterparts;
 * because every host reads the <em>same</em> JSON files, the data and the logic are
 * identical across languages by construction.
 *
 * <p>Usage: java hydra.demos.neo4jvalidation.Neo4jJsonValidateDemo &lt;data-directory&gt;
 */
public class Neo4jJsonValidateDemo {

    // The graph files, in the order produced by GenerateData.
    private static final String[] GRAPH_FILES = {
        "valid",
        "missing_required_property",
        "wrong_property_type",
        "missing_required_released",
        "endpoint_mismatch",
        "valid_richer",
        "valid_likes_person",
        "multiple_violations",
        "optional_wrong_type",
        "acted_in_wrong_direction",
        "undeclared_label_open_world",
    };

    // The graphs to validate: the canonical fixture list when present, otherwise every *.json in the
    // directory except schema.json (so a Cypher-ingested graph dropped in is picked up automatically).
    private static List<String> graphNames(Path dataDir) throws IOException {
        List<String> canonical = new java.util.ArrayList<>();
        for (String name : GRAPH_FILES) {
            if (Files.exists(dataDir.resolve(name + ".json"))) {
                canonical.add(name);
            }
        }
        if (!canonical.isEmpty()) {
            return canonical;
        }
        List<String> found = new java.util.ArrayList<>();
        try (java.util.stream.Stream<Path> s = Files.list(dataDir)) {
            s.map(p -> p.getFileName().toString())
                .filter(n -> n.endsWith(".json") && !n.equals("schema.json"))
                .map(n -> n.substring(0, n.length() - ".json".length()))
                .sorted()
                .forEach(found::add);
        }
        return found;
    }

    public static void main(String[] args) throws IOException {
        if (args.length < 1) {
            System.err.println("Usage: java hydra.demos.neo4jvalidation.Neo4jJsonValidateDemo <data-directory>");
            System.exit(1);
        }

        Path dataDir = Paths.get(args[0]);

        // Read and decode the shared schema.
        Value schemaJson = Generation.parseJsonFile(dataDir.resolve("schema.json").toString());
        GraphType schema = JsonNeo4jDecoder.decodeGraphType(schemaJson);

        // An open-world profile that reports all violations (default bounds maxErrors=1).
        ValidationProfile base = Neo4j.defaultNeo4jProfile();
        ValidationProfile profile = new ValidationProfile(
            base.errorRules, base.warningRules, 1000, 1000);

        for (String name : graphNames(dataDir)) {
            Path graphPath = dataDir.resolve(name + ".json");
            if (!Files.exists(graphPath)) {
                continue;
            }
            Value graphJson = Generation.parseJsonFile(graphPath.toString());
            List<Node> nodes = JsonNeo4jDecoder.decodeNodes(graphJson);
            List<Relationship> rels = JsonNeo4jDecoder.decodeRelationships(graphJson);

            ValidationResult<InvalidGraphError> result =
                Neo4j.validateGraph(profile, schema, nodes, rels);

            if (result.errors.isEmpty()) {
                System.out.println("Graph \"" + name + "\": VALID");
            } else {
                System.out.println("Graph \"" + name + "\": INVALID (" + result.errors.size() + " violation(s))");
                for (InvalidGraphError err : result.errors) {
                    System.out.println("  - " + describe(err));
                }
            }
        }
    }

    // A compact, human-readable rendering of a structured graph-validation error.
    // The full structured value is available for programmatic use; this is for display.
    private static String describe(InvalidGraphError err) {
        return err.accept(new InvalidGraphError.Visitor<String>() {
            @Override
            public String visit(InvalidGraphError.Node n) {
                return "node " + n.value.id.value + ": " + describeNode(n.value.error);
            }

            @Override
            public String visit(InvalidGraphError.Relationship r) {
                return "relationship " + r.value.id.value + ": " + describeRel(r.value.error);
            }
        });
    }

    private static String describeNode(hydra.error.neo4j.InvalidNodeError e) {
        return e.accept(new hydra.error.neo4j.InvalidNodeError.Visitor<String>() {
            @Override
            public String visit(hydra.error.neo4j.InvalidNodeError.MissingProperty x) {
                return "missing required property '" + x.value.key.value + "'";
            }

            @Override
            public String visit(hydra.error.neo4j.InvalidNodeError.WrongPropertyType x) {
                return "property '" + x.value.key.value + "' has the wrong type (expected "
                    + valueTypeName(x.value.expectedType) + ")";
            }

            @Override
            public String visit(hydra.error.neo4j.InvalidNodeError.MissingImpliedLabel x) {
                return "missing implied label '" + x.value.label.value + "'";
            }

            @Override
            public String visit(hydra.error.neo4j.InvalidNodeError.NoSuchLabel x) {
                return "no node element type matches the node's labels";
            }

            @Override
            public String visit(hydra.error.neo4j.InvalidNodeError.KeyViolation x) {
                return "key constraint violation";
            }

            @Override
            public String visit(hydra.error.neo4j.InvalidNodeError.UniquenessViolation x) {
                return "uniqueness violation";
            }
        });
    }

    private static String describeRel(hydra.error.neo4j.InvalidRelationshipError e) {
        return e.accept(new hydra.error.neo4j.InvalidRelationshipError.PartialVisitor<String>() {
            @Override
            public String otherwise(hydra.error.neo4j.InvalidRelationshipError x) {
                return x.getClass().getSimpleName();
            }

            @Override
            public String visit(hydra.error.neo4j.InvalidRelationshipError.NoMatchingPattern x) {
                return "endpoints match no declared pattern for this relationship type";
            }

            @Override
            public String visit(hydra.error.neo4j.InvalidRelationshipError.MissingProperty x) {
                return "missing required property '" + x.value.key.value + "'";
            }

            @Override
            public String visit(hydra.error.neo4j.InvalidRelationshipError.WrongPropertyType x) {
                return "property '" + x.value.key.value + "' has the wrong type (expected "
                    + valueTypeName(x.value.expectedType) + ")";
            }

            @Override
            public String visit(hydra.error.neo4j.InvalidRelationshipError.NoSuchType x) {
                return "no relationship element type has this type";
            }
        });
    }

    private static String valueTypeName(hydra.neo4j.model.ValueType vt) {
        return vt.accept(new hydra.neo4j.model.ValueType.PartialVisitor<String>() {
            @Override
            public String otherwise(hydra.neo4j.model.ValueType x) {
                return x.getClass().getSimpleName().replace("_", "").toUpperCase();
            }
        });
    }
}
