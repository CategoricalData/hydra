package hydra.demos.validatepg;

import hydra.Generation;
import hydra.core.Literal;
import hydra.core.LiteralType;
import hydra.json.model.Value;
import hydra.pg.model.Graph;
import hydra.pg.model.GraphSchema;
import hydra.error.pg.InvalidGraphError;
import hydra.error.pg.InvalidValueError;
import hydra.show.Core;
import hydra.validate.Pg;
import hydra.util.Maybe;
import hydra.validation.ValidationResult;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Function;

/**
 * Java driver for the PG validation translingual demo.
 *
 * <p>Reads a schema JSON file and one or more graph JSON files (produced by GenerateData using
 * hydra.encode.pg.model), validates each graph against the schema using hydra.validate.pg,
 * and prints the results.
 *
 * <p>Usage: java hydra.demos.validatepg.ValidateDemo &lt;data-directory&gt;
 */
public class ValidateDemo {

    // Adapter from hydra.validate.core.checkLiteral (typed InvalidLiteralError) to the
    // stringified InvalidValueError shape that hydra.validate.pg.validateGraph expects.
    private static final Function<LiteralType, Function<Literal, Maybe<InvalidValueError>>>
            CHECK_LITERAL = type -> value -> {
        Maybe<hydra.error.core.InvalidLiteralError> result = hydra.validate.Core.checkLiteral(type, value);
        if (!result.isJust()) {
            return Maybe.nothing();
        }
        return Maybe.just(new InvalidValueError(Core.literalType(type), Core.literal(value)));
    };

    public static void main(String[] args) throws IOException {
        if (args.length < 1) {
            System.err.println("Usage: java hydra.demos.validatepg.ValidateDemo <data-directory>");
            System.exit(1);
        }

        Path dataDir = Paths.get(args[0]);

        // Read and decode schema
        Value schemaJson = Generation.parseJsonFile(dataDir.resolve("schema.json").toString());
        GraphSchema<LiteralType> schema = JsonPgDecoder.decodeGraphSchema(schemaJson);

        // Validate each graph file
        String[] graphFiles = {
                "valid_social_network",
                "missing_required_property",
                "wrong_id_type",
                "unknown_edge_endpoint",
                "unexpected_vertex_label",
                "unexpected_edge_label",
                "property_value_type_mismatch",
                "unexpected_property_key",
                "wrong_in_vertex_label",
                "wrong_out_vertex_label",
                "missing_required_edge_property",
        };

        // Read all graph files (I/O, not timed)
        java.util.List<String> names = new java.util.ArrayList<>();
        java.util.List<Graph<Literal>> graphs = new java.util.ArrayList<>();
        for (String name : graphFiles) {
            Path graphPath = dataDir.resolve(name + ".json");
            if (!Files.exists(graphPath)) {
                continue;
            }
            Value graphJson = Generation.parseJsonFile(graphPath.toString());
            names.add(name);
            graphs.add(JsonPgDecoder.decodeGraph(graphJson));
        }

        // Validate (timed: Hydra computation only)
        long startTime = System.nanoTime();

        for (int i = 0; i < names.size(); i++) {
            String name = names.get(i);
            Graph<Literal> graph = graphs.get(i);

            ValidationResult<InvalidGraphError<Literal>> validationResult = Pg.validateGraph(
                    Pg.defaultPgProfile(),
                    new ValidationResult<InvalidGraphError<Literal>>(java.util.Collections.emptyList(), java.util.Collections.emptyList()),
                    CHECK_LITERAL,
                    schema,
                    graph);

            if (!validationResult.errors.isEmpty()) {
                System.out.println("Graph \"" + name + "\": INVALID - " + validationResult.errors.get(0));
            } else {
                System.out.println("Graph \"" + name + "\": VALID");
            }
        }

        long elapsedNs = System.nanoTime() - startTime;
        System.err.println("HYDRA_TIME_MS=" + (elapsedNs / 1_000_000.0));
    }
}
