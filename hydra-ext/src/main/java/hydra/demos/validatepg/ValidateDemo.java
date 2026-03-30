package hydra.demos.validatepg;

import hydra.Generation;
import hydra.core.Literal;
import hydra.core.LiteralType;
import hydra.dsl.LiteralTypes;
import hydra.dsl.Literals;
import hydra.json.model.Value;
import hydra.pg.model.Graph;
import hydra.pg.model.GraphSchema;
import hydra.error.pg.InvalidGraphError;
import hydra.error.pg.InvalidValueError;
import hydra.validate.Pg;
import hydra.util.Maybe;

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

    // Checks a literal value against a literal type.
    // Compares the type family (string vs string, integer.int32 vs integer.int32, etc.)
    private static final Function<LiteralType, Function<Literal, Maybe<InvalidValueError>>>
            CHECK_LITERAL = type -> value -> {
        String expected = LiteralTypes.showLiteralType(type);
        String actual = literalFamily(value);
        if (expected.equals(actual)) {
            return Maybe.nothing();
        }
        return Maybe.just(new InvalidValueError(expected, Literals.showLiteral(value)));
    };

    private static String literalFamily(Literal lit) {
        return lit.accept(new Literal.Visitor<String>() {
            @Override public String visit(Literal.Binary instance) { return "binary"; }
            @Override public String visit(Literal.Boolean_ instance) { return "boolean"; }
            @Override public String visit(Literal.Float_ instance) {
                return "float:" + instance.value.accept(new hydra.core.FloatValue.Visitor<String>() {
                    @Override public String visit(hydra.core.FloatValue.Bigfloat i) { return "bigfloat"; }
                    @Override public String visit(hydra.core.FloatValue.Float32 i) { return "float32"; }
                    @Override public String visit(hydra.core.FloatValue.Float64 i) { return "float64"; }
                });
            }
            @Override public String visit(Literal.Integer_ instance) {
                return "integer:" + instance.value.accept(new hydra.core.IntegerValue.Visitor<String>() {
                    @Override public String visit(hydra.core.IntegerValue.Bigint i) { return "bigint"; }
                    @Override public String visit(hydra.core.IntegerValue.Int8 i) { return "int8"; }
                    @Override public String visit(hydra.core.IntegerValue.Int16 i) { return "int16"; }
                    @Override public String visit(hydra.core.IntegerValue.Int32 i) { return "int32"; }
                    @Override public String visit(hydra.core.IntegerValue.Int64 i) { return "int64"; }
                    @Override public String visit(hydra.core.IntegerValue.Uint8 i) { return "uint8"; }
                    @Override public String visit(hydra.core.IntegerValue.Uint16 i) { return "uint16"; }
                    @Override public String visit(hydra.core.IntegerValue.Uint32 i) { return "uint32"; }
                    @Override public String visit(hydra.core.IntegerValue.Uint64 i) { return "uint64"; }
                });
            }
            @Override public String visit(Literal.String_ instance) { return "string"; }
        });
    }

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

            Maybe<InvalidGraphError<Literal>> validationResult = Pg.validateGraph(
                    CHECK_LITERAL,
                    schema,
                    graph);

            if (validationResult.isJust()) {
                System.out.println("Graph \"" + name + "\": INVALID - " + validationResult.fromJust());
            } else {
                System.out.println("Graph \"" + name + "\": VALID");
            }
        }

        long elapsedNs = System.nanoTime() - startTime;
        System.err.println("HYDRA_TIME_MS=" + (elapsedNs / 1_000_000.0));
    }
}
