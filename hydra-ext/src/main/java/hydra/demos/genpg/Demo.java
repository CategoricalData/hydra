package hydra.demos.genpg;

import hydra.Generation;
import hydra.compute.Flow;
import hydra.core.Term;
import hydra.dsl.Flows;
import hydra.graph.Graph;
import hydra.json.writer.Writer;
import hydra.pg.model.Edge;
import hydra.pg.model.Element;
import hydra.pg.model.LazyGraph;
import hydra.pg.model.Vertex;
import hydra.relational.RelationName;
import hydra.tabular.Table;
import hydra.tabular.TableType;
import hydra.tools.FlowException;
import hydra.util.Either;
import hydra.util.Tuple.Tuple2;

import hydra.demos.genpg.transform.Transform;
import hydra.demos.genpg.sales.Sales;
import hydra.demos.genpg.health.Health;
import hydra.pg.graphson.utils.Utils;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;
import java.util.stream.Collectors;


/**
 * GenPG Demo - Property Graph Generation from CSV Tables (Java).
 *
 * This is the Java equivalent of demo.py / Demo.hs.
 * It demonstrates end-to-end transformation of relational CSV data into
 * a property graph in GraphSON format.
 *
 * Usage:
 *     java hydra.demos.genpg.Demo sales
 *     java hydra.demos.genpg.Demo health
 */
public class Demo {

    private final Graph graphContext;

    public Demo() {
        this.graphContext = Generation.bootstrapGraph();
    }

    /**
     * Read and decode a CSV file into a Table of Terms.
     */
    public Table<Term> decodeTableIo(TableType tableType, Path path) throws IOException {
        List<String> rawLines = Files.readAllLines(path, StandardCharsets.UTF_8);

        Either<String, Table<String>> parseResult = Transform.parseTableLines(true, rawLines);
        if (parseResult.isLeft()) {
            throw new RuntimeException(
                "CSV read error in " + path + ": " + ((Either.Left<String, Table<String>>) parseResult).value);
        }
        Table<String> table = ((Either.Right<String, Table<String>>) parseResult).value;

        Either<String, Table<Term>> decodeResult = Transform.decodeTable(tableType, table);
        if (decodeResult.isLeft()) {
            throw new RuntimeException(
                "Decode error: " + ((Either.Left<String, Table<Term>>) decodeResult).value);
        }
        return ((Either.Right<String, Table<Term>>) decodeResult).value;
    }

    /**
     * Transform a table by reading from a file and applying vertex/edge specifications.
     */
    public Tuple2<List<Vertex<Term>>, List<Edge<Term>>> transformTable(
            TableType tableType,
            Path path,
            List<Vertex<Term>> vspecs,
            List<Edge<Term>> especs) throws IOException, FlowException {
        Table<Term> table = decodeTableIo(tableType, path);
        Flow<Graph, Tuple2<List<Vertex<Term>>, List<Edge<Term>>>> flow =
            Transform.transformTableRows(vspecs, especs, tableType, table.data);
        return Flows.fromFlow(graphContext, flow);
    }

    /**
     * Transform multiple tables according to a graph mapping specification.
     */
    public LazyGraph<Term> transformTables(
            Path sourceRoot,
            List<TableType> tableTypes,
            LazyGraph<Term> spec) throws IOException, FlowException {
        Either<String, Map<String, Tuple2<List<Vertex<Term>>, List<Edge<Term>>>>> specsResult =
            Transform.elementSpecsByTable(spec);
        if (specsResult.isLeft()) {
            throw new RuntimeException(
                "Error in mapping specification: " + ((Either.Left<String, ?>) specsResult).value);
        }
        Map<String, Tuple2<List<Vertex<Term>>, List<Edge<Term>>>> byTable =
            ((Either.Right<String, Map<String, Tuple2<List<Vertex<Term>>, List<Edge<Term>>>>>) specsResult).value;

        Map<RelationName, TableType> tblTypesByName = Transform.tableTypesByName(tableTypes);

        List<Vertex<Term>> allVertices = new ArrayList<>();
        List<Edge<Term>> allEdges = new ArrayList<>();

        for (Map.Entry<String, Tuple2<List<Vertex<Term>>, List<Edge<Term>>>> entry : byTable.entrySet()) {
            String tname = entry.getKey();
            List<Vertex<Term>> vspecs = entry.getValue().object1;
            List<Edge<Term>> especs = entry.getValue().object2;

            RelationName relName = new RelationName(tname);
            TableType tableType = tblTypesByName.get(relName);
            if (tableType == null) {
                throw new RuntimeException("Table specified in mapping does not exist: " + tname);
            }

            Path path = sourceRoot.resolve(tname);
            Tuple2<List<Vertex<Term>>, List<Edge<Term>>> result =
                transformTable(tableType, path, vspecs, especs);
            allVertices.addAll(result.object1);
            allEdges.addAll(result.object2);
        }

        return Transform.makeLazyGraph(allVertices, allEdges);
    }

    /**
     * Generate GraphSON output from CSV sources.
     */
    public void generateGraphson(
            Path sourceRoot,
            List<TableType> tableSchemas,
            LazyGraph<Term> graphMapping,
            Path outputPath) throws IOException, FlowException {
        System.out.println("Reading CSV files from " + sourceRoot + "/");
        StringJoiner tableNames = new StringJoiner(", ");
        for (TableType t : tableSchemas) {
            tableNames.add(t.name.value);
        }
        System.out.println("  Tables: " + tableNames);

        LazyGraph<Term> g = transformTables(sourceRoot, tableSchemas, graphMapping);
        List<Element<Term>> els = lazyGraphToElements(g);

        long vertexCount = els.stream().filter(e -> e instanceof Element.Vertex).count();
        long edgeCount = els.stream().filter(e -> e instanceof Element.Edge).count();

        System.out.println("Transforming to property graph...");
        System.out.println("  Vertices: " + vertexCount);
        System.out.println("  Edges: " + edgeCount);

        System.out.println("Writing GraphSON to " + outputPath);
        Flow<Graph, List<hydra.json.model.Value>> flow = Utils.pgElementsToGraphson(
            v -> Utils.encodeTermValue(v), els);
        List<hydra.json.model.Value> jsonValues = Flows.fromFlow(graphContext, flow);

        List<String> jsonStrings = jsonValues.stream()
            .map(Writer::printJson)
            .collect(Collectors.toList());

        Files.createDirectories(outputPath.getParent());
        Files.write(outputPath, (String.join("\n", jsonStrings) + "\n").getBytes(StandardCharsets.UTF_8));

        System.out.println("Done. Output written to " + outputPath);
    }

    /**
     * Convert a lazy graph to a list of elements.
     */
    private static List<Element<Term>> lazyGraphToElements(LazyGraph<Term> graph) {
        List<Element<Term>> elements = new ArrayList<>();
        for (Vertex<Term> v : graph.vertices) {
            elements.add(new Element.Vertex<>(v));
        }
        for (Edge<Term> e : graph.edges) {
            elements.add(new Element.Edge<>(e));
        }
        return elements;
    }

    /**
     * Resolve the hydra-ext root directory from the location of this class.
     */
    private static Path findHydraExtRoot() {
        // Try the standard project layout: running from the repo root or hydra-ext
        Path cwd = Paths.get("").toAbsolutePath();
        if (Files.exists(cwd.resolve("demos/genpg"))) {
            return cwd;
        }
        Path hydraExtChild = cwd.resolve("hydra-ext");
        if (Files.exists(hydraExtChild.resolve("demos/genpg"))) {
            return hydraExtChild;
        }
        throw new RuntimeException(
            "Cannot find hydra-ext root. Run from the repository root or hydra-ext directory.");
    }

    public void generateSalesGraphson() throws IOException, FlowException {
        Path root = findHydraExtRoot();
        List<TableType> tableSchemas = Sales.salesDatabaseSchema();
        LazyGraph<Term> graph = Sales.salesMapping();

        generateGraphson(
            root.resolve("demos/genpg/data/sources/sales"),
            tableSchemas,
            graph,
            root.resolve("demos/genpg/output/sales.jsonl"));
    }

    public void generateHealthGraphson() throws IOException, FlowException {
        Path root = findHydraExtRoot();
        List<TableType> tableSchemas = Health.healthDatabaseSchema();
        LazyGraph<Term> graph = Health.healthMapping();

        generateGraphson(
            root.resolve("demos/genpg/data/sources/health"),
            tableSchemas,
            graph,
            root.resolve("demos/genpg/output/health.jsonl"));
    }

    public static void main(String[] args) throws Exception {
        String dataset = args.length > 0 ? args[0] : "sales";
        Demo demo = new Demo();

        switch (dataset) {
            case "sales":
                demo.generateSalesGraphson();
                break;
            case "health":
                demo.generateHealthGraphson();
                break;
            default:
                System.err.println("Unknown dataset: " + dataset + ". Use 'sales' or 'health'.");
                System.exit(1);
        }
    }
}
