package hydra.demos.genpg;

import hydra.Generation;
import hydra.context.Context;
import hydra.core.*;
import hydra.errors.Error_;
import hydra.errors.OtherError;
import hydra.ext.org.w3.rdf.syntax.*;
import hydra.ext.org.w3.shacl.model.*;
import hydra.ext.rdf.Utils;
import hydra.graph.Graph;
import hydra.pg.model.*;
import hydra.pg.rdf.Mappings;
import hydra.pg.rdf.environment.PgRdfEnvironment;
import hydra.relational.RelationName;
import hydra.tabular.Table;
import hydra.tabular.TableType;
import hydra.util.Either;
import hydra.util.Maybe;
import hydra.util.Pair;

import java.io.IOException;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.Function;

/**
 * GenPG RDF Demo - Property Graph to RDF/SHACL conversion (Java).
 * <p>
 * Converts CSV data to a property graph, then outputs:
 * 1. SHACL shapes graph (from schema)
 * 2. RDF instance data (from graph data)
 * 3. Intentionally invalid RDF data (for negative validation)
 */
public class RdfDemo {
    private static final String DEMO_NS = "urn:hydra:genpg:";
    private final Graph graphContext;

    public RdfDemo() {
        this.graphContext = Generation.bootstrapGraph();
    }

    // -----------------------------------------------------------------------
    // Instance-level: PG data → RDF (using generated Mappings)

    private PgRdfEnvironment<Term> makeDefaultEnv() {
        return new PgRdfEnvironment<>(
            v -> termToIri("vertex:", v),
            vl -> new Iri(DEMO_NS + vl.value),
            v -> termToIri("edge:", v),
            el -> new Iri(DEMO_NS + el.value),
            pk -> new Iri(DEMO_NS + pk.value),
            v -> termToLiteral(v));
    }

    private static Iri termToIri(String prefix, Term term) {
        if (term instanceof Term.Literal) {
            hydra.core.Literal lit = ((Term.Literal) term).value;
            if (lit instanceof hydra.core.Literal.String_) {
                return new Iri(DEMO_NS + prefix + ((hydra.core.Literal.String_) lit).value);
            } else if (lit instanceof hydra.core.Literal.Integer_) {
                IntegerValue iv = ((hydra.core.Literal.Integer_) lit).value;
                if (iv instanceof IntegerValue.Int32) {
                    return new Iri(DEMO_NS + prefix + ((IntegerValue.Int32) iv).value);
                }
            }
        }
        throw new RuntimeException("Unsupported term type for IRI encoding: " + term);
    }

    private static hydra.ext.org.w3.rdf.syntax.Literal termToLiteral(Term term) {
        if (term instanceof Term.Literal) {
            return Utils.encodeLiteral(((Term.Literal) term).value);
        }
        throw new RuntimeException("Expected a literal term: " + term);
    }

    // -----------------------------------------------------------------------
    // Schema-level: GraphSchema → SHACL shapes

    private static ShapesGraph graphSchemaToShapesGraph(GraphSchema<Type> schema) {
        List<EdgeType<Type>> allEdgeTypes = new ArrayList<>(schema.edges.values());
        Set<Definition<Shape>> defs = new LinkedHashSet<>();
        for (VertexType<Type> vt : schema.vertices.values()) {
            defs.add(vertexTypeToNodeShape(vt, allEdgeTypes));
        }
        return new ShapesGraph(defs);
    }

    private static Definition<Shape> vertexTypeToNodeShape(VertexType<Type> vt, List<EdgeType<Type>> allEdgeTypes) {
        VertexLabel vlabel = vt.label;
        Set<CommonConstraint> constraints = new LinkedHashSet<>();

        for (PropertyType<Type> pt : vt.properties) {
            PropertyShape ps = propertyTypeToShape(pt);
            Iri scopedIri = new Iri(DEMO_NS + vlabel.value + "#" + pt.key.value);
            constraints.add(new CommonConstraint.Property(
                Collections.singleton(new Reference.Definition<>(new Definition<>(scopedIri, ps)))));
        }

        for (EdgeType<Type> et : allEdgeTypes) {
            if (et.out.equals(vlabel)) {
                Iri edgeIri = new Iri(DEMO_NS + et.label.value);
                Iri inIri = new Iri(DEMO_NS + et.in.value);
                Set<CommonConstraint> edgeConstraints = new LinkedHashSet<>(Arrays.asList(
                    new CommonConstraint.Class_(Collections.singleton(new RdfsClass(null))),
                    new CommonConstraint.Node(Collections.singleton(new Reference.Named<>(inIri)))));
                PropertyShape ps = new PropertyShape(
                    emptyCommon(edgeConstraints, Collections.emptySet()),
                    Collections.emptySet(), Maybe.nothing(), emptyLangStrings(), emptyLangStrings(), Maybe.nothing(), edgeIri);
                Iri scopedIri = new Iri(DEMO_NS + vlabel.value + "#" + et.label.value);
                constraints.add(new CommonConstraint.Property(
                    Collections.singleton(new Reference.Definition<>(new Definition<>(scopedIri, ps)))));
            }
        }

        CommonProperties common = emptyCommon(constraints, Collections.singleton(new RdfsClass(null)));
        return new Definition<>(
            new Iri(DEMO_NS + vlabel.value),
            new Shape.Node(new NodeShape(common)));
    }

    private static PropertyShape propertyTypeToShape(PropertyType<Type> pt) {
        Iri dtIri = typeToXsdIri(pt.value);
        Set<CommonConstraint> constraints = Collections.singleton(new CommonConstraint.Datatype(dtIri));
        Set<PropertyShapeConstraint> propConstraints = pt.required
            ? Collections.singleton(new PropertyShapeConstraint.MinCount(BigInteger.ONE))
            : Collections.emptySet();
        return new PropertyShape(
            emptyCommon(constraints, Collections.emptySet()),
            propConstraints, Maybe.nothing(), emptyLangStrings(), emptyLangStrings(), Maybe.nothing(),
            new Iri(DEMO_NS + pt.key.value));
    }

    private static CommonProperties emptyCommon(Set<CommonConstraint> constraints, Set<RdfsClass> targetClass) {
        return new CommonProperties(constraints, Maybe.nothing(), emptyLangStrings(),
            new Severity.Violation(), targetClass,
            Collections.emptySet(), Collections.emptySet(), Collections.emptySet());
    }

    private static LangStrings emptyLangStrings() {
        return new LangStrings(Collections.emptyMap());
    }

    private static Iri typeToXsdIri(Type typ) {
        String xsd = "http://www.w3.org/2001/XMLSchema#";
        if (typ instanceof Type.Literal) {
            LiteralType lt = ((Type.Literal) typ).value;
            if (lt instanceof LiteralType.Boolean_) return new Iri(xsd + "boolean");
            if (lt instanceof LiteralType.String_) return new Iri(xsd + "string");
            if (lt instanceof LiteralType.Float_) {
                FloatType ft = ((LiteralType.Float_) lt).value;
                if (ft instanceof FloatType.Float32) return new Iri(xsd + "float");
                if (ft instanceof FloatType.Float64) return new Iri(xsd + "double");
                return new Iri(xsd + "decimal");
            }
            if (lt instanceof LiteralType.Integer_) {
                IntegerType it = ((LiteralType.Integer_) lt).value;
                if (it instanceof IntegerType.Int32) return new Iri(xsd + "int");
                if (it instanceof IntegerType.Int64) return new Iri(xsd + "long");
                if (it instanceof IntegerType.Int8) return new Iri(xsd + "byte");
                if (it instanceof IntegerType.Int16) return new Iri(xsd + "short");
                return new Iri(xsd + "integer");
            }
        }
        return new Iri(xsd + "string");
    }

    // -----------------------------------------------------------------------
    // SHACL → N-Triples serialization

    private static String shapesGraphToNtriples(ShapesGraph sg) {
        List<Triple> triples = new ArrayList<>();
        for (Definition<Shape> defn : sg.value) {
            triples.addAll(definitionToTriples(defn));
        }
        return triplesToNtriples(triples);
    }

    private static List<Triple> definitionToTriples(Definition<Shape> defn) {
        Shape shape = defn.target;
        if (shape instanceof Shape.Node) {
            return nodeShapeToTriples(defn.iri, ((Shape.Node) shape).value);
        } else if (shape instanceof Shape.Property) {
            return propertyShapeToTriples(new Resource.Iri(defn.iri), ((Shape.Property) shape).value);
        }
        return Collections.emptyList();
    }

    private static List<Triple> nodeShapeToTriples(Iri iri, NodeShape ns) {
        Resource subj = new Resource.Iri(iri);
        List<Triple> triples = new ArrayList<>();
        triples.add(new Triple(subj, rdf("type"), new Node.Iri(sh("NodeShape"))));
        triples.add(new Triple(subj, sh("targetClass"), new Node.Iri(iri)));
        triples.addAll(commonToTriples(subj, ns.common));
        return triples;
    }

    private static List<Triple> propertyShapeToTriples(Resource subj, PropertyShape ps) {
        List<Triple> triples = new ArrayList<>();
        triples.add(new Triple(subj, rdf("type"), new Node.Iri(sh("PropertyShape"))));
        triples.add(new Triple(subj, sh("path"), new Node.Iri(ps.path)));
        triples.addAll(commonToTriples(subj, ps.common));
        for (PropertyShapeConstraint psc : ps.constraints) {
            triples.addAll(propConstraintToTriples(subj, psc));
        }
        return triples;
    }

    private static List<Triple> commonToTriples(Resource subj, CommonProperties cp) {
        List<Triple> triples = new ArrayList<>();
        for (CommonConstraint cc : cp.constraints) {
            triples.addAll(constraintToTriples(subj, cc));
        }
        return triples;
    }

    private static List<Triple> constraintToTriples(Resource subj, CommonConstraint cc) {
        List<Triple> triples = new ArrayList<>();
        if (cc instanceof CommonConstraint.Datatype) {
            triples.add(new Triple(subj, sh("datatype"),
                new Node.Iri(((CommonConstraint.Datatype) cc).value)));
        } else if (cc instanceof CommonConstraint.Node) {
            for (Reference<NodeShape> ref : ((CommonConstraint.Node) cc).value) {
                if (ref instanceof Reference.Named) {
                    triples.add(new Triple(subj, sh("node"),
                        new Node.Iri(((Reference.Named<NodeShape>) ref).value)));
                }
            }
        } else if (cc instanceof CommonConstraint.Property) {
            for (Reference<PropertyShape> ref : ((CommonConstraint.Property) cc).value) {
                if (ref instanceof Reference.Definition) {
                    Definition<PropertyShape> defn = ((Reference.Definition<PropertyShape>) ref).value;
                    triples.add(new Triple(subj, sh("property"), new Node.Iri(defn.iri)));
                    triples.addAll(propertyShapeToTriples(new Resource.Iri(defn.iri), defn.target));
                } else if (ref instanceof Reference.Named) {
                    triples.add(new Triple(subj, sh("property"),
                        new Node.Iri(((Reference.Named<PropertyShape>) ref).value)));
                }
            }
        }
        return triples;
    }

    private static List<Triple> propConstraintToTriples(Resource subj, PropertyShapeConstraint psc) {
        String xsd = "http://www.w3.org/2001/XMLSchema#";
        if (psc instanceof PropertyShapeConstraint.MinCount) {
            BigInteger n = ((PropertyShapeConstraint.MinCount) psc).value;
            return Collections.singletonList(new Triple(subj, sh("minCount"),
                new Node.Literal(new hydra.ext.org.w3.rdf.syntax.Literal(
                    n.toString(), new Iri(xsd + "integer"), Maybe.nothing()))));
        }
        if (psc instanceof PropertyShapeConstraint.MaxCount) {
            BigInteger n = ((PropertyShapeConstraint.MaxCount) psc).value;
            return Collections.singletonList(new Triple(subj, sh("maxCount"),
                new Node.Literal(new hydra.ext.org.w3.rdf.syntax.Literal(
                    n.toString(), new Iri(xsd + "integer"), Maybe.nothing()))));
        }
        return Collections.emptyList();
    }

    private static Iri rdf(String local) {
        return new Iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#" + local);
    }

    private static Iri sh(String local) {
        return new Iri("http://www.w3.org/ns/shacl#" + local);
    }

    // -----------------------------------------------------------------------
    // N-Triples serialization

    private static String triplesToNtriples(List<Triple> triples) {
        TreeSet<String> lines = new TreeSet<>();
        for (Triple t : triples) {
            lines.add(tripleToNt(t));
        }
        StringBuilder sb = new StringBuilder();
        for (String line : lines) {
            sb.append(line).append("\n");
        }
        return sb.toString();
    }

    private static String graphToNtriples(hydra.ext.org.w3.rdf.syntax.Graph g) {
        return triplesToNtriples(new ArrayList<>(g.value));
    }

    private static String tripleToNt(Triple t) {
        return resourceToNt(t.subject) + " " + iriToNt(t.predicate) + " " + nodeToNt(t.object) + " .";
    }

    private static String resourceToNt(Resource r) {
        if (r instanceof Resource.Iri) return iriToNt(((Resource.Iri) r).value);
        if (r instanceof Resource.Bnode) return "_:" + ((Resource.Bnode) r).value.value;
        return "";
    }

    private static String iriToNt(Iri iri) {
        return "<" + iri.value + ">";
    }

    private static String nodeToNt(Node n) {
        if (n instanceof Node.Iri) return iriToNt(((Node.Iri) n).value);
        if (n instanceof Node.Bnode) return "_:" + ((Node.Bnode) n).value.value;
        if (n instanceof Node.Literal) {
            hydra.ext.org.w3.rdf.syntax.Literal lit = ((Node.Literal) n).value;
            String escaped = lit.lexicalForm.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n");
            return "\"" + escaped + "\"^^" + iriToNt(lit.datatypeIri);
        }
        return "";
    }

    // -----------------------------------------------------------------------
    // Invalid data

    private static String generateInvalidData() {
        String xsd = "http://www.w3.org/2001/XMLSchema#";
        List<Triple> triples = new ArrayList<>();
        // Employee missing required firstName and lastName
        triples.add(new Triple(res("vertex:invalid_emp"), rdf("type"), iriNode("Employee")));
        triples.add(new Triple(res("vertex:invalid_emp"), iri("email"),
            litNode("bad@example.com", xsd + "string")));
        // Product with integer name
        triples.add(new Triple(res("vertex:invalid_prod"), rdf("type"), iriNode("Product")));
        triples.add(new Triple(res("vertex:invalid_prod"), iri("name"),
            litNode("42", xsd + "integer")));
        return triplesToNtriples(triples);
    }

    private static Resource res(String local) {
        return new Resource.Iri(new Iri(DEMO_NS + local));
    }

    private static Iri iri(String local) {
        return new Iri(DEMO_NS + local);
    }

    private static Node iriNode(String local) {
        return new Node.Iri(new Iri(DEMO_NS + local));
    }

    private static Node litNode(String value, String datatype) {
        return new Node.Literal(new hydra.ext.org.w3.rdf.syntax.Literal(
            value, new Iri(datatype), Maybe.nothing()));
    }

    // -----------------------------------------------------------------------
    // CSV-to-PG pipeline (reused from Demo.java)

    private Table<Term> decodeTableIo(TableType tableType, Path path) throws IOException {
        List<String> rawLines = Files.readAllLines(path, StandardCharsets.UTF_8);
        Either<String, Table<String>> parseResult = Transform.parseTableLines(true, rawLines);
        if (parseResult.isLeft()) {
            throw new RuntimeException("CSV read error in " + path + ": " + ((Either.Left<String, Table<String>>) parseResult).value);
        }
        Table<String> table = ((Either.Right<String, Table<String>>) parseResult).value;
        Either<String, Table<Term>> decodeResult = Transform.decodeTable(tableType, table);
        if (decodeResult.isLeft()) {
            throw new RuntimeException("Decode error: " + ((Either.Left<String, Table<Term>>) decodeResult).value);
        }
        return ((Either.Right<String, Table<Term>>) decodeResult).value;
    }

    private LazyGraph<Term> transformTables(Path sourceRoot, List<TableType> tableTypes, LazyGraph<Term> spec) throws IOException {
        Either<String, Map<String, Pair<List<Vertex<Term>>, List<Edge<Term>>>>> specsResult =
            Transform.elementSpecsByTable(spec);
        if (specsResult.isLeft()) {
            throw new RuntimeException("Error in mapping specification: " + ((Either.Left<String, ?>) specsResult).value);
        }
        Map<String, Pair<List<Vertex<Term>>, List<Edge<Term>>>> byTable =
            ((Either.Right<String, Map<String, Pair<List<Vertex<Term>>, List<Edge<Term>>>>>) specsResult).value;
        Map<RelationName, TableType> tblTypesByName = Transform.tableTypesByName(tableTypes);

        List<Vertex<Term>> allVertices = new ArrayList<>();
        List<Edge<Term>> allEdges = new ArrayList<>();

        for (Map.Entry<String, Pair<List<Vertex<Term>>, List<Edge<Term>>>> entry : byTable.entrySet()) {
            String tname = entry.getKey();
            TableType tableType = tblTypesByName.get(new RelationName(tname));
            if (tableType == null) throw new RuntimeException("Table not found: " + tname);
            Table<Term> table = decodeTableIo(tableType, sourceRoot.resolve(tname));
            Context cx = new Context(Collections.emptyList(), Collections.emptyList(), Collections.emptyMap());
            Either<Error_, Pair<List<Vertex<Term>>, List<Edge<Term>>>> result =
                Transform.transformTableRows(cx, graphContext, entry.getValue().first, entry.getValue().second, tableType, table.data);
            if (result.isLeft()) throw new RuntimeException("Transform error");
            Pair<List<Vertex<Term>>, List<Edge<Term>>> pair = ((Either.Right<Error_, Pair<List<Vertex<Term>>, List<Edge<Term>>>>) result).value;
            allVertices.addAll(pair.first);
            allEdges.addAll(pair.second);
        }
        return Transform.makeLazyGraph(allVertices, allEdges);
    }

    // -----------------------------------------------------------------------
    // Entry points

    public void generateRdf(Path sourceRoot, List<TableType> tableSchemas,
            LazyGraph<Term> graphMapping, GraphSchema<Type> graphSchema, Path outputDir) throws IOException {
        System.out.println("Reading CSV files from " + sourceRoot + "/");
        long startTime = System.nanoTime();

        LazyGraph<Term> lg = transformTables(sourceRoot, tableSchemas, graphMapping);

        // SHACL shapes
        System.out.println("Generating SHACL shapes...");
        ShapesGraph shapes = graphSchemaToShapesGraph(graphSchema);
        String shapesNt = shapesGraphToNtriples(shapes);
        Path shapesFile = Paths.get(outputDir + "-shapes.nt");
        Files.createDirectories(shapesFile.getParent());
        Files.write(shapesFile, shapesNt.getBytes(StandardCharsets.UTF_8));
        System.out.println("  Wrote shapes to " + shapesFile);

        // RDF data
        System.out.println("Encoding property graph as RDF...");
        PgRdfEnvironment<Term> env = makeDefaultEnv();
        List<Description> vertexDescs = new ArrayList<>();
        for (Vertex<Term> v : lg.vertices) {
            vertexDescs.add(Mappings.encodeVertex(env, v));
        }
        List<Description> edgeDescs = new ArrayList<>();
        for (Edge<Term> e : lg.edges) {
            edgeDescs.add(Mappings.encodeEdge(env, e));
        }
        List<Description> allDescs = new ArrayList<>(vertexDescs);
        allDescs.addAll(edgeDescs);
        hydra.ext.org.w3.rdf.syntax.Graph dataGraph = Utils.descriptionsToGraph(allDescs);
        String dataNt = graphToNtriples(dataGraph);
        Path dataFile = Paths.get(outputDir + "-data.nt");
        Files.write(dataFile, dataNt.getBytes(StandardCharsets.UTF_8));
        System.out.println("  Wrote " + vertexDescs.size() + " vertex and " + edgeDescs.size() + " edge descriptions to " + dataFile);

        // Invalid data
        System.out.println("Generating non-conforming RDF data...");
        String invalidNt = generateInvalidData();
        Path invalidFile = Paths.get(outputDir + "-invalid.nt");
        Files.write(invalidFile, invalidNt.getBytes(StandardCharsets.UTF_8));
        System.out.println("  Wrote to " + invalidFile);

        long elapsedNs = System.nanoTime() - startTime;
        System.out.println("Done.");
        System.err.println("HYDRA_TIME_MS=" + (elapsedNs / 1_000_000.0));
    }

    private static Path findHydraExtRoot() {
        Path cwd = Paths.get("").toAbsolutePath();
        if (Files.exists(cwd.resolve("demos/genpg"))) return cwd;
        Path child = cwd.resolve("hydra-ext");
        if (Files.exists(child.resolve("demos/genpg"))) return child;
        throw new RuntimeException("Cannot find hydra-ext root.");
    }

    public void generateSalesRdf() throws IOException {
        Path root = findHydraExtRoot();
        generateRdf(
            root.resolve("demos/genpg/data/sources/sales"),
            Sales.salesDatabaseSchema(),
            Sales.salesMapping(),
            Sales.salesGraphSchema(),
            root.resolve("demos/genpg/output/sales"));
    }

    public void generateHealthRdf() throws IOException {
        Path root = findHydraExtRoot();
        generateRdf(
            root.resolve("demos/genpg/data/sources/health"),
            Health.healthDatabaseSchema(),
            Health.healthMapping(),
            Health.healthGraphSchema(),
            root.resolve("demos/genpg/output/health"));
    }

    public static void main(String[] args) throws Exception {
        String dataset = args.length > 0 ? args[0] : "sales";
        String outputPrefix = args.length > 1 ? args[1] : null;
        RdfDemo demo = new RdfDemo();
        Path root = findHydraExtRoot();
        Path sourceRoot = root.resolve("demos/genpg/data/sources/" + dataset);
        Path outputDir = outputPrefix != null
            ? Paths.get(outputPrefix)
            : root.resolve("demos/genpg/output/" + dataset);
        switch (dataset) {
            case "sales":
                demo.generateRdf(sourceRoot, Sales.salesDatabaseSchema(), Sales.salesMapping(),
                    Sales.salesGraphSchema(), outputDir);
                break;
            case "health":
                demo.generateRdf(sourceRoot, Health.healthDatabaseSchema(), Health.healthMapping(),
                    Health.healthGraphSchema(), outputDir);
                break;
            default:
                System.err.println("Unknown dataset: " + dataset);
                System.exit(1);
        }
    }
}
