package hydra.demos.neo4jvalidation;

import hydra.core.Term;
import hydra.encode.neo4j.Model;
import hydra.json.Encode;
import hydra.json.Writer;
import hydra.json.model.Value;
import hydra.neo4j.model.ElementId;
import hydra.neo4j.model.Key;
import hydra.neo4j.model.Node;
import hydra.neo4j.model.NodeLabel;
import hydra.neo4j.model.Relationship;
import hydra.neo4j.model.RelationshipType;
import hydra.overlay.java.util.Either;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Reads a Neo4j Cypher {@code CREATE} script and converts it into Hydra's Neo4j model, emitting the
 * graph as Hydra term-JSON for the translingual validators to consume.
 *
 * <p>The point of this tool is that a Neo4j user need not know Hydra at all: they write the same
 * Cypher they would type into {@code cypher-shell} (see {@code fixture.cypher}), and this turns it
 * into the portable JSON artifact that every host then validates identically. The Hydra JSON is an
 * intermediate the user can inspect, but never has to author.
 *
 * <p>This is a deliberately small reader for the {@code CREATE}-statement subset used to seed graph
 * data: node patterns {@code (var:Label {k: v, ...})} and relationship patterns
 * {@code (a)-[:TYPE {k: v}]->(b)} / {@code (a)<-[:TYPE]-(b)}, with string, integer, float, boolean,
 * and list-of-literal property values. It is not a full openCypher parser (the project ships one of
 * those as a Java overlay, {@code hydra.overlay.java.cypher.CypherReader}); it covers exactly what a
 * data-seeding script needs, with no third-party dependencies, so the demo runs anywhere.
 *
 * <p>Usage: java hydra.demos.neo4jvalidation.CypherIngest &lt;input.cypher&gt; &lt;output.json&gt;
 */
public class CypherIngest {

    public static void main(String[] args) throws IOException {
        if (args.length < 2) {
            System.err.println("Usage: java hydra.demos.neo4jvalidation.CypherIngest <input.cypher> <output.json>");
            System.exit(1);
        }
        Path input = Paths.get(args[0]);
        Path output = Paths.get(args[1]);

        String cypher = Files.readString(input, StandardCharsets.UTF_8);
        Graph graph = parse(cypher);

        if (graph.nodes.isEmpty() && graph.relationships.isEmpty()) {
            // An empty graph validates trivially; that is almost always a sign the input had no
            // CREATE statements (or none this reader understood), not a graph the user meant to check.
            System.err.println("WARNING: " + input + " yielded an empty graph (no CREATE data found). "
                + "This reader handles CREATE statements that seed nodes and relationships; "
                + "MATCH/MERGE/other clauses are ignored.");
        }

        Files.write(output, encodeGraphToJson(graph).getBytes(StandardCharsets.UTF_8));
        System.out.println("Ingested " + input + " -> " + output
            + " (" + graph.nodes.size() + " nodes, " + graph.relationships.size() + " relationships)");
    }

    // A nodes/relationships pair (matches the JSON the validators read).
    private record Graph(List<Node> nodes, List<Relationship> relationships) {}

    // ------------------------------------------------------------------------
    // Parsing
    // ------------------------------------------------------------------------

    private static Graph parse(String cypher) {
        String src = stripComments(cypher);
        Map<String, Node> nodesByVar = new LinkedHashMap<>();
        List<Node> nodes = new ArrayList<>();
        List<Relationship> rels = new ArrayList<>();
        int[] relCounter = {0};

        // Process each statement (statements are separated by ';').
        for (String stmt : src.split(";")) {
            String s = stmt.trim();
            if (s.isEmpty()) {
                continue;
            }
            // Only CREATE statements seed data; skip MATCH/DELETE/etc.
            String upper = s.toUpperCase();
            int createIdx = upper.indexOf("CREATE");
            if (createIdx < 0) {
                continue;
            }
            String body = s.substring(createIdx + "CREATE".length());
            // A CREATE body is a comma-separated list of patterns, but commas can appear inside
            // property maps and lists, so split at top level only.
            for (String pattern : splitTopLevel(body)) {
                parsePattern(pattern.trim(), nodesByVar, nodes, rels, relCounter);
            }
        }
        return new Graph(nodes, rels);
    }

    /** Parse one pattern: a single node, or a node-relationship-node chain. */
    private static void parsePattern(String pattern, Map<String, Node> nodesByVar,
                                     List<Node> nodes, List<Relationship> rels, int[] relCounter) {
        if (pattern.isEmpty()) {
            return;
        }
        // Tokenize into alternating node "(...)" segments and relationship segments.
        List<String> nodeSegs = new ArrayList<>();
        List<String> relSegs = new ArrayList<>();
        int i = 0;
        while (i < pattern.length()) {
            char c = pattern.charAt(i);
            if (c == '(') {
                int close = matchParen(pattern, i);
                nodeSegs.add(pattern.substring(i, close + 1));
                i = close + 1;
            } else if (c == '-' || c == '<') {
                // A relationship segment runs up to the start of the next node '('.
                int next = pattern.indexOf('(', i);
                if (next < 0) {
                    next = pattern.length();
                }
                relSegs.add(pattern.substring(i, next));
                i = next;
            } else {
                i++;
            }
        }

        // Resolve each node segment (declaring new nodes, reusing variables).
        List<String> nodeVars = new ArrayList<>();
        for (String seg : nodeSegs) {
            nodeVars.add(resolveNode(seg, nodesByVar, nodes));
        }

        // Each relationship segment connects the surrounding two nodes.
        for (int r = 0; r < relSegs.size(); r++) {
            String relSeg = relSegs.get(r);
            String leftVar = nodeVars.get(r);
            String rightVar = nodeVars.get(r + 1);
            parseRelationship(relSeg, leftVar, rightVar, nodesByVar, rels, relCounter);
        }
    }

    /**
     * Resolve a node segment "(var:Label {props})" to its element id (variable name), declaring the
     * node if its variable is new. A bare "(var)" reference reuses an already-declared node.
     */
    private static String resolveNode(String seg, Map<String, Node> nodesByVar, List<Node> nodes) {
        String inner = seg.substring(1, seg.length() - 1).trim();  // strip ( )
        // Split off the property map (if any).
        String head = inner;
        Map<Key, hydra.neo4j.model.Value> props = new LinkedHashMap<>();
        int brace = inner.indexOf('{');
        if (brace >= 0) {
            head = inner.substring(0, brace).trim();
            int close = matchBrace(inner, brace);
            props = parsePropertyMap(inner.substring(brace, close + 1));
        }
        // head is "var:Label1:Label2" or "var" or ":Label".
        String var;
        Set<NodeLabel> labels = new LinkedHashSet<>();
        int colon = head.indexOf(':');
        if (colon < 0) {
            var = head.trim();
        } else {
            var = head.substring(0, colon).trim();
            for (String lab : head.substring(colon + 1).split(":")) {
                if (!lab.trim().isEmpty()) {
                    labels.add(new NodeLabel(lab.trim()));
                }
            }
        }
        if (var.isEmpty()) {
            // Anonymous node: synthesize an id.
            var = "_anon" + nodes.size();
        }
        // A bare reference (no labels, no props) to an existing variable reuses it.
        if (labels.isEmpty() && props.isEmpty() && nodesByVar.containsKey(var)) {
            return var;
        }
        Node node = new Node(new ElementId(var), labels, props);
        nodesByVar.put(var, node);
        nodes.add(node);
        return var;
    }

    /** Parse a relationship segment like "-[:TYPE {props}]->" or "<-[r:TYPE]-". */
    private static void parseRelationship(String relSeg, String leftVar, String rightVar,
                                          Map<String, Node> nodesByVar, List<Relationship> rels,
                                          int[] relCounter) {
        boolean leftArrow = relSeg.contains("<");
        boolean rightArrow = relSeg.contains(">");
        String type = "";
        Map<Key, hydra.neo4j.model.Value> props = new LinkedHashMap<>();
        int lb = relSeg.indexOf('[');
        if (lb >= 0) {
            int rb = matchBracket(relSeg, lb);
            String detail = relSeg.substring(lb + 1, rb).trim();
            int brace = detail.indexOf('{');
            String head = brace >= 0 ? detail.substring(0, brace).trim() : detail;
            if (brace >= 0) {
                props = parsePropertyMap(detail.substring(brace, matchBrace(detail, brace) + 1));
            }
            // head is "var:TYPE" or ":TYPE" or "var".
            int colon = head.indexOf(':');
            if (colon >= 0) {
                type = head.substring(colon + 1).trim();
            }
        }
        // Direction: default left->right; "<-...-" reverses.
        String start = leftVar;
        String end = rightVar;
        if (leftArrow && !rightArrow) {
            start = rightVar;
            end = leftVar;
        }
        String id = "_r" + (relCounter[0]++);
        rels.add(new Relationship(
            new ElementId(id), props, new RelationshipType(type),
            new ElementId(start), new ElementId(end)));
    }

    /** Parse a "{k: v, k2: v2}" property map. */
    private static Map<Key, hydra.neo4j.model.Value> parsePropertyMap(String map) {
        Map<Key, hydra.neo4j.model.Value> result = new LinkedHashMap<>();
        String inner = map.substring(1, map.length() - 1).trim();  // strip { }
        if (inner.isEmpty()) {
            return result;
        }
        for (String pair : splitTopLevel(inner)) {
            int colon = topLevelColon(pair);
            if (colon < 0) {
                continue;
            }
            String key = pair.substring(0, colon).trim();
            String val = pair.substring(colon + 1).trim();
            result.put(new Key(unquoteKey(key)), parseValue(val));
        }
        return result;
    }

    /** Parse a literal value: string, integer, float, boolean, or list of literals. */
    private static hydra.neo4j.model.Value parseValue(String v) {
        v = v.trim();
        if (v.startsWith("'") || v.startsWith("\"")) {
            return new hydra.neo4j.model.Value.String_(unquote(v));
        }
        if (v.startsWith("[")) {
            List<hydra.neo4j.model.Value> items = new ArrayList<>();
            String inner = v.substring(1, v.length() - 1).trim();
            if (!inner.isEmpty()) {
                for (String item : splitTopLevel(inner)) {
                    items.add(parseValue(item.trim()));
                }
            }
            return new hydra.neo4j.model.Value.List(items);
        }
        if (v.equalsIgnoreCase("true") || v.equalsIgnoreCase("false")) {
            return new hydra.neo4j.model.Value.Boolean_(Boolean.parseBoolean(v));
        }
        if (v.matches("[+-]?\\d+")) {
            return new hydra.neo4j.model.Value.Integer_(Long.parseLong(v));
        }
        if (v.matches("[+-]?\\d*\\.\\d+([eE][+-]?\\d+)?")) {
            return new hydra.neo4j.model.Value.Float_(Double.parseDouble(v));
        }
        // Fall back to a string for anything else (e.g. an unquoted token).
        return new hydra.neo4j.model.Value.String_(v);
    }

    // ------------------------------------------------------------------------
    // Small lexical helpers (brace/paren/quote aware)
    // ------------------------------------------------------------------------

    private static String stripComments(String s) {
        // Remove // line comments (Cypher style), respecting string literals.
        StringBuilder out = new StringBuilder();
        String[] lines = s.split("\n", -1);
        for (String line : lines) {
            int idx = indexOfLineComment(line);
            out.append(idx >= 0 ? line.substring(0, idx) : line).append('\n');
        }
        return out.toString();
    }

    private static int indexOfLineComment(String line) {
        boolean inStr = false;
        char q = 0;
        for (int i = 0; i + 1 < line.length(); i++) {
            char c = line.charAt(i);
            if (inStr) {
                if (c == q) {
                    inStr = false;
                }
            } else if (c == '\'' || c == '"') {
                inStr = true;
                q = c;
            } else if (c == '/' && line.charAt(i + 1) == '/') {
                return i;
            }
        }
        return -1;
    }

    /** Split a string at top-level commas (ignoring commas inside (), [], {}, and quotes). */
    private static List<String> splitTopLevel(String s) {
        List<String> parts = new ArrayList<>();
        int depth = 0;
        boolean inStr = false;
        char q = 0;
        int start = 0;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (inStr) {
                if (c == q) {
                    inStr = false;
                }
            } else if (c == '\'' || c == '"') {
                inStr = true;
                q = c;
            } else if (c == '(' || c == '[' || c == '{') {
                depth++;
            } else if (c == ')' || c == ']' || c == '}') {
                depth--;
            } else if (c == ',' && depth == 0) {
                parts.add(s.substring(start, i));
                start = i + 1;
            }
        }
        if (start < s.length()) {
            parts.add(s.substring(start));
        }
        return parts;
    }

    /** Index of the top-level ':' in a "key: value" pair (skipping quotes/brackets). */
    private static int topLevelColon(String s) {
        int depth = 0;
        boolean inStr = false;
        char q = 0;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (inStr) {
                if (c == q) {
                    inStr = false;
                }
            } else if (c == '\'' || c == '"') {
                inStr = true;
                q = c;
            } else if (c == '(' || c == '[' || c == '{') {
                depth++;
            } else if (c == ')' || c == ']' || c == '}') {
                depth--;
            } else if (c == ':' && depth == 0) {
                return i;
            }
        }
        return -1;
    }

    private static int matchParen(String s, int open) {
        return matchDelim(s, open, '(', ')');
    }

    private static int matchBrace(String s, int open) {
        return matchDelim(s, open, '{', '}');
    }

    private static int matchBracket(String s, int open) {
        return matchDelim(s, open, '[', ']');
    }

    private static int matchDelim(String s, int open, char o, char c) {
        int depth = 0;
        boolean inStr = false;
        char q = 0;
        for (int i = open; i < s.length(); i++) {
            char ch = s.charAt(i);
            if (inStr) {
                if (ch == q) {
                    inStr = false;
                }
            } else if (ch == '\'' || ch == '"') {
                inStr = true;
                q = ch;
            } else if (ch == o) {
                depth++;
            } else if (ch == c) {
                depth--;
                if (depth == 0) {
                    return i;
                }
            }
        }
        throw new RuntimeException("Unbalanced '" + o + "' in: " + s);
    }

    private static String unquote(String s) {
        s = s.trim();
        if (s.length() >= 2 && (s.charAt(0) == '\'' || s.charAt(0) == '"')) {
            return s.substring(1, s.length() - 1);
        }
        return s;
    }

    private static String unquoteKey(String s) {
        return unquote(s.trim());
    }

    // ------------------------------------------------------------------------
    // Encoding to Hydra JSON (shared shape with the validators)
    // ------------------------------------------------------------------------

    private static String encodeGraphToJson(Graph graph) {
        List<Term> nodeTerms = graph.nodes.stream().map(Model::node).toList();
        List<Term> relTerms = graph.relationships.stream().map(Model::relationship).toList();
        Term term = new Term.Record(new hydra.core.Record(
            new hydra.core.Name("hydra.demos.neo4jvalidation.Graph"),
            hydra.overlay.java.util.ConsList.<hydra.core.Field>of(
                new hydra.core.Field(new hydra.core.Name("nodes"), new Term.List(nodeTerms)),
                new hydra.core.Field(new hydra.core.Name("relationships"), new Term.List(relTerms)))));
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
}
