package hydra;

import hydra.annotations.Annotations;
import hydra.codeGeneration.CodeGeneration;
import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.compute.Trace;
import hydra.core.Binding;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.graph.Primitive;
import hydra.json.model.Value;
import hydra.lib.Libraries;
import hydra.module.Definition;
import hydra.module.Module;
import hydra.module.Namespace;
import hydra.rewriting.Rewriting;
import hydra.tools.FlowException;
import hydra.tools.PrimitiveFunction;
import hydra.util.Either;
import hydra.util.Maybe;
import hydra.util.Tuple;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;


/**
 * I/O wrapper for Hydra code generation in Java.
 * Provides file I/O around the pure/Flow-based functions in CodeGeneration.
 */
public class Generation {

    /**
     * Create an empty graph with standard primitives (the bootstrap graph).
     */
    public static Graph bootstrapGraph() {
        List<Binding> elements = Collections.emptyList();
        Map<Name, TypeScheme> types = Collections.emptyMap();
        Map<Name, Maybe<Term>> environment = Collections.emptyMap();
        Term body = Terms.string("empty graph");

        Map<Name, Primitive> primitives = new HashMap<>();
        for (PrimitiveFunction prim : Libraries.standardPrimitives()) {
            primitives.put(prim.name(), prim.toNative());
        }

        Maybe<Graph> schema = Maybe.nothing();

        return new Graph(elements, environment, types, body, primitives, schema);
    }

    /**
     * Evaluate a Flow computation, throwing on failure.
     */
    public static <S, A> A runFlow(S state, Flow<S, A> flow) {
        return Flows.fromFlow(state, flow);
    }

    /**
     * Parse a JSON file into a Hydra JSON value.
     * Uses a simple hand-written recursive descent parser (no external dependencies).
     */
    public static Value parseJsonFile(String path) throws IOException {
        String jsonStr = new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8);
        return new SimpleJsonParser(jsonStr).parseValue();
    }

    /**
     * A simple recursive descent JSON parser that produces hydra.json.model.Value objects.
     */
    private static class SimpleJsonParser {
        private final String input;
        private int pos;

        SimpleJsonParser(String input) {
            this.input = input;
            this.pos = 0;
        }

        Value parseValue() {
            skipWhitespace();
            if (pos >= input.length()) throw new RuntimeException("Unexpected end of JSON");
            char c = input.charAt(pos);
            if (c == '{') return parseObject();
            if (c == '[') return parseArray();
            if (c == '"') return parseString();
            if (c == 't' || c == 'f') return parseBoolean();
            if (c == 'n') return parseNull();
            if (c == '-' || (c >= '0' && c <= '9')) return parseNumber();
            throw new RuntimeException("Unexpected character '" + c + "' at position " + pos);
        }

        private Value parseObject() {
            expect('{');
            skipWhitespace();
            Map<String, Value> map = new java.util.LinkedHashMap<>();
            if (pos < input.length() && input.charAt(pos) != '}') {
                parseKeyValue(map);
                while (pos < input.length() && input.charAt(pos) == ',') {
                    pos++;
                    parseKeyValue(map);
                }
            }
            expect('}');
            return new Value.Object_(map);
        }

        private void parseKeyValue(Map<String, Value> map) {
            skipWhitespace();
            String key = parseRawString();
            skipWhitespace();
            expect(':');
            Value val = parseValue();
            map.put(key, val);
        }

        private Value parseArray() {
            expect('[');
            skipWhitespace();
            List<Value> list = new ArrayList<>();
            if (pos < input.length() && input.charAt(pos) != ']') {
                list.add(parseValue());
                while (pos < input.length() && input.charAt(pos) == ',') {
                    pos++;
                    list.add(parseValue());
                }
            }
            expect(']');
            return new Value.Array(list);
        }

        private Value parseString() {
            return new Value.String_(parseRawString());
        }

        private String parseRawString() {
            expect('"');
            StringBuilder sb = new StringBuilder();
            while (pos < input.length() && input.charAt(pos) != '"') {
                char c = input.charAt(pos);
                if (c == '\\') {
                    pos++;
                    if (pos >= input.length()) throw new RuntimeException("Unexpected end of string escape");
                    char escaped = input.charAt(pos);
                    switch (escaped) {
                        case '"': sb.append('"'); break;
                        case '\\': sb.append('\\'); break;
                        case '/': sb.append('/'); break;
                        case 'b': sb.append('\b'); break;
                        case 'f': sb.append('\f'); break;
                        case 'n': sb.append('\n'); break;
                        case 'r': sb.append('\r'); break;
                        case 't': sb.append('\t'); break;
                        case 'u':
                            pos++;
                            if (pos + 4 > input.length()) throw new RuntimeException("Invalid unicode escape");
                            int codePoint = Integer.parseInt(input.substring(pos, pos + 4), 16);
                            sb.append((char) codePoint);
                            pos += 3; // will be incremented below
                            break;
                        default: sb.append(escaped);
                    }
                } else {
                    sb.append(c);
                }
                pos++;
            }
            expect('"');
            return sb.toString();
        }

        private Value parseNumber() {
            int start = pos;
            if (pos < input.length() && input.charAt(pos) == '-') pos++;
            while (pos < input.length() && input.charAt(pos) >= '0' && input.charAt(pos) <= '9') pos++;
            if (pos < input.length() && input.charAt(pos) == '.') {
                pos++;
                while (pos < input.length() && input.charAt(pos) >= '0' && input.charAt(pos) <= '9') pos++;
            }
            if (pos < input.length() && (input.charAt(pos) == 'e' || input.charAt(pos) == 'E')) {
                pos++;
                if (pos < input.length() && (input.charAt(pos) == '+' || input.charAt(pos) == '-')) pos++;
                while (pos < input.length() && input.charAt(pos) >= '0' && input.charAt(pos) <= '9') pos++;
            }
            String numStr = input.substring(start, pos);
            return new Value.Number_(new java.math.BigDecimal(numStr));
        }

        private Value parseBoolean() {
            if (input.startsWith("true", pos)) {
                pos += 4;
                return new Value.Boolean_(true);
            }
            if (input.startsWith("false", pos)) {
                pos += 5;
                return new Value.Boolean_(false);
            }
            throw new RuntimeException("Expected boolean at position " + pos);
        }

        private Value parseNull() {
            if (input.startsWith("null", pos)) {
                pos += 4;
                return new Value.Null();
            }
            throw new RuntimeException("Expected null at position " + pos);
        }

        private void skipWhitespace() {
            while (pos < input.length()) {
                char c = input.charAt(pos);
                if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
                    pos++;
                } else {
                    break;
                }
            }
        }

        private void expect(char c) {
            skipWhitespace();
            if (pos >= input.length() || input.charAt(pos) != c) {
                throw new RuntimeException("Expected '" + c + "' at position " + pos);
            }
            pos++;
        }
    }

    /**
     * Decode a single module from a JSON value using the generated schema-based decoder.
     * Requires a non-empty universe of modules for type resolution.
     */
    public static Module decodeModuleFromJson(Graph bsGraph, List<Module> universeModules,
            boolean stripTypeSchemes, Value jsonVal) {
        Either<String, Module> result = CodeGeneration.decodeModuleFromJson(
                bsGraph, universeModules, stripTypeSchemes, jsonVal);
        return result.accept(new Either.Visitor<String, Module, Module>() {
            @Override
            public Module visit(Either.Left<String, Module> instance) {
                throw new RuntimeException("Module decode error: " + instance.value);
            }

            @Override
            public Module visit(Either.Right<String, Module> instance) {
                return instance.value;
            }
        });
    }

    /**
     * Decode a Module from a JSON Value using native Java parsing.
     * This bypasses the schema-based decoder, interpreting the JSON structure directly.
     * Used when kernel modules are not available (e.g., Java bootstrapping).
     */
    public static Module nativeDecodeModule(Value jsonVal) {
        Map<String, Value> obj = expectObject(jsonVal, "Module");

        String nsStr = expectString(obj.get("namespace"), "Module.namespace");
        Namespace namespace = new Namespace(nsStr);

        List<Value> elemsList = expectArray(obj.get("elements"), "Module.elements");
        List<Binding> elements = new ArrayList<>();
        for (Value elemVal : elemsList) {
            elements.add(nativeDecodeBinding(elemVal));
        }

        List<Namespace> typeDeps = new ArrayList<>();
        List<Value> typeDepsVal = expectArray(obj.get("typeDependencies"), "Module.typeDependencies");
        for (Value v : typeDepsVal) {
            typeDeps.add(new Namespace(expectString(v, "typeDependency")));
        }

        List<Namespace> termDeps = new ArrayList<>();
        List<Value> termDepsVal = expectArray(obj.get("termDependencies"), "Module.termDependencies");
        for (Value v : termDepsVal) {
            termDeps.add(new Namespace(expectString(v, "termDependency")));
        }

        Maybe<String> description = Maybe.nothing();
        if (obj.containsKey("description")) {
            List<Value> descArr = expectArray(obj.get("description"), "Module.description");
            if (!descArr.isEmpty()) {
                description = Maybe.just(expectString(descArr.get(0), "Module.description.value"));
            }
        }

        return new Module(namespace, elements, typeDeps, termDeps, description);
    }

    /**
     * Decode a Binding from JSON: {name: string, term: Term, type: Maybe TypeScheme}
     */
    private static Binding nativeDecodeBinding(Value val) {
        Map<String, Value> obj = expectObject(val, "Binding");
        String name = expectString(obj.get("name"), "Binding.name");
        Term term = nativeDecodeTerm(obj.get("term"));
        Maybe<TypeScheme> type = nativeDecodeMaybeTypeScheme(obj.get("type"));
        return new Binding(new Name(name), term, type);
    }

    /**
     * Decode a Maybe TypeScheme from JSON.
     * In JSON, Maybe is encoded as an array: [] for Nothing, [value] for Just.
     */
    private static Maybe<TypeScheme> nativeDecodeMaybeTypeScheme(Value val) {
        if (val == null) return Maybe.nothing();
        if (!(val instanceof Value.Array)) return Maybe.nothing();
        List<Value> arr = ((Value.Array) val).value;
        if (arr.isEmpty()) return Maybe.nothing();
        return Maybe.just(nativeDecodeTypeScheme(arr.get(0)));
    }

    /**
     * Decode a TypeScheme from JSON: {variables: [Name], type: Type, constraints: Maybe ...}
     */
    private static TypeScheme nativeDecodeTypeScheme(Value val) {
        Map<String, Value> obj = expectObject(val, "TypeScheme");
        List<Name> variables = new ArrayList<>();
        List<Value> varsArr = expectArray(obj.get("variables"), "TypeScheme.variables");
        for (Value v : varsArr) {
            variables.add(new Name(expectString(v, "TypeScheme.variable")));
        }
        hydra.core.Type type = nativeDecodeType(obj.get("type"));
        Maybe<Map<Name, hydra.core.TypeVariableMetadata>> constraints = Maybe.nothing();
        return new TypeScheme(variables, type, constraints);
    }

    /**
     * Decode a Type from JSON (minimal — only handles the patterns needed for isNativeType).
     */
    private static hydra.core.Type nativeDecodeType(Value val) {
        Map<String, Value> obj = expectObject(val, "Type");
        if (obj.containsKey("variable")) {
            return new hydra.core.Type.Variable(new Name(expectString(obj.get("variable"), "Type.variable")));
        }
        // For other type shapes, return a placeholder — only TypeVariable matters for isNativeType
        return new hydra.core.Type.Variable(new Name("unknown"));
    }

    /**
     * Recursively decode a Term from JSON.
     * Hydra terms in JSON use specific keys that map to term constructors.
     * This decoder recognizes the Hydra term encoding and produces properly
     * structured Term objects that the code generator can process.
     */
    private static Term nativeDecodeTerm(Value val) {
        if (val == null) {
            return Terms.string("null");
        }
        return val.accept(new Value.Visitor<Term>() {
            @Override
            public Term visit(Value.Array instance) {
                List<Term> terms = new ArrayList<>();
                for (Value v : instance.value) {
                    terms.add(nativeDecodeTerm(v));
                }
                return new Term.List(terms);
            }

            @Override
            public Term visit(Value.Boolean_ instance) {
                return new Term.Literal(new hydra.core.Literal.Boolean_(instance.value));
            }

            @Override
            public Term visit(Value.Null instance) {
                return Terms.string("null");
            }

            @Override
            public Term visit(Value.Number_ instance) {
                double d = instance.value.doubleValue();
                if (d == Math.floor(d) && !Double.isInfinite(d)) {
                    return new Term.Literal(new hydra.core.Literal.Integer_(
                            new hydra.core.IntegerValue.Int32((int) d)));
                }
                return new Term.Literal(new hydra.core.Literal.Float_(
                        new hydra.core.FloatValue.Float64(d)));
            }

            @Override
            public Term visit(Value.Object_ instance) {
                Map<String, Value> obj = instance.value;
                if (obj.size() == 1) {
                    Map.Entry<String, Value> entry = obj.entrySet().iterator().next();
                    String tag = entry.getKey();
                    Value payload = entry.getValue();
                    return decodeTaggedTerm(tag, payload);
                }

                // Multiple keys = record encoding; infer typeName from field patterns
                String inferredTypeName = inferRecordTypeName(obj);
                List<hydra.core.Field> fields = new ArrayList<>();
                for (Map.Entry<String, Value> entry : obj.entrySet()) {
                    fields.add(new hydra.core.Field(
                            new Name(entry.getKey()), nativeDecodeTerm(entry.getValue())));
                }
                return new Term.Record(new hydra.core.Record(new Name(inferredTypeName), fields));
            }

            @Override
            public Term visit(Value.String_ instance) {
                return Terms.string(instance.value);
            }
        });
    }

    /**
     * Decode a tagged term from JSON. Recognizes Hydra term constructors
     * and produces properly structured Term objects.
     */
    private static Term decodeTaggedTerm(String tag, Value payload) {
        switch (tag) {
            case "annotated": {
                Map<String, Value> obj = expectObject(payload, "annotated");
                Term body = nativeDecodeTerm(obj.get("body"));
                // Annotations are a map; decode as key-value pairs
                Map<Name, Term> annotations = new HashMap<>();
                if (obj.containsKey("annotation")) {
                    List<Value> annList = expectArray(obj.get("annotation"), "annotation");
                    for (Value annVal : annList) {
                        Map<String, Value> annObj = expectObject(annVal, "annotation entry");
                        String key = expectString(annObj.get("@key"), "annotation key");
                        Term value = nativeDecodeTerm(annObj.get("@value"));
                        annotations.put(new Name(key), value);
                    }
                }
                return new Term.Annotated(new hydra.core.AnnotatedTerm(body, annotations));
            }
            case "application": {
                Map<String, Value> obj = expectObject(payload, "application");
                Term function = nativeDecodeTerm(obj.get("function"));
                Term argument = nativeDecodeTerm(obj.get("argument"));
                return new Term.Application(new hydra.core.Application(function, argument));
            }
            case "function": {
                // Function wrapper: {"function": {"lambda": {...}}} or {"function": {"primitive": "name"}}
                // or {"function": {"elimination": {...}}}
                Map<String, Value> obj = expectObject(payload, "function");
                if (obj.size() == 1) {
                    Map.Entry<String, Value> entry = obj.entrySet().iterator().next();
                    String funcTag = entry.getKey();
                    Value funcPayload = entry.getValue();
                    switch (funcTag) {
                        case "lambda":
                            return decodeTaggedTerm("lambda", funcPayload);
                        case "elimination":
                            return decodeTaggedTerm("elimination", funcPayload);
                        case "primitive":
                            return new Term.Function(new hydra.core.Function.Primitive(
                                    new Name(expectString(funcPayload, "function.primitive"))));
                        default:
                            break;
                    }
                }
                // Fallback: might be a FunctionType in type position {domain, codomain}
                if (obj.containsKey("domain") && obj.containsKey("codomain")) {
                    // Treat as record for FunctionType
                    List<hydra.core.Field> fields = new ArrayList<>();
                    for (Map.Entry<String, Value> e : obj.entrySet()) {
                        fields.add(new hydra.core.Field(new Name(e.getKey()), nativeDecodeTerm(e.getValue())));
                    }
                    return new Term.Record(new hydra.core.Record(new Name("hydra.core.FunctionType"), fields));
                }
                return nativeDecodeTerm(payload);
            }
            case "lambda": {
                Map<String, Value> obj = expectObject(payload, "lambda");
                String param = expectString(obj.get("parameter"), "lambda.parameter");
                Term body = nativeDecodeTerm(obj.get("body"));
                Maybe<hydra.core.Type> domain = Maybe.nothing();
                if (obj.containsKey("domain") && !(obj.get("domain") instanceof Value.Null)) {
                    domain = decodeMaybeType(obj.get("domain"));
                }
                return new Term.Function(new hydra.core.Function.Lambda(
                        new hydra.core.Lambda(new Name(param), domain, body)));
            }
            case "primitive": {
                String name = expectString(payload, "primitive");
                return new Term.Function(new hydra.core.Function.Primitive(new Name(name)));
            }
            case "variable": {
                String name = expectString(payload, "variable");
                return new Term.Variable(new Name(name));
            }
            case "literal": {
                return decodeLiteral(payload);
            }
            case "list": {
                List<Value> items = expectArray(payload, "list");
                List<Term> terms = new ArrayList<>();
                for (Value v : items) {
                    terms.add(nativeDecodeTerm(v));
                }
                return new Term.List(terms);
            }
            case "wrap": {
                Map<String, Value> obj = expectObject(payload, "wrap");
                String typeName = expectString(obj.get("typeName"), "wrap.typeName");
                Term body = nativeDecodeTerm(obj.get("body"));
                return new Term.Wrap(new hydra.core.WrappedTerm(new Name(typeName), body));
            }
            case "record": {
                Map<String, Value> obj = expectObject(payload, "record");
                String typeName = obj.containsKey("typeName") ?
                        expectString(obj.get("typeName"), "record.typeName") : "";
                List<hydra.core.Field> fields = new ArrayList<>();
                if (obj.containsKey("fields")) {
                    List<Value> fieldVals = expectArray(obj.get("fields"), "record.fields");
                    for (Value fv : fieldVals) {
                        Map<String, Value> fobj = expectObject(fv, "field");
                        String fname = expectString(fobj.get("name"), "field.name");
                        Term fterm = nativeDecodeTerm(fobj.get("term"));
                        fields.add(new hydra.core.Field(new Name(fname), fterm));
                    }
                }
                // Unit type: Record with typeName "hydra.core.Unit" and empty fields
                if ("hydra.core.Unit".equals(typeName) && fields.isEmpty()) {
                    return new Term.Unit();
                }
                return new Term.Record(new hydra.core.Record(new Name(typeName), fields));
            }
            case "union": {
                Map<String, Value> obj = expectObject(payload, "union");
                String typeName = obj.containsKey("typeName") ?
                        expectString(obj.get("typeName"), "union.typeName") : "";
                Map<String, Value> fieldObj = expectObject(obj.get("field"), "union.field");
                String fieldName = expectString(fieldObj.get("name"), "field.name");
                Term fieldTerm = nativeDecodeTerm(fieldObj.get("term"));
                return new Term.Union(new hydra.core.Injection(
                        new Name(typeName), new hydra.core.Field(new Name(fieldName), fieldTerm)));
            }
            case "let": {
                Map<String, Value> obj = expectObject(payload, "let");
                List<Value> bindingVals = expectArray(obj.get("bindings"), "let.bindings");
                List<Binding> bindings = new ArrayList<>();
                for (Value bv : bindingVals) {
                    Map<String, Value> bobj = expectObject(bv, "let binding");
                    String bname = expectString(bobj.get("name"), "binding.name");
                    Term bterm = nativeDecodeTerm(bobj.get("term"));
                    Maybe<TypeScheme> btype = bobj.containsKey("type") ?
                            nativeDecodeMaybeTypeScheme(bobj.get("type")) : Maybe.nothing();
                    bindings.add(new Binding(new Name(bname), bterm, btype));
                }
                Term body = nativeDecodeTerm(obj.get("body"));
                return new Term.Let(new hydra.core.Let(bindings, body));
            }
            case "map": {
                // Maps are encoded as arrays of {@key: k, @value: v} pairs
                Map<Term, Term> termMap = new HashMap<>();
                if (payload instanceof Value.Array) {
                    for (Value entry : ((Value.Array) payload).value) {
                        Map<String, Value> eobj = expectObject(entry, "map entry");
                        Term k = nativeDecodeTerm(eobj.get("@key"));
                        Term v = nativeDecodeTerm(eobj.get("@value"));
                        termMap.put(k, v);
                    }
                }
                return new Term.Map(termMap);
            }
            case "set": {
                List<Value> items = expectArray(payload, "set");
                java.util.Set<Term> termSet = new java.util.HashSet<>();
                for (Value v : items) {
                    termSet.add(nativeDecodeTerm(v));
                }
                return new Term.Set(termSet);
            }
            case "optional":
            case "maybe": {
                // Maybe encoded as array: [] for Nothing, [val] for Just
                // Also handles null payload as Nothing
                if (payload instanceof Value.Null) return new Term.Maybe(Maybe.nothing());
                if (payload instanceof Value.Array) {
                    List<Value> arr = ((Value.Array) payload).value;
                    if (arr.isEmpty()) return new Term.Maybe(Maybe.nothing());
                    return new Term.Maybe(Maybe.just(nativeDecodeTerm(arr.get(0))));
                }
                // In type contexts, "maybe" wraps a type (dict payload).
                // These will be stripped by removeTypesFromTerm; just recurse.
                return new Term.Maybe(Maybe.just(nativeDecodeTerm(payload)));
            }
            case "elimination": {
                return decodeElimination(payload);
            }
            case "typeApplication": {
                Map<String, Value> obj = expectObject(payload, "typeApplication");
                Term body = nativeDecodeTerm(obj.get("body"));
                // Type argument stripped by removeTypesFromTerm anyway
                return new Term.TypeApplication(new hydra.core.TypeApplicationTerm(
                        body, new hydra.core.Type.Variable(new Name("_"))));
            }
            case "typeLambda": {
                Map<String, Value> obj = expectObject(payload, "typeLambda");
                String param = expectString(obj.get("parameter"), "typeLambda.parameter");
                Term body = nativeDecodeTerm(obj.get("body"));
                return new Term.TypeLambda(new hydra.core.TypeLambda(new Name(param), body));
            }
            case "product": {
                // Product is encoded as a pair (2-tuple) in the Java API
                List<Value> items = expectArray(payload, "product");
                if (items.size() == 2) {
                    return new Term.Pair(new Tuple.Tuple2<>(
                            nativeDecodeTerm(items.get(0)),
                            nativeDecodeTerm(items.get(1))));
                }
                // Fallback: treat as a list
                List<Term> terms = new ArrayList<>();
                for (Value v : items) {
                    terms.add(nativeDecodeTerm(v));
                }
                return new Term.List(terms);
            }
            case "pair": {
                // Pair encoded as object with @first/@second keys (term context)
                // or first/second keys (type context)
                Map<String, Value> obj = expectObject(payload, "pair");
                Term first, second;
                if (obj.containsKey("@first")) {
                    first = nativeDecodeTerm(obj.get("@first"));
                    second = nativeDecodeTerm(obj.get("@second"));
                } else {
                    first = nativeDecodeTerm(obj.get("first"));
                    second = nativeDecodeTerm(obj.get("second"));
                }
                return new Term.Pair(new Tuple.Tuple2<>(first, second));
            }
            case "either": {
                // Either encoded as object with @left/@right keys (term context)
                // or left/right keys (type context)
                Map<String, Value> obj = expectObject(payload, "either");
                if (obj.containsKey("@left")) {
                    return new Term.Either(Either.left(nativeDecodeTerm(obj.get("@left"))));
                } else if (obj.containsKey("@right")) {
                    return new Term.Either(Either.right(nativeDecodeTerm(obj.get("@right"))));
                } else if (obj.containsKey("left") && obj.containsKey("right")) {
                    // Type context — two-key object with left/right
                    List<hydra.core.Field> fields = new ArrayList<>();
                    for (Map.Entry<String, Value> e : obj.entrySet()) {
                        fields.add(new hydra.core.Field(new Name(e.getKey()), nativeDecodeTerm(e.getValue())));
                    }
                    return new Term.Record(new hydra.core.Record(new Name("hydra.core.EitherType"), fields));
                }
                // Fallback
                return nativeDecodeTerm(payload);
            }
            case "classes": {
                // TypeVariableMetadata classes list — appears in TypeScheme constraints.
                // Will be stripped; decode as a list.
                List<Value> items = expectArray(payload, "classes");
                List<Term> terms = new ArrayList<>();
                for (Value v : items) {
                    terms.add(nativeDecodeTerm(v));
                }
                return new Term.List(terms);
            }
            case "unit": {
                return new Term.Unit();
            }
            default: {
                // Unknown tag — treat as union with empty type name
                Term payloadTerm = nativeDecodeTerm(payload);
                return new Term.Union(new hydra.core.Injection(
                        new Name(""), new hydra.core.Field(new Name(tag), payloadTerm)));
            }
        }
    }

    /**
     * Decode a literal term from JSON.
     */
    private static Term decodeLiteral(Value payload) {
        if (payload instanceof Value.Object_) {
            Map<String, Value> obj = ((Value.Object_) payload).value;
            if (obj.size() == 1) {
                Map.Entry<String, Value> entry = obj.entrySet().iterator().next();
                String litTag = entry.getKey();
                Value litVal = entry.getValue();
                switch (litTag) {
                    case "string":
                        return new Term.Literal(new hydra.core.Literal.String_(
                                expectString(litVal, "literal.string")));
                    case "boolean":
                        return new Term.Literal(new hydra.core.Literal.Boolean_(
                                ((Value.Boolean_) litVal).value));
                    case "integer":
                        return decodeLiteralInteger(litVal);
                    case "float":
                        return decodeLiteralFloat(litVal);
                    default:
                        break;
                }
            }
        }
        // Fallback for simple values
        if (payload instanceof Value.String_) {
            return new Term.Literal(new hydra.core.Literal.String_(((Value.String_) payload).value));
        }
        return Terms.string("unknown-literal");
    }

    /**
     * Extract a numeric value from a JSON value that may be a Number_ or a String_ containing a number.
     */
    private static double extractNumber(Value val) {
        if (val instanceof Value.Number_) {
            return ((Value.Number_) val).value.doubleValue();
        }
        if (val instanceof Value.String_) {
            return Double.parseDouble(((Value.String_) val).value);
        }
        return 0.0;
    }

    /**
     * Decode a literal integer from JSON.
     */
    private static Term decodeLiteralInteger(Value val) {
        if (val instanceof Value.Object_) {
            Map<String, Value> obj = ((Value.Object_) val).value;
            if (obj.size() == 1) {
                Map.Entry<String, Value> entry = obj.entrySet().iterator().next();
                String intTag = entry.getKey();
                double d = extractNumber(entry.getValue());
                switch (intTag) {
                    case "int32": return new Term.Literal(new hydra.core.Literal.Integer_(
                            new hydra.core.IntegerValue.Int32((int) d)));
                    case "int64": return new Term.Literal(new hydra.core.Literal.Integer_(
                            new hydra.core.IntegerValue.Int64((long) d)));
                    case "int16": return new Term.Literal(new hydra.core.Literal.Integer_(
                            new hydra.core.IntegerValue.Int16((short) d)));
                    case "int8": return new Term.Literal(new hydra.core.Literal.Integer_(
                            new hydra.core.IntegerValue.Int8((byte) d)));
                    case "bigint": return new Term.Literal(new hydra.core.Literal.Integer_(
                            new hydra.core.IntegerValue.Bigint(java.math.BigInteger.valueOf((long) d))));
                    case "uint16": return new Term.Literal(new hydra.core.Literal.Integer_(
                            new hydra.core.IntegerValue.Uint16((char) (int) d)));
                    case "uint32": return new Term.Literal(new hydra.core.Literal.Integer_(
                            new hydra.core.IntegerValue.Uint32((long) d)));
                    case "uint64": return new Term.Literal(new hydra.core.Literal.Integer_(
                            new hydra.core.IntegerValue.Uint64(java.math.BigInteger.valueOf((long) d))));
                    case "uint8": return new Term.Literal(new hydra.core.Literal.Integer_(
                            new hydra.core.IntegerValue.Uint8((short) d)));
                    default: break;
                }
            }
        }
        return new Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)));
    }

    /**
     * Decode a literal float from JSON.
     */
    private static Term decodeLiteralFloat(Value val) {
        if (val instanceof Value.Object_) {
            Map<String, Value> obj = ((Value.Object_) val).value;
            if (obj.size() == 1) {
                Map.Entry<String, Value> entry = obj.entrySet().iterator().next();
                String floatTag = entry.getKey();
                double d = extractNumber(entry.getValue());
                switch (floatTag) {
                    case "float64": return new Term.Literal(new hydra.core.Literal.Float_(
                            new hydra.core.FloatValue.Float64(d)));
                    case "float32": return new Term.Literal(new hydra.core.Literal.Float_(
                            new hydra.core.FloatValue.Float32((float) d)));
                    case "bigfloat": return new Term.Literal(new hydra.core.Literal.Float_(
                            new hydra.core.FloatValue.Bigfloat(java.math.BigDecimal.valueOf(d))));
                    default: break;
                }
            }
        }
        return new Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)));
    }

    /**
     * Decode an elimination term from JSON.
     */
    private static Term decodeElimination(Value payload) {
        if (payload instanceof Value.Object_) {
            Map<String, Value> obj = ((Value.Object_) payload).value;
            if (obj.size() == 1) {
                Map.Entry<String, Value> entry = obj.entrySet().iterator().next();
                String elimTag = entry.getKey();
                Value elimVal = entry.getValue();
                switch (elimTag) {
                    case "wrap": {
                        String typeName = expectString(elimVal, "elimination.wrap");
                        return new Term.Function(new hydra.core.Function.Elimination(
                                new hydra.core.Elimination.Wrap(new Name(typeName))));
                    }
                    case "record": {
                        Map<String, Value> robj = expectObject(elimVal, "elimination.record");
                        String typeName = expectString(robj.get("typeName"), "projection.typeName");
                        String fieldName = expectString(robj.get("field"), "projection.field");
                        return new Term.Function(new hydra.core.Function.Elimination(
                                new hydra.core.Elimination.Record(new hydra.core.Projection(
                                        new Name(typeName), new Name(fieldName)))));
                    }
                    case "union": {
                        Map<String, Value> uobj = expectObject(elimVal, "elimination.union");
                        String typeName = expectString(uobj.get("typeName"), "caseStatement.typeName");
                        Maybe<Term> defaultTerm = Maybe.nothing();
                        if (uobj.containsKey("default") && uobj.get("default") instanceof Value.Array) {
                            List<Value> defArr = ((Value.Array) uobj.get("default")).value;
                            if (!defArr.isEmpty()) {
                                defaultTerm = Maybe.just(nativeDecodeTerm(defArr.get(0)));
                            }
                        }
                        List<hydra.core.Field> cases = new ArrayList<>();
                        List<Value> casesArr = expectArray(uobj.get("cases"), "caseStatement.cases");
                        for (Value cv : casesArr) {
                            Map<String, Value> cobj = expectObject(cv, "case field");
                            String cname = expectString(cobj.get("name"), "case.name");
                            Term cterm = nativeDecodeTerm(cobj.get("term"));
                            cases.add(new hydra.core.Field(new Name(cname), cterm));
                        }
                        return new Term.Function(new hydra.core.Function.Elimination(
                                new hydra.core.Elimination.Union(new hydra.core.CaseStatement(
                                        new Name(typeName), defaultTerm, cases))));
                    }
                    // Note: no Elimination.Optional in the Java API
                    default: break;
                }
            }
        }
        return Terms.string("unknown-elimination");
    }

    /**
     * Decode a Maybe Type from JSON (array encoding: [] for Nothing, [type] for Just).
     */
    private static Maybe<hydra.core.Type> decodeMaybeType(Value val) {
        if (val instanceof Value.Array) {
            List<Value> arr = ((Value.Array) val).value;
            if (arr.isEmpty()) return Maybe.nothing();
            return Maybe.just(nativeDecodeFullType(arr.get(0)));
        }
        if (val instanceof Value.Null) return Maybe.nothing();
        return Maybe.just(nativeDecodeFullType(val));
    }

    /**
     * Decode a Type from JSON (full version for term type annotations).
     */
    private static hydra.core.Type nativeDecodeFullType(Value val) {
        if (!(val instanceof Value.Object_)) {
            return new hydra.core.Type.Variable(new Name("unknown"));
        }
        Map<String, Value> obj = ((Value.Object_) val).value;
        if (obj.size() != 1) {
            return new hydra.core.Type.Variable(new Name("unknown"));
        }
        Map.Entry<String, Value> entry = obj.entrySet().iterator().next();
        String tag = entry.getKey();
        Value payload = entry.getValue();
        switch (tag) {
            case "variable":
                return new hydra.core.Type.Variable(new Name(expectString(payload, "Type.variable")));
            case "application": {
                Map<String, Value> aobj = expectObject(payload, "TypeApplication");
                return new hydra.core.Type.Application(new hydra.core.ApplicationType(
                        nativeDecodeFullType(aobj.get("function")),
                        nativeDecodeFullType(aobj.get("argument"))));
            }
            case "lambda": {
                Map<String, Value> lobj = expectObject(payload, "TypeLambda");
                String param = expectString(lobj.get("parameter"), "TypeLambda.parameter");
                return new hydra.core.Type.Forall(new hydra.core.ForallType(
                        new Name(param), nativeDecodeFullType(lobj.get("body"))));
            }
            case "list":
                return new hydra.core.Type.List(nativeDecodeFullType(payload));
            case "optional":
            case "maybe":
                return new hydra.core.Type.Maybe(nativeDecodeFullType(payload));
            case "set":
                return new hydra.core.Type.Set(nativeDecodeFullType(payload));
            case "map": {
                Map<String, Value> mobj = expectObject(payload, "TypeMap");
                return new hydra.core.Type.Map(new hydra.core.MapType(
                        nativeDecodeFullType(mobj.get("keys")),
                        nativeDecodeFullType(mobj.get("values"))));
            }
            case "record": {
                Map<String, Value> robj = expectObject(payload, "TypeRecord");
                String typeName = expectString(robj.get("typeName"), "RowType.typeName");
                List<hydra.core.FieldType> fields = new ArrayList<>();
                if (robj.containsKey("fields")) {
                    for (Value fv : expectArray(robj.get("fields"), "RowType.fields")) {
                        Map<String, Value> fobj = expectObject(fv, "FieldType");
                        String fname = expectString(fobj.get("name"), "FieldType.name");
                        hydra.core.Type ftype = nativeDecodeFullType(fobj.get("type"));
                        fields.add(new hydra.core.FieldType(new Name(fname), ftype));
                    }
                }
                return new hydra.core.Type.Record(new hydra.core.RowType(new Name(typeName), fields));
            }
            case "union": {
                Map<String, Value> uobj = expectObject(payload, "TypeUnion");
                String typeName = expectString(uobj.get("typeName"), "RowType.typeName");
                List<hydra.core.FieldType> fields = new ArrayList<>();
                if (uobj.containsKey("fields")) {
                    for (Value fv : expectArray(uobj.get("fields"), "RowType.fields")) {
                        Map<String, Value> fobj = expectObject(fv, "FieldType");
                        String fname = expectString(fobj.get("name"), "FieldType.name");
                        hydra.core.Type ftype = nativeDecodeFullType(fobj.get("type"));
                        fields.add(new hydra.core.FieldType(new Name(fname), ftype));
                    }
                }
                return new hydra.core.Type.Union(new hydra.core.RowType(new Name(typeName), fields));
            }
            case "wrap": {
                Map<String, Value> wobj = expectObject(payload, "TypeWrap");
                String typeName = expectString(wobj.get("typeName"), "WrappedType.typeName");
                hydra.core.Type inner = nativeDecodeFullType(wobj.get("object"));
                return new hydra.core.Type.Wrap(new hydra.core.WrappedType(new Name(typeName), inner));
            }
            case "function": {
                Map<String, Value> fobj = expectObject(payload, "FunctionType");
                return new hydra.core.Type.Function(new hydra.core.FunctionType(
                        nativeDecodeFullType(fobj.get("domain")),
                        nativeDecodeFullType(fobj.get("codomain"))));
            }
            case "literal":
                return decodeLiteralType(payload);
            case "product":
            case "pair": {
                // Pair/product type with first/second fields
                Map<String, Value> pobj = expectObject(payload, "PairType");
                return new hydra.core.Type.Pair(new hydra.core.PairType(
                        nativeDecodeFullType(pobj.get("first")),
                        nativeDecodeFullType(pobj.get("second"))));
            }
            case "either": {
                Map<String, Value> eobj = expectObject(payload, "EitherType");
                return new hydra.core.Type.Either(new hydra.core.EitherType(
                        nativeDecodeFullType(eobj.get("left")),
                        nativeDecodeFullType(eobj.get("right"))));
            }
            case "annotated": {
                Map<String, Value> aobj = expectObject(payload, "AnnotatedType");
                return new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(
                        nativeDecodeFullType(aobj.get("body")), new HashMap<>()));
            }
            default:
                return new hydra.core.Type.Variable(new Name("unknown." + tag));
        }
    }

    /**
     * Decode a literal type from JSON.
     */
    private static hydra.core.Type decodeLiteralType(Value val) {
        if (val instanceof Value.Object_) {
            Map<String, Value> obj = ((Value.Object_) val).value;
            if (obj.size() == 1) {
                Map.Entry<String, Value> entry = obj.entrySet().iterator().next();
                String tag = entry.getKey();
                Value payload = entry.getValue();
                switch (tag) {
                    case "boolean":
                        return new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_());
                    case "string":
                        return new hydra.core.Type.Literal(new hydra.core.LiteralType.String_());
                    case "integer":
                        return decodeLiteralIntegerType(payload);
                    case "float":
                        return decodeLiteralFloatType(payload);
                    case "binary":
                        return new hydra.core.Type.Literal(new hydra.core.LiteralType.Binary());
                    default:
                        break;
                }
            }
        }
        return new hydra.core.Type.Literal(new hydra.core.LiteralType.String_());
    }

    private static hydra.core.Type decodeLiteralIntegerType(Value val) {
        String tag = extractUnionTag(val, "IntegerType");
        switch (tag) {
            case "int32": return new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()));
            case "int64": return new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int64()));
            case "int16": return new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int16()));
            case "int8": return new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int8()));
            case "bigint": return new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Bigint()));
            case "uint16": return new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint16()));
            case "uint32": return new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint32()));
            case "uint64": return new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint64()));
            case "uint8": return new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint8()));
            default: return new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()));
        }
    }

    private static hydra.core.Type decodeLiteralFloatType(Value val) {
        String tag = extractUnionTag(val, "FloatType");
        switch (tag) {
            case "float64": return new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float64()));
            case "float32": return new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32()));
            case "bigfloat": return new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Bigfloat()));
            default: return new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float64()));
        }
    }

    /**
     * Extract a union variant tag from a value that may be either a string ("tag")
     * or an object encoding ({"tag": payload}).
     */
    private static String extractUnionTag(Value val, String context) {
        if (val instanceof Value.String_) {
            return ((Value.String_) val).value;
        }
        if (val instanceof Value.Object_) {
            Map<String, Value> obj = ((Value.Object_) val).value;
            if (obj.size() == 1) {
                return obj.keySet().iterator().next();
            }
        }
        throw new RuntimeException("Expected JSON string or single-key object for " + context + ", got " + val.getClass().getSimpleName());
    }

    /**
     * Infer the Hydra record type name from a multi-key JSON object's field names.
     */
    private static String inferRecordTypeName(Map<String, Value> obj) {
        java.util.Set<String> keys = obj.keySet();
        if (keys.contains("function") && keys.contains("argument")) return "hydra.core.Application";
        if (keys.contains("name") && keys.contains("term") && keys.contains("type")) return "hydra.core.Binding";
        if (keys.contains("name") && keys.contains("term")) return "hydra.core.Field";
        if (keys.contains("name") && keys.contains("type")) return "hydra.core.FieldType";
        if (keys.contains("fields") && keys.contains("typeName")) return "hydra.core.RowType";
        if (keys.contains("field") && keys.contains("typeName")) return "hydra.core.Injection";
        if (keys.contains("body") && keys.contains("typeName")) return "hydra.core.WrappedTerm";
        if (keys.contains("body") && keys.contains("domain") && keys.contains("parameter")) return "hydra.core.Lambda";
        if (keys.contains("body") && keys.contains("annotation")) return "hydra.core.AnnotatedTerm";
        if (keys.contains("body") && keys.contains("parameter")) return "hydra.core.TypeLambda";
        if (keys.contains("body") && keys.contains("type")) return "hydra.core.TypeApplicationTerm";
        if (keys.contains("codomain") && keys.contains("domain")) return "hydra.core.FunctionType";
        if (keys.contains("bindings") && keys.contains("body")) return "hydra.core.Let";
        if (keys.contains("keys") && keys.contains("values")) return "hydra.core.MapType";
        if (keys.contains("first") && keys.contains("second")) return "hydra.core.PairType";
        if (keys.contains("left") && keys.contains("right")) return "hydra.core.EitherType";
        if (keys.contains("cases") && keys.contains("typeName")) return "hydra.core.CaseStatement";
        if (keys.contains("constraints") && keys.contains("type") && keys.contains("variables")) return "hydra.core.TypeScheme";
        if (keys.contains("namespace") && keys.contains("elements")) return "hydra.module.Module";
        if (keys.contains("@key") && keys.contains("@value")) return "";  // map entry
        if (keys.contains("@first") && keys.contains("@second")) return "";  // pair entry
        return "";
    }

    // Helper methods for native JSON decoding

    private static Map<String, Value> expectObject(Value val, String context) {
        if (val instanceof Value.Object_) {
            return ((Value.Object_) val).value;
        }
        throw new RuntimeException("Expected JSON object for " + context + ", got " + val.getClass().getSimpleName());
    }

    private static List<Value> expectArray(Value val, String context) {
        if (val instanceof Value.Array) {
            return ((Value.Array) val).value;
        }
        throw new RuntimeException("Expected JSON array for " + context + ", got " + val.getClass().getSimpleName());
    }

    private static String expectString(Value val, String context) {
        if (val instanceof Value.String_) {
            return ((Value.String_) val).value;
        }
        throw new RuntimeException("Expected JSON string for " + context + ", got " + val.getClass().getSimpleName());
    }

    /**
     * Load modules from JSON files.
     * If universeModules is empty, uses native JSON decoding (no schema required).
     * Otherwise, uses the generated schema-based decoder.
     */
    public static List<Module> loadModulesFromJson(boolean stripTypeSchemes, String basePath,
            List<Module> universeModules, List<Namespace> namespaces) throws IOException {
        boolean useNative = universeModules.isEmpty();
        Graph bsGraph = useNative ? null : bootstrapGraph();
        List<Module> modules = new ArrayList<>();
        for (Namespace ns : namespaces) {
            String filePath = basePath + File.separator
                    + CodeGeneration.namespaceToPath(ns) + ".json";
            Value jsonVal = parseJsonFile(filePath);
            Module mod;
            if (useNative) {
                mod = nativeDecodeModule(jsonVal);
            } else {
                mod = decodeModuleFromJson(bsGraph, universeModules, stripTypeSchemes, jsonVal);
            }
            System.out.println("  Loaded: " + ns.value);
            modules.add(mod);
        }
        return modules;
    }

    /**
     * Discover namespaces from JSON files in a directory tree.
     */
    public static List<Namespace> discoverJsonNamespaces(String basePath) throws IOException {
        Path base = Paths.get(basePath);
        if (!Files.isDirectory(base)) {
            return Collections.emptyList();
        }

        List<Namespace> namespaces;
        try (Stream<Path> walk = Files.walk(base)) {
            namespaces = walk
                    .filter(p -> p.toString().endsWith(".json"))
                    .map(p -> {
                        String rel = base.relativize(p).toString();
                        // Remove .json extension and convert path separators to dots
                        String withoutExt = rel.substring(0, rel.length() - 5);
                        String ns = withoutExt.replace(File.separatorChar, '.').replace('/', '.');
                        return new Namespace(ns);
                    })
                    .sorted()
                    .collect(Collectors.toList());
        }
        return namespaces;
    }

    /**
     * Load all modules from a JSON directory.
     * TypeSchemes are stripped by default (suitable for main/kernel modules).
     */
    public static List<Module> loadAllModulesFromJsonDir(String basePath,
            List<Module> universeModules) throws IOException {
        return loadAllModulesFromJsonDirWith(true, basePath, universeModules);
    }

    /**
     * Load all modules from a JSON directory with control over TypeScheme stripping.
     */
    public static List<Module> loadAllModulesFromJsonDirWith(boolean stripTypeSchemes,
            String basePath, List<Module> universeModules) throws IOException {
        List<Namespace> namespaces = discoverJsonNamespaces(basePath);
        System.out.println("  Discovered " + namespaces.size() + " modules in " + basePath);
        return loadModulesFromJson(stripTypeSchemes, basePath, universeModules, namespaces);
    }

    /**
     * Generate source files and write them to disk.
     */
    public static void generateSources(
            Function<Module, Function<List<Definition>, Flow<Graph, Map<String, String>>>> coder,
            hydra.coders.Language language,
            boolean doExpand,
            boolean doHoistCase,
            boolean doHoistPoly,
            String basePath,
            List<Module> universe,
            List<Module> modulesToGenerate) {
        Graph bsGraph = bootstrapGraph();
        Flow<Graph, List<Tuple.Tuple2<String, String>>> flow =
                CodeGeneration.generateSourceFiles(coder, language,
                        doExpand, doHoistCase, doHoistPoly,
                        bsGraph, universe, modulesToGenerate);
        List<Tuple.Tuple2<String, String>> files;
        try {
            files = runFlow(bsGraph, flow);
        } catch (Exception e) {
            // Print trace for debugging
            FlowState<Graph, List<Tuple.Tuple2<String, String>>> state =
                    flow.value.apply(bsGraph).apply(new Trace(Collections.emptyList(), Collections.emptyList(), Collections.emptyMap()));
            if (state.trace != null) {
                if (state.trace.stack != null && !state.trace.stack.isEmpty()) {
                    System.err.println("Trace stack:");
                    for (String s : state.trace.stack) {
                        System.err.println("  > " + s);
                    }
                }
                if (state.trace.messages != null) {
                    System.err.println("Trace messages (" + state.trace.messages.size() + "):");
                    int count = 0;
                    for (String msg : state.trace.messages) {
                        System.err.println("  " + msg);
                        if (++count > 50) {
                            System.err.println("  ... (" + (state.trace.messages.size() - 50) + " more)");
                            break;
                        }
                    }
                }
            }
            // Error already logged via instrumented Unification.joinTypes_cannotUnify
            throw e;
        }
        for (Tuple.Tuple2<String, String> pair : files) {
            String filePath = basePath + File.separator + pair.object1;
            String content = pair.object2;
            if (!content.endsWith("\n")) {
                content = content + "\n";
            }
            try {
                Path p = Paths.get(filePath);
                Files.createDirectories(p.getParent());
                Files.write(p, content.getBytes(StandardCharsets.UTF_8));
            } catch (IOException e) {
                throw new RuntimeException("Failed to write " + filePath, e);
            }
        }
    }

    /**
     * Generate Java source files from modules.
     */
    public static void writeJava(String basePath, List<Module> universe, List<Module> mods) {
        generateSources(
                mod -> defs -> hydra.ext.java.coder.Coder.moduleToJava(mod, defs),
                hydra.ext.java.language.Language.javaLanguage(),
                true, false, true,
                basePath, universe, mods);
    }

    /**
     * Generate Python source files from modules.
     */
    public static void writePython(String basePath, List<Module> universe, List<Module> mods) {
        generateSources(
                mod -> defs -> hydra.ext.python.coder.Coder.moduleToPython(mod, defs),
                hydra.ext.python.language.Language.pythonLanguage(),
                true, true, false,
                basePath, universe, mods);
    }

    /**
     * Generate Haskell source files from modules.
     */
    public static void writeHaskell(String basePath, List<Module> universe, List<Module> mods) {
        generateSources(
                mod -> defs -> hydra.ext.haskell.coder.Coder.moduleToHaskell(mod, defs),
                hydra.ext.haskell.language.Language.haskellLanguage(),
                false, false, false,
                basePath, universe, mods);
    }

    /**
     * Convert a namespace to a file path.
     */
    public static String namespaceToPath(Namespace ns) {
        return CodeGeneration.namespaceToPath(ns);
    }

    /**
     * Strip System F type annotations from all term bodies in a module.
     * Uses removeTypesFromTerm which strips TypeApplication, TypeLambda,
     * lambda domain types, and let-binding TypeSchemes from terms.
     * Module-level TypeSchemes are stripped from term bindings (to avoid
     * bigfloat/float64 conflicts) but preserved on type-defining bindings
     * (needed by isNativeType for schema graph construction).
     */
    public static Module stripTermTypes(Module m) {
        List<Binding> stripped = new ArrayList<>();
        for (Binding b : m.elements) {
            Term newTerm = Rewriting.removeTypesFromTerm(b.term);
            Maybe<TypeScheme> newType = Annotations.isNativeType(b) ? b.type : Maybe.nothing();
            stripped.add(new Binding(b.name, newTerm, newType));
        }
        return new Module(m.namespace, stripped, m.typeDependencies, m.termDependencies, m.description);
    }

    /**
     * Strip System F type annotations from all modules.
     */
    public static List<Module> stripAllTermTypes(List<Module> modules) {
        List<Module> result = new ArrayList<>();
        for (Module m : modules) {
            result.add(stripTermTypes(m));
        }
        return result;
    }

    /**
     * Filter modules to only kernel modules (exclude hydra.ext.* namespaces).
     */
    public static List<Module> filterKernelModules(List<Module> modules) {
        List<Module> result = new ArrayList<>();
        for (Module m : modules) {
            if (!m.namespace.value.startsWith("hydra.ext.")) {
                result.add(m);
            }
        }
        return result;
    }

    /**
     * Filter modules to only those containing type-defining bindings.
     */
    public static List<Module> filterTypeModules(List<Module> modules) {
        List<Module> result = new ArrayList<>();
        for (Module m : modules) {
            boolean hasTypes = false;
            for (Binding b : m.elements) {
                if (Annotations.isNativeType(b)) {
                    hasTypes = true;
                    break;
                }
            }
            if (hasTypes) {
                result.add(m);
            }
        }
        return result;
    }
}
